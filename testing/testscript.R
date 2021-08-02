library(future)
plan(strategy = 'multisession',cores=6)
require(data.table)
require(bigIRT)
scaleNames = c('maths','english')
sc <- t(chol(matrix(c(2,1,1,.8),2,2,dimnames = list(scaleNames,NULL))))

N=100

pcovs=data.table(age=seq(-4,4,length.out=N),
                 ses=rnorm(N,0,2),
                 height=rnorm(N,0,.2))

persons=bigIRT:::simPersons(N=N,
                   mu=c(0,.3),
                   scaleChol=sc,
                   covs=pcovs,
                   beta=matrix(c(.3,.2,0, .4, .3, 0),byrow=TRUE,2,3)
)

cor(persons)
cov2cor(sc)
plot(persons$age,persons$maths)


NperScale=1000
icovs=data.table(grade=seq(-4,4,length.out=NperScale),
                 clarity=rnorm(NperScale,0,2),
                 specificity=rnorm(NperScale,0,.2))

items <- bigIRT:::simItems(NperScale = 1000,scaleNames = colnames(sc),logAmu = inv_log1p_exp(c(3,3)),
                  logASD = c(0,.2),Bmu = c(0,0),BSD = c(3,4),logitCmu = c(-10,-10),logitCSD = c(0,0),
                  covs = icovs,
                  logAbeta = matrix(c(.01,.2,0, 0,-.2,0),byrow=TRUE,2,3),
                  Bbeta = matrix(c(1,0,.1, 0,-.2,0),byrow=TRUE,2,3),
                  logitCbeta = matrix(c(.1,0,.1, 0,-.2,0),byrow=TRUE,2,3)
)

items$C <- 0 #2pl model

items$Item <- paste0('i_',items$Scale,'_',1:nrow(items))

itemmat=as.matrix(items[,c('A','B','C',colnames(icovs)),with=FALSE])
cor(itemmat)
cor(itemmat[1:NperScale,])
cor(itemmat[(NperScale+1):(NperScale*2),])

plot(icovs$clarity[1:NperScale],items$A[1:NperScale])


#simulation loops

Nassesments <- 5
Nperassessment <- 50
truepersons <- copy(persons)
persons[,(scaleNames):= 0]
setwd("/home/driver/bigIRT/testing/")
write.csv(x = persons,file = 'persons.csv')
write.csv(x = items,file = 'items.csv')
a=Sys.time()

rscript <- Sys.which("Rscript")
system2(rscript, paste0(" --vanilla ", getwd(),"/algoserver.R"),wait = FALSE) #start socket server

adatlist <- list()

for(ai in 1:Nassesments){
  person <- sample(1:nrow(persons),1) #random person selected
  scale <- sample(scaleNames,1)

  adatlist[[ai]] <- future({
  adat <- data.table(Item='xxxxxxxxx') #blank placeholder
  for(i in 1:Nperassessment){

    #select an item to present
    source(file = 'algoselect.R')
    # itemcode <-  selectItem(items[Scale %in% scale & !Item %in% adat$item,],
    #                         ability = unlist(persons[person,scale,with=FALSE]),
    #                         targetease = 0.1,samplesize = ifelse(ai > (Nassesments/2), 1000,1))
    # item <- (algoNewItem(person=person, scale=scale, targetease=.1)

    #assessment data
    rowdat <- cbind(data.table(id=person, trueability=unlist(truepersons[person,scale,with=FALSE]), AssessmentItemCount=i,
                               item=itemcode, score=as.integer(NA), ability=0.0,items[Item %in% itemcode,]),pcovs[person,])
    if(i==1) adat <- rowdat else adat <- rbind(adat,rowdat)

    #get response from student
    adat[i,score:= simResponse(items[Item %in% itemcode,],unlist(truepersons[person,scale,with=FALSE])) ]

    #update ability est
    a1=Sys.time()
    source(file = 'algofit.R')
print(Sys.time()-a1)


persons[person,(scale):=fit$pars$Ability]
adat[nrow(adat),ability:=fit$pars$Ability]



    #update ability estimate of student
    # save(adat,file='adat.rda')

    # cmd <- paste0("setwd('",getwd(),"');
    #   load(file='adat.rda');
      # fit <- fitIRT(dat = adat,score='score',id = 'id',item = 'item',scale = 'Scale',pl = 2,cores=1,
      #   # AbilitySD = 5,
      #   priors = TRUE,ebayes = FALSE,
      #   # personPreds = colnames(pcovs), #need fixed betas here or else unidentified
      #   itemDat = adat,normalise = FALSE,dropPerfectScores = FALSE);
    #   1+3
    #     "
    # )
    #
    # cmd <- gsub('\\n','',cmd)
    # cmd <- gsub(' ','',cmd)
    #
    # system.time(
    #   system(command = paste0(Sys.getenv("R_HOME"),'/bin/','Rscript --vanilla ',getwd(),'/algofit.R'),
    #          intern = T,show.output.on.console = TRUE)
    # )

  }
  })
  print(ai)
  print(Sys.time()-a)
  # if(ai==1) record <- data.table(AssessmentID=ai,adat) else record <- rbind(record,data.table(AssessmentID=ai,adat))

}
record <- lapply(adatlist,value)
print(Sys.time()-a)

require(ggplot2)
ggplot(record,aes(y=ability, x=AssessmentItemCount,colour=factor(AssessmentID)))+
  geom_line()+
  theme_bw()+
  geom_hline(data = record,
             aes(yintercept=trueability,colour=factor(AssessmentID)),size=1,alpha=.5,linetype=2)

record[,Random:=ifelse(AssessmentID > (Nassesments/2),TRUE,FALSE)]
record[,RMSE:=sqrt(mean((trueability-ability)^2)),by=interaction(Random,AssessmentItemCount)]

ggplot(record,aes(y=RMSE,colour=Random,x=AssessmentItemCount))+geom_line()+theme_bw()
