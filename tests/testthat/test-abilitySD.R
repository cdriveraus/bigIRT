if(identical(Sys.getenv("NOT_CRAN"), "true")& .Machine$sizeof.pointer != 4){
  library(bigIRT)
  library(testthat)

  test_that("personPreds2PL", {
    cores=1
    set.seed(1)

    require(data.table)
    Np=100
    Ni=50
    itemsAnswered=25
    age = scale(seq(9,16,length.out=Np),scale = F)
    predBeta=matrix(1)
    AbilitySD = 1


    dat <- bigIRT:::IRTsim(Nsubs = Np,Nitems = Ni,Nscales = 1,
      logitCMean = -20,logitCSD = .0,AMean = 2,ASD = .25,
      BMean=0,BSD = 5,AbilitySD = AbilitySD,
      personPreds = age, AbilityPredEffects = t(predBeta),
      AbilityMean = 0)

    dat$D <- 1 #delete if sim function updated to output D!

    #sparsity
    dat$dat <- dat$dat[,.SD[sample(.N,size=itemsAnswered, prob = exp((.001+max(abs(B-Ability)))-abs(B-Ability)) )],by=id]
    plot(density(dat$dat$B-dat$dat$Ability))
    dat$dat <- bigIRT:::dropPerfectScores(data.table(dat$dat),tol.=.01)
    dat$Ability=dat$Ability[dat$dat[!duplicated(id),id]]

    fiti <- fitIRT(dat$dat,cores=cores,pl=4,priors=F,
      personPreds = c('V1'),dropPerfectScores = T,
      itemDat = dat$dat[!duplicated(Item),],
      integrateAbility = T,integrateWidth = 0,
      betaScale = 100,tol=1,carefulfit = T,stochastic=T,
      normalise = F,ebayes = F,ebayesmultiplier = 2)

    #ability SD numeric v analytic comparison
    smf <- ctsem:::stan_reinitsf(bigIRT:::stanmodels$irt,data = fiti$dat)
    sg <- function(x,whichAbil){
      pars <- fiti$optim$par
      pars[whichAbil] <- x
      rstan::grad_log_prob(smf,pars)[whichAbil]
    }

    library(numDeriv)

    numSD=sapply(1:fiti$dat$Nsubs,function(x) sqrt(abs(1/jacobian(sg,fiti$optim$par[x],whichAbil=x))))
    out=cbind(fiti$pars$sAbilitySD,numSD)
    plot(out)
    testthat::expect_equivalent(out[,1], out[,2], tol=1e-5)
  })

}
