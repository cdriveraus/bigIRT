setwd( "C:/Users/Driver/Seafile/mpib/bigIRT/testing")
load(file='adat.rda');
fit <- bigIRT::fitIRT(dat = adat,score='score',id = 'id',item = 'item',scale = 'Scale',pl = 2,cores=1,
  # AbilitySD = 5,
  priors = TRUE,ebayes = FALSE,
  # personPreds = colnames(pcovs), #need fixed betas here or else unidentified
  itemDat = adat,normalise = FALSE,dropPerfectScores = FALSE);
save(fit,file='fit.rda');
