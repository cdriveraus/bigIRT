data{
  int Nobs;
  int Nitems;
  int Nsubs;
  int Nscales;
  int start;
  int end;
  int rowIndex[Nobs];

  int id[Nobs];
  int score [Nobs];
  int item[Nobs];
  int scale[Nobs];

  int fixedA;
  int fixedB;
  int fixedC;
  int fixedAbility;
  int fixedMeans;

  vector[fixedA ? Nitems : 0] Adata;
  vector[fixedB ? Nitems : 0] Bdata;
  vector[fixedC ? Nitems : 0] Cdata;
  matrix[fixedAbility ? Nsubs : 0, fixedAbility ? Nscales : 0] Abilitydata;

  //priors
  int dopriors;
  real ASD;
  real BSD;
  real logitCSD;
  vector[Nscales] AbilitySD;
  real AMeandat;
  real BMeandat;
  real logitCMeandat;
  vector[Nscales] AbilityMeandat;
  real AMeanSD;
  real BMeanSD;
  real logitCMeanSD;
  vector[Nscales] AbilityMeanSD;
}
transformed data{
  int Nincorrect = end-start+1-sum(score[start:end]);
  int incorrect[Nincorrect];
  int counter=0;
  int fixedAMean = fixedA || fixedMeans;
  int fixedBMean = fixedB || fixedMeans;
  int fixedCMean = fixedC || fixedMeans;
  int fixedAbilityMean = fixedAbility || fixedMeans;

  for(i in start:end){
    if(score[i]==0){
      counter+=1;
      incorrect[counter] = i-start+1;
    }
  }
}
parameters{
  vector[fixedA ? 0 : Nitems] Apars;
  vector[fixedB ? 0 : Nitems] Bpars;
  vector[fixedC ? 0 : Nitems] logitCpars;
  matrix[fixedAbility ? 0: Nsubs , fixedAbility ? 0 : Nscales] Abilitypars;
  vector[fixedAMean ? 0 : 1] AMeanpar;
  vector[fixedBMean ? 0 : 1] BMeanpar;
  vector[fixedCMean ? 0 : 1] logitCMeanpar;
  vector[fixedAbilityMean ? 0 : Nscales] AbilityMeanpar;
}
transformed parameters{
  vector[end-start+1] p;
  vector[end-start+1] AbilityNobs;
  vector[Nitems] A = fixedA ? Adata : Apars;
  vector[Nitems] B= fixedB ? Bdata : Bpars;
  vector[Nitems] C= fixedC ? Cdata : inv_logit(logitCpars);
  matrix[Nsubs,Nscales] Ability;
  real AMean = fixedAMean ? AMeandat : AMeanpar[1];
  real BMean = fixedBMean ? BMeandat : BMeanpar[1];
  real logitCMean = fixedCMean ? logitCMeandat : logitCMeanpar[1];
  vector[Nscales] AbilityMean = fixedAbilityMean ? AbilityMeandat : AbilityMeanpar;

  if(fixedAbility) Ability = Abilitydata;
  if(!fixedAbility){
    for(i in 1:Nscales) Ability[,i] = Abilitypars[,i];
  }

  for(i in start:end) AbilityNobs[i-start+1] = Ability[id[i],scale[i]];

  p= C[item[start:end]] + (1.0-C[item[start:end]]) ./ ( 1.0 + exp(
    (-A[item[start:end]] .* (
      AbilityNobs-
      B[item[start:end]]
      ))));

      p[incorrect] = 1-p[incorrect];

}
model{
  target+= sum(log(p+1e-6));
  if(dopriors){
    if(!fixedA){
      Apars ~ normal(AMean,ASD);
      mean(Apars) ~ normal(AMean, AMeanSD);
    }
    if(!fixedB){
      Bpars ~ normal(BMean,BSD);
      mean(Bpars) ~ normal(BMean, BMeanSD);
    }
    if(!fixedC){
      logitCpars ~ normal(logitCMean,logitCSD);
      mean(logitCpars) ~ normal(logitCMean, logitCMeanSD);
    }
    if(!fixedAbility) {
      for(i in 1:(Nscales)){
        Abilitypars[,i] ~ normal(AbilityMean[i],AbilitySD[i]);
        mean(Ability[,i]) ~ normal(AbilityMean[i],AbilityMeanSD[i]);
      }
    }
  }
}
generated quantities{
  vector[end-start+1] pcorrect;
  for(i in start:end){
    if(score[i]==0) pcorrect[i-start+1] = 1-p[i-start+1]; else pcorrect[i-start+1]=p[i-start+1];
  }
}

