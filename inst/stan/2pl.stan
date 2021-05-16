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

  int fixedAMean;
  int fixedBMean;
  int fixedCMean;
  int fixedAbilityMean;

  int restrictAMean;
  int restrictBMean;
  int restrictCMean;
  int restrictAbilityMean;

  //priors
  int dopriors;
  int outlierfix;
  real outlierscale;
  real logASD;
  real BSD;
  real logitCSD;
  vector[Nscales] AbilitySD;
  real logAMeandat;
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

  for(i in start:end){
    if(score[i]==0){
      counter+=1;
      incorrect[counter] = i-start+1;
    }
  }
}
parameters{
  vector[fixedA ? 0 : Nitems] logApars;
  vector[fixedB ? 0 : Nitems] Bpars;
  vector[fixedC ? 0 : Nitems] logitCpars;
  matrix[fixedAbility ? 0: Nsubs , fixedAbility ? 0 : Nscales] Abilitypars;
  vector[fixedAMean ? 0 : 1] logAMeanpar;
  vector[fixedBMean ? 0 : 1] BMeanpar;
  vector[fixedCMean ? 0 : 1] logitCMeanpar;
  vector[fixedAbilityMean ? 0 : Nscales] AbilityMeanpar;
}
transformed parameters{
  vector[end-start+1] p;
  vector[end-start+1] AbilityNobs;
  vector[Nitems] A = fixedA ? Adata : log1p_exp(logApars);
  vector[Nitems] B= fixedB ? Bdata : Bpars;
  vector[Nitems] C= fixedC ? Cdata : exp(-exp(-logitCpars));
  matrix[Nsubs,Nscales] Ability;
  real logAMean = fixedAMean ? logAMeandat : logAMeanpar[1];
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
  if(!fixedA){
    if(dopriors) logApars ~ normal(logAMean,logASD);
    if(restrictAMean) mean(A) ~ normal(log1p_exp(logAMean), AMeanSD);
    if(outlierfix) ((logApars-logAMean)/sd(logApars)) ~ normal(0,outlierscale);
  }
  if(!fixedB){
    if(dopriors) Bpars ~ normal(BMean,BSD);
    if(restrictBMean) mean(Bpars) ~ normal(BMean, BMeanSD);
    if(outlierfix) ((Bpars-BMean)/sd(Bpars)) ~ normal(0,outlierscale);
  }
  if(!fixedC){
    if(dopriors) logitCpars ~ normal(logitCMean,logitCSD);
    if(restrictCMean) mean(logitCpars) ~ normal(logitCMean, logitCMeanSD);
    if(outlierfix) ((logitCpars-logitCMean)/sd(logitCpars)) ~ normal(0,outlierscale);
  }
  if(!fixedAbility) {
    for(i in 1:(Nscales)){
      if(dopriors) Abilitypars[,i] ~ normal(AbilityMean[i],AbilitySD[i]);
      if(restrictAbilityMean) mean(Ability[,i]) ~ normal(AbilityMean[i],AbilityMeanSD[i]);
      if(outlierfix) ((Abilitypars[,i]-AbilityMean[i])/sd(Abilitypars[,i])) ~ normal(0,outlierscale);
    }
  }

}
generated quantities{
  vector[end-start+1] pcorrect;
  for(i in start:end){
    if(score[i]==0) pcorrect[i-start+1] = 1-p[i-start+1]; else pcorrect[i-start+1]=p[i-start+1];
  }
}

