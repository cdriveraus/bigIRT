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

  int NitemPreds;
  int NpersonPreds;
  int NstatePreds;

  vector[Nitems] itemPreds[NitemPreds];
  vector[NpersonPreds] personPreds[Nsubs];

  vector[NstatePreds] statePreds[Nobs];

  int NfixedA;
  int NfixedB;
  int NfixedC;
  int NfixedAbility;

  int fixedA[NfixedA];
  int fixedB[NfixedB];
  int fixedC[NfixedC];
  int fixedAbilityLogical[Nsubs,Nscales];
  int Abilityparsindex[Nsubs,Nscales];
  int Abilityparsscaleindex[Nsubs*Nscales-NfixedAbility];

  int notfixedA[Nitems-NfixedA];
  int notfixedB[Nitems-NfixedB];
  int notfixedC[Nitems-NfixedC];

  vector[Nitems] Adata;
  vector[Nitems] Bdata;
  vector[Nitems] Cdata;
  matrix[Nsubs, Nscales] Abilitydata;

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
  vector[Nitems-NfixedA] logApars;
  vector[Nitems-NfixedB] Bpars;
  vector[Nitems-NfixedC] logitCpars;
  vector[Nsubs*Nscales-NfixedAbility] Abilitypars;
  vector[fixedAMean ? 0 : 1] logAMeanpar;
  vector[fixedBMean ? 0 : 1] BMeanpar;
  vector[fixedCMean ? 0 : 1] logitCMeanpar;
  vector[fixedAbilityMean ? 0 : Nscales] AbilityMeanpar;
  vector[(Nitems-NfixedA) ? NitemPreds : 0] logAbeta;
  vector[(Nitems-NfixedB) ? NitemPreds : 0] Bbeta;
  vector[(Nitems-NfixedC) ? NitemPreds : 0] logitCbeta;
  vector[(Nsubs*Nscales-NfixedAbility) ? NpersonPreds : 0] Abilitybeta[Nscales];
  vector[NstatePreds] statebeta[Nscales];
}
transformed parameters{
  vector[end-start+1] p;
  matrix[Nsubs,Nscales] Ability;
  vector[end-start+1] AbilityNobs;
  vector[Nitems] A;
  vector[Nitems] B;
  vector[Nitems] C;
  real logAMean = fixedAMean ? logAMeandat : logAMeanpar[1];
  real BMean = fixedBMean ? BMeandat : BMeanpar[1];
  real logitCMean = fixedCMean ? logitCMeandat : logitCMeanpar[1];
  vector[Nscales] AbilityMean = fixedAbilityMean ? AbilityMeandat : AbilityMeanpar;

  A[fixedA] = Adata[fixedA];
  B[fixedB] = Bdata[fixedB];
  C[fixedC] = Cdata[fixedC];

  A[notfixedA] = logApars;
  B[notfixedB] = Bpars;
  C[notfixedC] = logitCpars;

  if(NitemPreds > 0){
    if(num_elements(logAbeta)) for(i in 1:NitemPreds) A[notfixedA] += (itemPreds[i,notfixedA]' * logAbeta[i])';
    if(num_elements(Bbeta)) for(i in 1:NitemPreds) B[notfixedB] += (itemPreds[i,notfixedB]' * Bbeta[i])';
    if(num_elements(logitCbeta)) for(i in 1:NitemPreds) C[notfixedC] += (itemPreds[i,notfixedC]' * logitCbeta[i])';
  }

  A[notfixedA] = log1p_exp(A[notfixedA]);
  C[notfixedC] = inv_logit(C[notfixedC]);

  for(i in 1:Nsubs){
    for(j in 1:Nscales){
      if(fixedAbilityLogical[i,j]==1) Ability[i,j] = Abilitydata[i,j]; else{
        Ability[i,j] = Abilitypars[Abilityparsindex[i,j]];
        if(NpersonPreds) Ability[i,j] += personPreds[i,]' * Abilitybeta[j,];
      }
    }
  }


  for(i in start:end){
    // AbilityNobs[i-start+1] = fixedAbilityLogical[id[i], scale[i]] ? Abilitydata[id[i],scale[i]] : Abilitypars[Abilityparsindex[id[i],scale[i]]];
    AbilityNobs[i-start+1] = Ability[id[i],scale[i]];
    if(NstatePreds) AbilityNobs[i-start+1] += statePreds[i,]' * statebeta[scale[i]];
  }

  p= C[item[start:end]] + (1.0-C[item[start:end]]) ./ ( 1.0 + exp(
    (-A[item[start:end]] .* (
      AbilityNobs-
      B[item[start:end]]
      ))));

      p[incorrect] = 1-p[incorrect];

}
model{
  target+= sum(log(p+1e-6));
  if(NfixedA < Nitems){
    if(dopriors) logApars ~ normal(logAMean,logASD);
    //if(restrictAMean) mean(A) ~ normal(log1p_exp(logAMean), AMeanSD);
  }
  if(NfixedB < Nitems){
    if(dopriors) Bpars ~ normal(BMean,BSD);
    //if(restrictBMean) mean(Bpars) ~ normal(BMean, BMeanSD);
  }
  if(NfixedC < Nitems){
    if(dopriors) logitCpars ~ normal(logitCMean,logitCSD);
    //if(restrictCMean) mean(logitCpars) ~ normal(logitCMean, logitCMeanSD);
  }
  for(i in 1:(Nscales)){
    if(dopriors) Abilitypars ~ normal(AbilityMean[Abilityparsscaleindex],AbilitySD[Abilityparsscaleindex]);
    //if(restrictAbilityMean) mean(Ability[,i]) ~ normal(AbilityMean[i],AbilityMeanSD[i]);
  }
}
generated quantities{
  vector[end-start+1] pcorrect;
  for(i in start:end){
    if(score[i]==0) pcorrect[i-start+1] = 1-p[i-start+1]; else pcorrect[i-start+1]=p[i-start+1];
  }


}

