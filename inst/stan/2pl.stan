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
  int fixedAbility;

  vector[fixedA ? Nitems : 0] Adata;
  vector[fixedB ? Nitems : 0] Bdata;
  matrix[fixedAbility ? Nsubs : 0, fixedAbility ? Nscales : 0] Abilitydata;

  //priors
  int dopriors;
  real ASD;
  real BSD;
  real AbilitySD;
  real AMean;
  real BMean;
  real AbilityMean;
}
transformed data{
  int Ncorrect = sum(score[start:end]);
  int correct[Ncorrect];
  int counter=0;
  for(i in start:end){
    if(score[i]==1){
      counter+=1;
      correct[counter] = i-start+1;
    }
  }
}
parameters{
  vector[fixedA ? 0 : Nitems] Apars;
  vector[fixedB ? 0 : Nitems] Bpars;
  matrix[fixedAbility ? 0: Nsubs , fixedAbility ? 0 : Nscales] Abilitypars;
}
transformed parameters{
  vector[end-start+1] p;
  vector[end-start+1] AbilityNobs;
  vector[Nitems] A;
  vector[Nitems] B;
  matrix[Nsubs,Nscales] Ability;

  if(fixedA) A = Adata; else A=Apars+1;
  if(fixedB) B = Bdata; else B=Bpars;
  if(fixedAbility) Ability = Abilitydata; else Ability=Abilitypars;

  for(i in start:end) AbilityNobs[i-start+1] = Ability[id[i],scale[i]];

  p= 1.0 ./ ( 1.0 + exp(
    (A[item[start:end]] .* (
      AbilityNobs+
      B[item[start:end]]
      ))));

  p[correct] = 1-p[correct];

}
model{
  target+= sum(log(p+1e-6));
  if(dopriors){
    A ~ normal(AMean,ASD);
    B ~ normal(BMean,BSD);
    for(i in 1:(Nscales)){
      if(i<=Nscales) Ability[,i] ~ normal(AbilityMean,AbilitySD);
      //mean(Ability[,i]) ~ normal(0,.0001);
    }
  }
}
generated quantities{
  vector[end-start+1] pcorrect;
  for(i in start:end){
    if(score[i]==0) pcorrect[i-start+1] = 1-p[i-start+1]; else pcorrect[i-start+1]=p[i-start+1];
  }
}

