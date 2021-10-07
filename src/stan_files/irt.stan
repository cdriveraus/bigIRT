// Item response theory based probability model
// Square brackets indicate object dimensions, square brackets after name indicates array of specified object.
// Objects are all declared at the top of the relevant sections and potentially modified below.
functions{
 int[] which(int[] a, int condition){
   int Nmatches = 0;
    int zero[0];
   int whichout[size(a)];

   for(i in 1:size(a)){
     if(a[i]==condition){
       Nmatches += 1;
       whichout[Nmatches] = i;
     }
   }
   if(Nmatches > 0) return whichout[1:Nmatches]; else return zero;
 }

  //  matrix constraincorsqrt(vector rawcor, int d){ //converts from unconstrained lower tri vec to cor sqrt
  // int counter = 0;
  // matrix[d,d] o;
  // vector[d] ss = rep_vector(0,d);
  // vector[d] s = rep_vector(0,d);
  // real r;
  // real r3;
  // real r4;
  // real r1;
  // real r2;
  //
  // for(i in 1:d){ //set upper tri to lower
  // for(j in 1:d){
  //   if(j > i){
  //     counter+=1;
  //     o[j,i] =  rawcor[counter];//inv_logit(rawcor[counter])*2-1; //divide by i for approx whole matrix equiv priors
  //   }
  // }
  // }
  //
  // for(i in 1:d){
  //   for(j in 1:d){
  //     if(j > i) {
  //       ss[i] +=square(o[j,i]);
  //       s[i] +=o[j,i];
  //     }
  //     if(j < i){
  //       ss[i] += square(o[i,j]);
  //       s[i] += o[i,j];
  //     }
  //   }
  //   s[i]+=1e-5;
  //   ss[i]+=1e-5;
  // }
  //
  //
  // for(i in 1:d){
  //   o[i,i]=0;
  //   r1=sqrt(ss[i]);
  //   r2=s[i];
  //
  //   r3=(fabs(r2))/(r1)-1;
  //   r4=sqrt(log1p_exp(2*(fabs(r2)-r2-1)-4));
  //   r=(r4*((r3))+1)*r4+1;
  //   r=(sqrt(ss[i]+r));
  //   for(j in 1:d){
  //     if(j > i)  o[i,j]=o[j,i]/r;
  //     if(j < i) o[i,j] = o[i,j] /r;
  //   }
  //   o[i,i]=sqrt(1-sum(square(o[i,]))+1e-5);
  // }
  //
  // return o;
  // }

}
data{ // Section specifies the user supplied data that is passed to the probability model
  int Nobs; //Total number of responses (yes or no answers) observed
  int Nitems; //Total number of unique items (questions)
  int Nsubs; //Total unique subjects (may be students or assessment occasions)
  int Nscales; //Total number of scales (e.g. German reading)
  int start; //Row of data to start computation on (for parallelisation)
  int end; //Row of data to end computation on (for parallelisation)
  int trainingLogical[Nobs]; //Which rows of data to use in target probability

  int id[Nobs]; //Subject identifier for each response
  int score [Nobs]; //Binary response array
  int item[Nobs]; //Item identifier for each response
  int scale[Nobs]; //Scale identifier for each response

  int NitemPreds; //Number of covariates used to predict item parameters
  int NpersonPreds; //Number of covariates used to predict person parameters
  int NstatePreds; //Number of covariates used to predict ability/difficulty (indistinguishable) on a per response basis

  vector[Nitems] itemPreds[NitemPreds]; //Values of item predictors
  vector[NpersonPreds] personPreds[Nsubs]; //Values of person predictors
  vector[NstatePreds] statePreds[Nobs]; //Values of state predictors

  int NfixedA; //Number of fixed (ie user supplied) 'A' (item discrimination) parameters
  int NfixedB;//Number of fixed (ie user supplied) 'B' (item difficulty) parameters
  int NfixedC;//Number of fixed (ie user supplied) 'C' (item guessing propensity) parameters
  int NfixedAbility;//Number of fixed (ie user supplied) ability parameters

  int fixedA[NfixedA]; //Vector indicating which responses (rows of data) have a fixed A parameter
  int fixedB[NfixedB];//Vector indicating which responses (rows of data) have a fixed B parameter
  int fixedC[NfixedC];//Vector indicating which responses (rows of data) have a fixed C parameter

  //As above, but vectors indicating which responses have *free* parameters (used for performance reasons here)
  int notfixedA[Nitems-NfixedA];
  int notfixedB[Nitems-NfixedB];
  int notfixedC[Nitems-NfixedC];

  int fixedAbilityLogical[Nsubs,Nscales];//Logical array indicating whether subjects have a fixed or free ability parameter
  int Abilityparsindex[Nsubs,Nscales];//Denotes which free ability param corresponds slots of the Nsubs * Nscales ability array
  int Abilityparsscaleindex[Nsubs*Nscales-NfixedAbility]; //Denotes which scale each free ability parameter corresponds to

  vector[Nitems] Adata; //user specified (fixed) A values for each item (values ignored for items with free parameter)
  vector[Nitems] Bdata; //user specified (fixed) B values for each item (values ignored for items with free parameter)
  vector[Nitems] Cdata; //user specified (fixed) C values for each item (values ignored for items with free parameter)
  matrix[Nsubs, Nscales] Abilitydata; //user input ability values for each subject * scale (ignored when free parameters exist)

  int fixedAMean; //Logical, is the mean of the (inverse softplus) A parameters user specified or estimated?
  int fixedBMean;//Logical, is the mean of the B parameters user specified or estimated?
  int fixedCMean;//Logical, is the mean of the (logit) C parameters user specified or estimated?
  int fixedAbilityMean; //Logical, are the means of the ability parameters user specified or estimated?

  //priors for parameters:

  int dopriors; //Logical -- use priors? if not, the following are all ignored.
  real invspASD; // standard deviation of the inverse softplus A parameters.
  real BSD;// standard deviation of the B parameters.
  real logitCSD; // standard deviation of the logit C parameters.
  vector[Nscales] AbilitySD; // standard deviation of the ability parameters.

  matrix[Nscales,Nscales] AbilityCorr;

  real invspAMeandat; //mean of the inverse softplus A parameters
  real BMeandat; //mean of B parameters
  real logitCMeandat; //mean of logit C parameters
  vector[Nscales] AbilityMeandat; //mean of ability parameters
}

transformed data{ // Section contains calculations that only depend on user input data
  vector[Nobs] scorevec = to_vector(score); //required in vector form below
  int Ntrainingset = sum(trainingLogical[start:end]);
  int trainingset[Ntrainingset];
  int counter=0;

  for(i in start:end){
    if(trainingLogical[i]==1){
      counter+=1;
      trainingset[counter] = i-start+1;
    }
  }
}

parameters{ //Section specifying free parameters to be estimated
  vector[Nitems-NfixedA] invspApars;// inverse softplus of free A parameters
  vector[Nitems-NfixedB] Bpars;//free B parameters
  vector[Nitems-NfixedC] logitCpars;//logit of free C parameters
  vector[Nsubs*Nscales-NfixedAbility] Abilitypars; //free ability parameters

  //when means of parameters are to be estimated
  vector[fixedAMean ? 0 : 1] invspAMeanpar; // mean of inverse softplus of A parameters, unless value fixed
  vector[fixedBMean ? 0 : 1] BMeanpar;//mean of B parameters, unless value fixed
  vector[fixedCMean ? 0 : 1] logitCMeanpar;//mean of logit C parameters, unless value fixed
  vector[fixedAbilityMean ? 0 : Nscales] AbilityMeanpar;//means of ability parameters, unless values fixed

  //when covariate effects are included
  vector[(Nitems-NfixedA) ? NitemPreds : 0] invspAbeta;//regression weights for covariate effects on inverse softplus A params
  vector[(Nitems-NfixedB) ? NitemPreds : 0] Bbeta;//regression weights for covariate effects on B params
  vector[(Nitems-NfixedC) ? NitemPreds : 0] logitCbeta;//regression weights for covariate effects on logit C params
  vector[(Nsubs*Nscales-NfixedAbility) ? NpersonPreds : 0] Abilitybeta[Nscales];//reg. weights for covariate effects on ability
  vector[NstatePreds] statebeta[Nscales];//regression weights for state predictor effects on state difficulty / ability.
  //corr_matrix[Nscales] AbilityCorr;
  // vector[(Nscales * Nscales - Nscales) / 2] rawcor;
}

transformed parameters{ //this section combines any user input fixed values and free parameters

  vector[end-start+1] p; //probability of observed response for responses in current parallel set
  matrix[Nsubs,Nscales] Ability; //ability matrix (potentially mix of free parameters and fixed values)
  vector[end-start+1] AbilityNobs; //relevant ability for each response in current parallel set
  vector[Nitems] A; // item A values
  vector[Nitems] B; //item B values
  vector[Nitems] C; //item C values

  real invspAMean = fixedAMean ? invspAMeandat : invspAMeanpar[1]; //mean of inverse softplus A params
  real BMean = fixedBMean ? BMeandat : BMeanpar[1]; //mean of B params
  real logitCMean = fixedCMean ? logitCMeandat : logitCMeanpar[1]; //mean of logit C params
  vector[Nscales] AbilityMean = fixedAbilityMean ? AbilityMeandat : AbilityMeanpar; //means of ability parameters

  // matrix[Nscales,Nscales] AbilityCorr=tcrossprod(constraincorsqrt(rawcor,Nscales));
  matrix[Nscales,Nscales] AbilityCov = quad_form_diag(AbilityCorr,AbilitySD);
  matrix[Nscales,Nscales] AbilityChol = cholesky_decompose(AbilityCov);

  //put the user supplied fixed values into the item parameter objects
  A[fixedA] = Adata[fixedA];
  B[fixedB] = Bdata[fixedB];
  C[fixedC] = Cdata[fixedC];

  //put the free parameters into the item parameter objects
  A[notfixedA] = invspApars;
  B[notfixedB] = Bpars;
  C[notfixedC] = logitCpars;

  //if there are item predictors supplied and the relevant regression weight object is larger than zero,
  //add the linear covariate effect to the relevant item parameter object (where that object contains free parameters)
  if(NitemPreds > 0){
    if(num_elements(invspAbeta)) for(i in 1:NitemPreds) A[notfixedA] += (itemPreds[i,notfixedA]' * invspAbeta[i])';
    if(num_elements(Bbeta)) for(i in 1:NitemPreds) B[notfixedB] += (itemPreds[i,notfixedB]' * Bbeta[i])';
    if(num_elements(logitCbeta)) for(i in 1:NitemPreds) C[notfixedC] += (itemPreds[i,notfixedC]' * logitCbeta[i])';
  }

  //transform the A and C objects to the appropriate region (i.e. A is positive, C is between 0 and 1)
  A[notfixedA] = log1p_exp(A[notfixedA]);
  C[notfixedC] = inv_logit(C[notfixedC]);

  for(i in 1:Nsubs){ //for every subject
    for(j in 1:Nscales){ //and every scale
      if(fixedAbilityLogical[i,j]==1) Ability[i,j] = Abilitydata[i,j]; else{ //if ability is user supplied, input it
        Ability[i,j] = Abilitypars[Abilityparsindex[i,j]]; // or input the free parameter
        if(NpersonPreds) Ability[i,j] += personPreds[i,]' * Abilitybeta[j,]; //when there are person predictors, apply the effect
      }
    }
  }


  for(i in start:end){ // for every response of the current parallel set (potentially all responses)
    AbilityNobs[i-start+1] = Ability[id[i],scale[i]]; //get the appropriate ability depending on subject id and scale id
    if(NstatePreds) AbilityNobs[i-start+1] += statePreds[i,]' * statebeta[scale[i]]; //if state covariate effects, apply these
  }

  //probability computation
  p= (1.0 - scorevec[start:end])+ (2.0*scorevec[start:end]-1.0) .* (
    C[item[start:end]] + (1.0-C[item[start:end]]) ./ ( 1.0 + exp(
    (-A[item[start:end]] .* (
      AbilityNobs-
      B[item[start:end]]
      )))));

}

model{ // This section modifies the 'target' (output log probability), via 'target+' or '~' operators

  target+= sum(log(p[trainingset]+1e-6)); //add log of the likelihood (sum of individual response probabilities) to target

  //following sections add the prior probability model for any free parameters
  if(NfixedA < Nitems){
    if(dopriors) invspApars ~ normal(invspAMean,invspASD);
  }
  if(NfixedB < Nitems){
    if(dopriors) Bpars ~ normal(BMean,BSD);
  }
  if(NfixedC < Nitems){
    if(dopriors) logitCpars ~ normal(logitCMean,logitCSD);
  }

  if(Nscales==1){
  for(i in 1:(Nscales)){
    if(dopriors) Abilitypars ~ normal(AbilityMean[Abilityparsscaleindex],AbilitySD[Abilityparsscaleindex]);
  }
  }
  if(Nscales > 1 && dopriors){
    for(i in 1:Nsubs) {
      int selector[Nscales - sum(fixedAbilityLogical[i,])] = which(fixedAbilityLogical[i,],0); // which scales does this subject have estimated pars for
      if(size(selector)>0) Abilitypars[Abilityparsindex[i,selector] ] ~ multi_normal_cholesky(AbilityMean[selector],AbilityChol[selector,selector] );
    }
  }
}

generated quantities{ //Section generates additional output that is not relevant for probability model
  vector[end-start+1] pcorrect; //probability of a correct response for each observation
  for(i in start:end){
    if(score[i]==0) pcorrect[i-start+1] = 1-p[i-start+1]; else pcorrect[i-start+1]=p[i-start+1];
  }


}

