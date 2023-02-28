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

  int realToInt(real a){
    real ab = round(a);
    int b = 0;
    while( ab > b) b+=1;
    return(b);
  }
}
data{ // Section specifies the user supplied data that i passed to the probability model
int Nobs; //Total number of responses (yes or no answers) observed
int Nitems; //Total number of unique items (questions)
int Nsubs; //Total unique subjects (may be students or assessment occasions)
int Nscales; //Total number of scales (e.g. German reading)
int trainingLogical[Nobs]; //Which rows of data to use in target probability

int rowIndexPar; //specifies single row to use for target probability, used for getting stan to compute score contributions by row

int id[Nobs]; //Subject identifier for each response
int score[Nobs]; //Binary response array
int item[Nobs]; //Item identifier for each response
int scale[Nobs]; //Scale identifier for each response

int NitemPreds; //Number of covariates used to predict item parameters
int NAitemPreds; //Number of covariates used to predict item parameters
int NBitemPreds; //Number of covariates used to predict item parameters
int NCitemPreds; //Number of covariates used to predict item parameters
int NDitemPreds; //Number of covariates used to predict item parameters
int NpersonPreds; //Number of covariates used to predict person parameters
int AitemPreds[NAitemPreds];
int BitemPreds[NBitemPreds];
int CitemPreds[NCitemPreds];
int DitemPreds[NDitemPreds];

int itemSpecificBetas;
int doGenQuant;
int integrateAbility;
int integrateAbilityFixedSE;
real integrateWidth;

row_vector[NitemPreds] itemPreds[Nobs]; //Values of item predictors
row_vector[NpersonPreds] personPreds[Nobs]; //Values of person predictors
//row_vector[NstatePreds] statePreds[Nobs]; //Values of state predictors

int NfixedA; //Number of fixed (ie user supplied) 'A' (item discrimination) parameters
int NfixedB;//Number of fixed (ie user supplied) 'B' (item difficulty) parameters
int NfixedC;//Number of fixed (ie user supplied) 'C' (item guessing propensity) parameters
int NfixedD;//Number of fixed (ie user supplied) 'D' (item guessing propensity) parameters
int NfixedAbility;//Number of fixed (ie user supplied) ability parameters

int whichfixedA[NfixedA]; //Vector indicating which items have a fixed A parameter
int whichfixedB[NfixedB];//Vector indicating which items have a fixed B parameter
int whichfixedC[NfixedC];//Vector indicating which items have a fixed C parameter
int whichfixedD[NfixedD];//Vector indicating which items have a fixed C parameter

int fixedAlog[Nitems]; //logical Vector indicating which items have a fixed A parameter
int fixedB[Nitems];//logical Vector indicating which items have a fixed B parameter
int fixedClogit[Nitems];//logical Vector indicating which items have a fixed C parameter
int fixedDlogit[Nitems];//logical Vector indicating which items have a fixed D parameter

//As above, but vectors indicating which responses have *free* parameters (used for performance reasons here)
int whichnotfixedA[Nitems-NfixedA];
int whichnotfixedB[Nitems-NfixedB];
int whichnotfixedC[Nitems-NfixedC];
int whichnotfixedD[Nitems-NfixedD];

int freeAref[Nitems]; //for each item, if fixed then 0, or i cumsum of free items so we know which free par corresponds
int freeBref[Nitems];
int freeCref[Nitems];
int freeDref[Nitems];

int fixedAbilityLogical[Nsubs,Nscales];//Logical array indicating whether subjects have a fixed or free ability parameter
int Abilityparsindex[Nsubs,Nscales];//Denotes which free ability param corresponds slots of the Nsubs * Nscales ability array
int Abilityparsscaleindex[Nsubs*Nscales-NfixedAbility]; //Denotes which scale each free ability parameter corresponds to

vector[Nitems] Adata; //user specified (fixed) A values for each item (values ignored for items with free parameter)
vector[Nitems] Bdata; //user specified (fixed) B values for each item (values ignored for items with free parameter)
vector[Nitems] Cdata; //user specified (fixed) C values for each item (values ignored for items with free parameter)
vector[Nitems] Ddata; //user specified (fixed) D values for each item (values ignored for items with free parameter)
matrix[Nsubs, Nscales] Abilitydata; //user input ability values for each subject * scale (ignored when free parameters exist)

int fixedAMean; //Logical, i the mean of the (inverse softplus) A parameters user specified or estimated?
int fixedBMean;//Logical, i the mean of the B parameters user specified or estimated?
int fixedCMean;//Logical, i the mean of the (logit) C parameters user specified or estimated?
int fixedDMean;//Logical, i the mean of the (logit) D parameters user specified or estimated?
int fixedAbilityMean; //Logical, are the means of the ability parameters user specified or estimated?

//priors for parameters:

int dopriors; //Logical -- use priors? if not, the following are all ignored.
real invspASD; // standard deviation of the inverse softplus A parameters.
real BSD;// standard deviation of the B parameters.
real logitCSD; // standard deviation of the logit C parameters.
real logitDSD; // standard deviation of the logit C parameters.
vector[Nscales] AbilitySD; // standard deviation of the ability parameters.

real betaScale; //sd of regression weights

matrix[Nscales,Nscales] AbilityCorr;

real invspAMeandat; //mean of the inverse softplus A parameters
real BMeandat; //mean of B parameters
real logitCMeandat; //mean of logit C parameters
real logitDMeandat; //mean of logit D parameters
vector[Nscales] AbilityMeandat; //mean of ability parameters
}

transformed data{ // Section contains calculations that only dependx on user input data
int Ntrainingset = sum(trainingLogical[1:Nobs]);
int trainingset[Ntrainingset];
int counter=0;
int doApreds = (Nitems-NfixedA) ? (NAitemPreds>0) : 0;
int doBpreds = (Nitems-NfixedB) ? (NBitemPreds>0) : 0;
int doCpreds = (Nitems-NfixedC) ? (NCitemPreds>0) : 0;
int doDpreds = (Nitems-NfixedD) ? (NDitemPreds>0) : 0;

for(i in 1:Nobs){
  if(trainingLogical[i]==1){
    counter+=1;
    trainingset[counter] = i;
  }
}
}

parameters{ //Section specifying free parameters to be estimated
vector[Nsubs*Nscales-NfixedAbility] Abilitypars; //free ability parameters
vector[fixedAbilityMean ? 0 : Nscales] AbilityMeanpar;//means of ability parameters, unless values fixed
vector[(Nsubs*Nscales-NfixedAbility) ? NpersonPreds : 0] Abilitybeta[Nscales];//reg. weights for covariate effects on ability

vector[Nitems-NfixedB] Bpars;//free B parameters
vector[fixedBMean ? 0 : 1] BMeanpar;//mean of B parameters, unless value fixed
vector[(Nitems-NfixedB) ? size(BitemPreds) : 0] Bbeta[ itemSpecificBetas ? (Nitems-NfixedB) : 1];//regression weights for covariate effects on B params

vector[Nitems-NfixedA] invspApars;// inverse softplus of free A parameters
vector[fixedAMean ? 0 : 1] invspAMeanpar; // mean of inverse softplus of A parameters, unless value fixed
vector[(Nitems-NfixedA) ? size(AitemPreds) : 0] invspAbeta[ itemSpecificBetas ? (Nitems-NfixedA) : 1];//regression weights for covariate effects on inverse softplus A params

vector[Nitems-NfixedC] logitCpars;//logit of free C parameters
vector[fixedCMean ? 0 : 1] logitCMeanpar;//mean of logit C parameters, unless value fixed
vector[(Nitems-NfixedC) ? size(CitemPreds) : 0] logitCbeta[itemSpecificBetas ? (Nitems-NfixedC) : 1];//regression weights for covariate effects on logit C params

vector[Nitems-NfixedD] logitDpars;//logit of free C parameters
vector[fixedDMean ? 0 : 1] logitDMeanpar;//mean of logit D parameters, unless value fixed
vector[(Nitems-NfixedD) ? size(CitemPreds) : 0] logitDbeta[itemSpecificBetas ? (Nitems-NfixedD) : 1];//regression weights for covariate effects on logit D params

}

transformed parameters{ //this section combines any user input fixed values and free parameters

vector[Nobs] p; //probability of observed response for responses in current parallel set
matrix[Nsubs,Nscales] sAbilitySD=rep_matrix(integrateAbilityFixedSE ? .1 : 0,Nsubs,Nscales); //abilitySD matrix (potentially mix of free parameters and fixed values)
//vector[Nobs] AbilityNobs; //relevant ability for each response in current parallel set
real ll;

real invspAMean = fixedAMean ? invspAMeandat : invspAMeanpar[1]; //mean of inverse softplus A params
real BMean = fixedBMean ? BMeandat : BMeanpar[1]; //mean of B params
real logitCMean = fixedCMean ? logitCMeandat : logitCMeanpar[1]; //mean of logit C params
real logitDMean = fixedDMean ? logitDMeandat : logitDMeanpar[1]; //mean of logit C params
vector[Nscales] AbilityMean = fixedAbilityMean ? AbilityMeandat : AbilityMeanpar; //means of ability parameters

// matrix[Nscales,Nscales] AbilityCorr=tcrossprod(constraincorsqrt(rawcor,Nscales));
matrix[Nscales,Nscales] AbilityCov = quad_form_diag(AbilityCorr,AbilitySD);
matrix[Nscales,Nscales] AbilityChol = cholesky_decompose(AbilityCov+diag_matrix(rep_vector(1e-6,Nscales)));


{ //local block for row specific parameter computation
vector[Nobs] sA;
vector[Nobs] sB;
vector[Nobs] sC;
vector[Nobs] sD;
vector[Nobs] sAbility;

//probability computation
for(doIntegrate in 0:integrateAbility){ //would be more efficient to re-write to avoid this loop and the recomputations
for(i in 1:Nobs){
  if(!doIntegrate){ //calculate item and ability parameters with covariate effects, compute max a posteriori prob
  sB[i]=fixedB[item[i]] ? Bdata[item[i]] : Bpars[freeBref[item[i]]];// + BMean;
  sC[i]=fixedClogit[item[i]] ? Cdata[item[i]] : logitCpars[freeCref[item[i]]];// +logitCMean;
  sD[i]=fixedDlogit[item[i]] ? Ddata[item[i]] : logitDpars[freeDref[item[i]]];// + logitDMean;
  sA[i]= fixedAlog[item[i]] ? Adata[item[i]] : invspApars[freeAref[item[i]]];// + invspAMean;
  sAbility[i]= fixedAbilityLogical[id[i],scale[i]] ? Abilitydata[id[i],scale[i]] : Abilitypars[Abilityparsindex[id[i],scale[i]]];// + AbilityMean[Abilityparsscaleindex[Abilityparsindex[id[i],scale[i]]]];

  if(doApreds && !fixedAlog[item[i]]) sA[i] += (itemPreds[i,AitemPreds] * invspAbeta[itemSpecificBetas ? freeAref[item[i]] : 1,]);
  if(NpersonPreds && !fixedAbilityLogical[id[i],scale[i]]) sAbility[i] += personPreds[i,] * Abilitybeta[scale[i],];

  if(doBpreds && !fixedB[item[i]]) sB[i] += (itemPreds[i,BitemPreds] * Bbeta[itemSpecificBetas ? freeBref[item[i]] : 1,]);
  if(doCpreds && !fixedClogit[item[i]]) sC[i] += (itemPreds[i,CitemPreds] * logitCbeta[itemSpecificBetas ? freeCref[item[i]] : 1,]);
  if(doDpreds && !fixedDlogit[item[i]]) sD[i] += (itemPreds[i,DitemPreds] * logitDbeta[itemSpecificBetas ? freeDref[item[i]] : 1,]);

  if(!fixedAlog[item[i]]) sA[i]=log1p_exp(sA[i]);
  if(!fixedClogit[item[i]]) sC[i]=inv_logit(sC[i])*.5;
  if(!fixedDlogit[item[i]]) sD[i]=inv_logit(sD[i])*.5+.5;

  p[i]= (1-score[i])+ (score[i] *2 -1) * (  sC[i] + (sD[i]-sC[i]) /  (1.0 + exp(sA[i] * (sAbility[i]- sB[i]))) );
  } //end !doIntegrate

  if(integrateAbility && !doIntegrate){ //if in the JML phase, prepare ability SDs
  real e2 = exp(-(sA[i] * (sAbility[i] - sB[i])));
  real e3 = 1 + e2;
  real e5 = 2 * score[i] - 1;
  real e6 = sD[i] - sC[i];
  real e9 = (e6/e3 + sC[i]) * e5 + 1 - score[i];
  real e10 = e9 * (e3^2);

  sAbilitySD[Abilityparsindex[id[i],scale[i]]] += -(sA[i]^2 * (inv(e10) - (2 * (e9 * e3) - e5 *
    e6) * e2/(e10^2)) * e5 * e2 * e6); //incremental addition to 2nd deriv
  }


  if(integrateAbility && doIntegrate){ //if finished the JML phase, use ability SD's for approx integral over ability
    for(ii in -1:1){ //but skip 0!
      if(ii != 0){
        p[i] +=  0.25 * ( //if mean, multiply by .5 else .25
        (1-score[i])+ (score[i] *2 -1) * (
          sC[i] + (sD[i]-sC[i]) / ( 1.0 + exp((-sA[i] * (
            (sAbility[i] + ii * sqrt(fabs(.5*inv(sAbilitySD[Abilityparsindex[id[i],scale[i]]][1]))) * integrateWidth) -
            sB[i]))))));
      }
    }
  }

} //end loop over rows

if(integrateAbility && !doIntegrate) p = p * .5; //prepare p values for integration step

} // end integration yes / no loop

if(doGenQuant && integrateAbility){ // then finish computing subject sd's
  for(rowi in 1:Nsubs){
    for(coli in 1:Nscales){
      if(fixedAbilityLogical[rowi,coli]==1) sAbilitySD[rowi,coli] = sqrt(inv(-sAbilitySD[rowi,coli]));
    }
  }
}

} //end local block

if(!rowIndexPar) ll= sum(log(p[trainingset]+1e-20)); //log of the likelihood (sum of individual response probabilities)
if(rowIndexPar) ll= log(p[rowIndexPar]+1e-20); //individual log likelihood for use during cross validation
}

model{ // This section modifies the 'target' (output log probability), via 'target+' or '~' operators

target+=ll;

//following sections add the prior probability model for any free parameters
if(dopriors){
  if(NfixedA < Nitems)  invspApars ~ normal(invspAMean,invspASD);
  if(NfixedB < Nitems) Bpars ~ normal(BMean,BSD);
  if(NfixedC < Nitems)logitCpars ~ normal(logitCMean,logitCSD);
  if(NfixedD < Nitems) logitDpars ~ normal(logitDMean,logitDSD);

  if(Nscales==1) Abilitypars ~ normal(AbilityMean[1],AbilitySD[1]); //AbilityMean[1]
  if(Nscales > 1){
    for(i in 1:Nsubs) {
      int selector[Nscales - sum(fixedAbilityLogical[i,])] = which(fixedAbilityLogical[i,],0); // which scales does this subject have estimated pars for
      if(size(selector)>0) Abilitypars[Abilityparsindex[i,selector] ] ~ multi_normal_cholesky(AbilityMean[selector],AbilityChol[selector,selector] ); //AbilityMean[selector] rep_vector(0,size(selector))
    }
  }

  if(doApreds) for(i in 1:NAitemPreds) invspAbeta[,i] ~ normal(0,betaScale);
  if(doBpreds) for(i in 1:NBitemPreds) Bbeta[,i] ~ normal(0,betaScale);
  if(doCpreds) for(i in 1:NCitemPreds) logitCbeta[,i] ~ normal(0,betaScale);
  if(doDpreds) for(i in 1:NDitemPreds) logitDbeta[,i] ~ normal(0,betaScale);

  for(i in 1:Nscales){
    if(num_elements(Abilitybeta[i,])) Abilitybeta[i,] ~ normal(0,betaScale);
  }
} // end dopriors

}

generated quantities{ //Section generates additional output that i not relevant for probability model
vector[Nobs] pcorrect; //probability of a correct response for each observation
matrix[Nsubs,Nscales] Ability; //ability matrix (potentially mix of free parameters and fixed values)
vector[(Nitems-NfixedA) ? NAitemPreds : 0] Abeta[ itemSpecificBetas ? (Nitems-NfixedA) : 1];//linearised regression weights for covariate effects on A params
vector[(Nitems-NfixedC) ? NCitemPreds : 0] Cbeta[ itemSpecificBetas ? (Nitems-NfixedC) : 1];//linearised regression weights for covariate effects on A params
vector[(Nitems-NfixedD) ? NDitemPreds : 0] Dbeta[ itemSpecificBetas ? (Nitems-NfixedD) : 1];//linearised regression weights for covariate effects on A params
vector[Nitems] A; // item A values
vector[Nitems] B; //item B values
vector[Nitems] C; //item C values
vector[Nitems] D; //item C values

row_vector[NitemPreds] itemPredsMean[Nitems]; //Values of item predictors
row_vector[NpersonPreds] personPredsMean[Nsubs]; //Values of person predictors

if(doGenQuant){ //only compute when single core, ie not performance orientation

//put the user supplied fixed values into the item parameter objects
A[whichfixedA] = Adata[whichfixedA];
B[whichfixedB] = Bdata[whichfixedB];
C[whichfixedC] = Cdata[whichfixedC];
D[whichfixedD] = Ddata[whichfixedD];

//put the free parameters into the item parameter objects
A[whichnotfixedA] = invspApars;// +invspAMean;
B[whichnotfixedB] = Bpars;// +BMean;
C[whichnotfixedC] = logitCpars;// + logitCMean;
D[whichnotfixedD] = logitDpars;// + logitDMean;


for(i in 1:Nobs){
  if(score[i]==0) pcorrect[i] = 1-p[i]; else pcorrect[i]=p[i];
}

// Compute abilities
for(i in 1:Nsubs){ //for every subject
for(j in 1:Nscales){ //and every scale
if(fixedAbilityLogical[i,j]==1){
  Ability[i,j] = Abilitydata[i,j];
} else{ //if ability i user supplied, input it
Ability[i,j] = Abilitypars[Abilityparsindex[i,j]];// + AbilityMean[j]; // or input the free parameter
if(NpersonPreds) {
  int count=0;
  personPredsMean[i]=rep_row_vector(0.0, NpersonPreds); //init to zero, mean of person predictors
  for( ri in 1:Nobs){
    if(id[ri] == i){
      count+=1;
      personPredsMean[i]+=personPreds[ri,];
    }
  }
  personPredsMean[i]= personPredsMean[i]/count;
  Ability[i,j] += personPredsMean[i] * Abilitybeta[j,]; //when there are person predictors, apply the effect
}
}
}
}

{
  row_vector[NitemPreds] predsmean; //create here for access later

  for(i in 1:Nitems){ //for every item
  if(doApreds || doBpreds || doCpreds) { //if any covariates, compute covariate mean
  int count=0;
  itemPredsMean[i]=rep_row_vector(0.0, NitemPreds);
  for( ri in 1:Nobs){
    if(item[ri] == i){
      count+=1;
      itemPredsMean[i]+=itemPreds[ri,];
    }
  }
  itemPredsMean[i]= itemPredsMean[i]/count;
  }

  if(fixedAlog[i]==0){ //if free A par and item predictors, compute average item effect
  if(doApreds) A[i] += itemPredsMean[i,AitemPreds] * invspAbeta[itemSpecificBetas ? freeAref[i] : 1,]; //when there are person predictors, apply the effect
  A[i]=log1p_exp(A[i]);
  }

  if(fixedB[i]==0){ //if free B par and item predictors, compute average item effect
  if(doBpreds)B[i] += itemPredsMean[i,BitemPreds] * Bbeta[itemSpecificBetas ? freeBref[i] : 1,]; //when there are person predictors, apply the effect
  }

  if(fixedClogit[i]==0){ //if free A par and item predictors, compute average item effect
  if(doCpreds) C[i] += itemPredsMean[i,CitemPreds] * logitCbeta[itemSpecificBetas ? freeCref[i] : 1,]; //when there are person predictors, apply the effect
  C[i]=inv_logit(C[i])*.5;
  }

  if(fixedDlogit[i]==0){ //if free A par and item predictors, compute average item effect
  if(doDpreds) D[i] += itemPredsMean[i,DitemPreds] * logitDbeta[itemSpecificBetas ? freeDref[i] : 1,]; //when there are person predictors, apply the effect
  D[i]=inv_logit(D[i])*.5+.5;
  }

  }
} //close local block for predsmean

//linearised regression weights for reporting
if(doApreds){
  if(size(Abeta)==1){
    Abeta[1,] = ((log1p_exp(mean(invspApars)+invspAbeta[1,]*.01))-(log1p_exp(mean(invspApars)-invspAbeta[1,]*.01)))/.02;
  }
  if(size(Abeta)>1){
    for(i in 1:size(Abeta)){
      Abeta[i,] = ((log1p(exp(invspApars[i]+invspAbeta[i,]*.01)))-(log1p(exp(invspApars[i]-invspAbeta[i,]*.01))))/.02;
    }
  }
}

if(doCpreds){
  if(size(Cbeta)==1)   Cbeta[1,] = ((inv_logit(mean(logitCpars)+logitCbeta[1,]*.01))-(inv_logit(mean(logitCpars)-logitCbeta[1,]*.01)))/.02;
  if(size(Cbeta)>1){
    for(i in 1:size(Cbeta)){
      Cbeta[i,] = ((inv_logit(logitCpars[i])+logitCbeta[i,]*.01)-(inv_logit(logitCpars[i])-logitCbeta[i,]*.01))/.02;
    }
  }
}

if(doDpreds){
  if(size(Dbeta)==1)   Dbeta[1,] = ((inv_logit(mean(logitDpars)+logitDbeta[1,]*.01))-(inv_logit(mean(logitDpars)-logitDbeta[1,]*.01)))/.02;
  if(size(Dbeta)>1){
    for(i in 1:size(Dbeta)){
      Dbeta[i,] = ((inv_logit(logitDpars[i])+logitDbeta[i,]*.01)-(inv_logit(logitDpars[i])-logitDbeta[i,]*.01))/.02;
    }
  }
}


}


}

