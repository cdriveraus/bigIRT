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
data{ // Section specifies the user supplied data that is passed to the probability model
int Nobs; //Total number of responses (yes or no answers) observed
int Nitems; //Total number of unique items (questions)
int Nsubs; //Total unique subjects (may be students or assessment occasions)
int Nscales; //Total number of scales (e.g. German reading)
int start; //Row of data to start computation on (for parallelisation)
int end; //Row of data to end computation on (for parallelisation)
int trainingLogical[Nobs]; //Which rows of data to use in target probability

int rowIndexPar;

int id[Nobs]; //Subject identifier for each response
int score[Nobs]; //Binary response array
int item[Nobs]; //Item identifier for each response
int scale[Nobs]; //Scale identifier for each response

int NitemPreds; //Number of covariates used to predict item parameters
int NAitemPreds; //Number of covariates used to predict item parameters
int NBitemPreds; //Number of covariates used to predict item parameters
int NCitemPreds; //Number of covariates used to predict item parameters
int NpersonPreds; //Number of covariates used to predict person parameters
int AitemPreds[NAitemPreds];
int BitemPreds[NBitemPreds];
int CitemPreds[NCitemPreds];

int itemSpecificBetas;
int Dpar;

int doGenQuant;

row_vector[NitemPreds] itemPreds[Nobs]; //Values of item predictors
row_vector[NpersonPreds] personPreds[Nobs]; //Values of person predictors
//row_vector[NstatePreds] statePreds[Nobs]; //Values of state predictors

int NfixedA; //Number of fixed (ie user supplied) 'A' (item discrimination) parameters
int NfixedB;//Number of fixed (ie user supplied) 'B' (item difficulty) parameters
int NfixedC;//Number of fixed (ie user supplied) 'C' (item guessing propensity) parameters
int NfixedAbility;//Number of fixed (ie user supplied) ability parameters

int fixedA[NfixedA]; //Vector indicating which items have a fixed A parameter
int fixedB[NfixedB];//Vector indicating which items have a fixed B parameter
int fixedC[NfixedC];//Vector indicating which items have a fixed C parameter

int fixedAlog[Nitems]; //Vector indicating which items have a fixed A parameter
int fixedBlog[Nitems];//Vector indicating which items have a fixed B parameter
int fixedClog[Nitems];//Vector indicating which items have a fixed C parameter

//As above, but vectors indicating which responses have *free* parameters (used for performance reasons here)
int notfixedA[Nitems-NfixedA];
int notfixedB[Nitems-NfixedB];
int notfixedC[Nitems-NfixedC];

int freeAref[Nitems]; //for each item, if fixed then 0, or is cumsum of free items so we know which free par corresponds
int freeBref[Nitems];
int freeCref[Nitems];

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

real betaScale; //sd of regression weights

matrix[Nscales,Nscales] AbilityCorr;

real invspAMeandat; //mean of the inverse softplus A parameters
real BMeandat; //mean of B parameters
real logitCMeandat; //mean of logit C parameters
vector[Nscales] AbilityMeandat; //mean of ability parameters
}

transformed data{ // Section contains calculations that only dependx on user input data
int Ntrainingset = sum(trainingLogical[start:end]);
int trainingset[Ntrainingset];
int counter=0;
int doApreds = (Nitems-NfixedA) ? (NitemPreds>0) : 0;
int doBpreds = (Nitems-NfixedB) ? (NitemPreds>0) : 0;
int doCpreds = (Nitems-NfixedC) ? (NitemPreds>0) : 0;

for(i in start:end){
  if(trainingLogical[i]==1){
    counter+=1;
    trainingset[counter] = i;
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
real Dbase[Dpar ? 1 : 0];
vector[fixedAbilityMean ? 0 : Nscales] AbilityMeanpar;//means of ability parameters, unless values fixed

//when covariate effects are included
vector[(Nitems-NfixedA) ? size(AitemPreds) : 0] invspAbeta[ itemSpecificBetas ? (Nitems-NfixedA) : 1];//regression weights for covariate effects on inverse softplus A params
vector[(Nitems-NfixedB) ? size(BitemPreds) : 0] Bbeta[ itemSpecificBetas ? (Nitems-NfixedB) : 1];//regression weights for covariate effects on B params
vector[(Nitems-NfixedC) ? size(CitemPreds) : 0] logitCbeta[itemSpecificBetas ? (Nitems-NfixedC) : 1];//regression weights for covariate effects on logit C params
vector[(Nsubs*Nscales-NfixedAbility) ? NpersonPreds : 0] Abilitybeta[Nscales];//reg. weights for covariate effects on ability
//vector[NstatePreds] statebeta[Nscales];//regression weights for state predictor effects on state difficulty / ability.
//corr_matrix[Nscales] AbilityCorr;
// vector[(Nscales * Nscales - Nscales) / 2] rawcor;
vector[rowIndexPar ? 1:0] rowIndex;
}

transformed parameters{ //this section combines any user input fixed values and free parameters

vector[Nobs] p; //probability of observed response for responses in current parallel set
// matrix[Nsubs,Nscales] Ability; //ability matrix (potentially mix of free parameters and fixed values)
//vector[Nobs] AbilityNobs; //relevant ability for each response in current parallel set
real ll;
real D= 1.0;


real invspAMean = fixedAMean ? invspAMeandat : invspAMeanpar[1]; //mean of inverse softplus A params
real BMean = fixedBMean ? BMeandat : BMeanpar[1]; //mean of B params
real logitCMean = fixedCMean ? logitCMeandat : logitCMeanpar[1]; //mean of logit C params
vector[Nscales] AbilityMean = fixedAbilityMean ? AbilityMeandat : AbilityMeanpar; //means of ability parameters

// matrix[Nscales,Nscales] AbilityCorr=tcrossprod(constraincorsqrt(rawcor,Nscales));
matrix[Nscales,Nscales] AbilityCov = quad_form_diag(AbilityCorr,AbilitySD);
matrix[Nscales,Nscales] AbilityChol = cholesky_decompose(AbilityCov+diag_matrix(rep_vector(1e-6,Nscales)));

if(Dpar) D = 1-.1/(.1+exp(Dbase[1]));

//if there are item predictors supplied and the relevant regression weight object is larger than zero,
//add the linear covariate effect to the relevant item parameter object (where that object contains free parameters)

// if(NitemPreds > 0){
//   if(itemSpecificBetas){
//     if((Nitems-NfixedA)) for(i in 1:NitemPreds) A[notfixedA] += (itemPreds[i,notfixedA] .* invspAbeta[i,]);
//     if((Nitems-NfixedB)) for(i in 1:NitemPreds) B[notfixedB] += (itemPreds[i,notfixedB] .* Bbeta[i,]);
//     if((Nitems-NfixedC)) for(i in 1:NitemPreds) C[notfixedC] += (itemPreds[i,notfixedC] .* logitCbeta[i,]);
//   } else{
//     if((Nitems-NfixedA)) for(i in 1:NitemPreds) A[notfixedA] += (itemPreds[i,notfixedA] * invspAbeta[i,1]);
//     if((Nitems-NfixedB)) for(i in 1:NitemPreds) B[notfixedB] += (itemPreds[i,notfixedB] * Bbeta[i,1]);
//     if((Nitems-NfixedC)) for(i in 1:NitemPreds) C[notfixedC] += (itemPreds[i,notfixedC] * logitCbeta[i,1]);
//   }
// }
//
// //transform the A and C objects to the appropriate region (i.e. A is positive, C is between 0 and 1)
// A[notfixedA] = log1p_exp(A[notfixedA]);
// C[notfixedC] = inv_logit(C[notfixedC]);
//
//
// for(i in 1:Nsubs){ //for every subject
//   for(j in 1:Nscales){ //and every scale
//     if(fixedAbilityLogical[i,j]==1){
//       Ability[i,j] = Abilitydata[i,j];
//     } else{ //if ability is user supplied, input it
//       Ability[i,j] = Abilitypars[Abilityparsindex[i,j]]; // or input the free parameter
//       if(NpersonPreds) Ability[i,j] += personPreds[i,] * Abilitybeta[j,]'; //when there are person predictors, apply the effect
//     }
//   }
// }

{ //local block for row index use
  int startx = rowIndexPar ? realToInt(rowIndex[1]) : start;
  int endx = rowIndexPar ? startx : end;

// for(i in startx:endx){ // for every response of the current parallel set (potentially all responses)
//   AbilityNobs[i] = Ability[id[i],scale[i]]; //get the appropriate ability dependxing on subject id and scale id
//   if(NstatePreds) AbilityNobs[i] += statePreds[i,]' * statebeta[scale[i]]; //if state covariate effects, apply these
// }

//probability computation
for(i in startx:endx){
  real sA= fixedAlog[item[i]] ? Adata[item[i]] : invspApars[freeAref[item[i]]];
  real sB=fixedBlog[item[i]] ? Bdata[item[i]] : Bpars[freeBref[item[i]]];
  real sC=fixedClog[item[i]] ? Cdata[item[i]] : logitCpars[freeCref[item[i]]];
  real sAbility= fixedAbilityLogical[id[i],scale[i]] ? Abilitydata[id[i],scale[i]] : Abilitypars[Abilityparsindex[id[i],scale[i]]];

  if(doApreds && !fixedAlog[item[i]]) sA += (itemPreds[i,AitemPreds] * invspAbeta[itemSpecificBetas ? freeAref[item[i]] : 1,]);
  if(doBpreds && !fixedBlog[item[i]]) sB += (itemPreds[i,BitemPreds] * Bbeta[itemSpecificBetas ? freeBref[item[i]] : 1,]);
  if(doCpreds && !fixedClog[item[i]]) sC += (itemPreds[i,CitemPreds] * logitCbeta[itemSpecificBetas ? freeCref[item[i]] : 1,]);
  if(NpersonPreds && !fixedAbilityLogical[id[i],scale[i]]) sAbility += personPreds[i,] * Abilitybeta[scale[i],];
  //if(NstatePreds && !fixedAbilityLogical[id[i],scale[i]]) sAbility += statePreds[i,] * statebeta[scale[i],];

if(!fixedAlog[item[i]]) sA=log1p_exp(sA);
if(!fixedClog[item[i]]) sC=inv_logit(sC);

p[i]= sC + (D-sC) / ( 1.0 + exp(
    (-sA * (
      sAbility- //AbilityNobs[i]-
      sB
      ))));

if(score[i]==0) p[i]= 1.0-p[i];//(1 - score[i]) + (2*score[i]-1) * p[i];

}

} //end local block of row index use

if(!rowIndexPar) ll= sum(log(p[trainingset]+1e-20)); //add log of the likelihood (sum of individual response probabilities) to target
if(rowIndexPar) ll= log(p[realToInt(rowIndex[1])]+1e-20); //add log of the likelihood (sum of individual response probabilities) to target
}

model{ // This section modifies the 'target' (output log probability), via 'target+' or '~' operators

target+=ll;

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
if(dopriors){
  for(i in 1:NitemPreds){
    if(doApreds) invspAbeta[,i] ~ normal(0,betaScale);
    if(doBpreds) Bbeta[,i] ~ normal(0,betaScale);
    if(doCpreds) logitCbeta[,i] ~ normal(0,betaScale);
  }
  for(i in 1:Nscales){
    if(num_elements(Abilitybeta[i,])) Abilitybeta[i,] ~ normal(0,betaScale);
    //if(num_elements(statebeta[i,])) statebeta[i,] ~ normal(0,1);
  }
}
}

generated quantities{ //Section generates additional output that is not relevant for probability model
vector[Nobs] pcorrect; //probability of a correct response for each observation
matrix[Nsubs,Nscales] Ability; //ability matrix (potentially mix of free parameters and fixed values)
vector[(Nitems-NfixedA) ? NitemPreds : 0] Abeta[ itemSpecificBetas ? (Nitems-NfixedA) : 1];//linearised regression weights for covariate effects on A params
vector[(Nitems-NfixedC) ? NitemPreds : 0] Cbeta[ itemSpecificBetas ? (Nitems-NfixedC) : 1];//linearised regression weights for covariate effects on A params
vector[Nitems] A; // item A values
vector[Nitems] B; //item B values
vector[Nitems] C; //item C values

row_vector[NitemPreds] itemPredsMean[Nitems]; //Values of item predictors
row_vector[NpersonPreds] personPredsMean[Nsubs]; //Values of person predictors

if(doGenQuant){ //only compute when single core, ie not performance orientation

//put the user supplied fixed values into the item parameter objects
A[fixedA] = Adata[fixedA];
B[fixedB] = Bdata[fixedB];
C[fixedC] = Cdata[fixedC];

//put the free parameters into the item parameter objects
A[notfixedA] = invspApars;
B[notfixedB] = Bpars;
C[notfixedC] = logitCpars;


  for(i in start:end){
    if(score[i]==0) pcorrect[i] = 1-p[i]; else pcorrect[i]=p[i];
  }

  for(i in 1:Nsubs){ //for every subject
  for(j in 1:Nscales){ //and every scale
    if(fixedAbilityLogical[i,j]==1){
      Ability[i,j] = Abilitydata[i,j];
    } else{ //if ability is user supplied, input it
      Ability[i,j] = Abilitypars[Abilityparsindex[i,j]]; // or input the free parameter
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

    if(fixedBlog[i]==0){ //if free B par and item predictors, compute average item effect
      if(doBpreds)B[i] += itemPredsMean[i,BitemPreds] * Bbeta[itemSpecificBetas ? freeBref[i] : 1,]; //when there are person predictors, apply the effect
    }

    if(fixedClog[i]==0){ //if free A par and item predictors, compute average item effect
      if(doCpreds) C[i] += itemPredsMean[i,CitemPreds] * logitCbeta[itemSpecificBetas ? freeCref[i] : 1,]; //when there are person predictors, apply the effect
      C[i]=inv_logit(C[i]);
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


}


}

