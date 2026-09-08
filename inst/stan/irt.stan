// Item response theory based probability model
// Square brackets indicate object dimensions, square brackets after name indicates array of specified object.
// Objects are all declared at the top of the relevant sections and potentially modified below.
functions{
  array[] int which(array[] int a, int condition){
    int Nmatches = 0;
    array[0] int zero;
    array[size(a)] int whichout;

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
int NitemScales; //Total number of item-by-scale loading slots
array[Nobs] int trainingLogical; //Which rows of data to use in target probability

int rowIndexPar; //specifies single row to use for target probability, used for getting stan to compute score contributions by row

array[Nobs] int id; //Subject identifier for each response
array[Nobs] int score; //Binary response array
array[Nobs] int item; //Item identifier for each response
array[Nobs] int scale; //Scale identifier for each response

int NitemPreds; //Number of covariates used to predict item parameters
int NAitemPreds; //Number of covariates used to predict item parameters
int NBitemPreds; //Number of covariates used to predict item parameters
int NCitemPreds; //Number of covariates used to predict item parameters
int NDitemPreds; //Number of covariates used to predict item parameters
int NpersonPreds; //Number of covariates used to predict person parameters
array[NAitemPreds] int AitemPreds;
array[NBitemPreds] int BitemPreds;
array[NCitemPreds] int CitemPreds;
array[NDitemPreds] int DitemPreds;

int itemSpecificBetas;
int doGenQuant;
int doRowEff;

array[Nobs] row_vector[NitemPreds] itemPreds; //Values of item predictors
array[Nobs] row_vector[NpersonPreds] personPreds; //Values of person predictors
//row_vector[NstatePreds] statePreds[Nobs]; //Values of state predictors

int NfixedA; //Number of fixed (ie user supplied) 'A' (item discrimination) parameters
int NfixedB;//Number of fixed (ie user supplied) 'B' (item difficulty) parameters
int NfixedC;//Number of fixed (ie user supplied) 'C' (item guessing propensity) parameters
int NfixedD;//Number of fixed (ie user supplied) 'D' (item guessing propensity) parameters
int NfixedAbility;//Number of fixed (ie user supplied) ability parameters

array[NfixedA] int whichfixedA; //Vector indicating which item-scale slots have fixed loading parameters
array[NfixedB] int whichfixedB;//Vector indicating which items have a fixed B parameter
array[NfixedC] int whichfixedC;//Vector indicating which items have a fixed C parameter
array[NfixedD] int whichfixedD;//Vector indicating which items have a fixed C parameter

array[NitemScales] int fixedAlog; //logical Vector indicating which item-scale slots have fixed loading parameters
array[Nitems] int fixedB;//logical Vector indicating which items have a fixed B parameter
array[Nitems] int fixedClogit;//logical Vector indicating which items have a fixed C parameter
array[Nitems] int fixedDlogit;//logical Vector indicating which items have a fixed D parameter

//As above, but vectors indicating which responses have *free* parameters (used for performance reasons here)
array[NitemScales-NfixedA] int whichnotfixedA;
array[Nitems-NfixedB] int whichnotfixedB;
array[Nitems-NfixedC] int whichnotfixedC;
array[Nitems-NfixedD] int whichnotfixedD;

array[NitemScales] int freeAref; //for each item-scale slot, if fixed then 0, or index of free loading parameter
array[Nitems] int freeBref;
array[Nitems] int freeCref;
array[Nitems] int freeDref;

array[Nsubs,Nscales] int fixedAbilityLogical;//Logical array indicating whether subjects have a fixed or free ability parameter
array[Nsubs,Nscales] int Abilityparsindex;//Denotes which free ability param corresponds slots of the Nsubs * Nscales ability array
array[Nsubs*Nscales-NfixedAbility] int Abilityparsscaleindex; //Denotes which scale each free ability parameter corresponds to

vector[NitemScales] Adata; //user specified (fixed) A values for each item-scale loading slot
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
real BSDx;// standard deviation of the B parameters.
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
array[Ntrainingset] int trainingset;
int counter=0;
int doApreds = (NitemScales-NfixedA) ? (NAitemPreds>0) : 0;
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
array[Nscales] vector[(Nsubs*Nscales-NfixedAbility) ? NpersonPreds : 0] Abilitybeta;//reg. weights for covariate effects on ability

vector[Nitems-NfixedB] Bpars;//free B parameters
vector[fixedBMean ? 0 : 1] BMeanpar;//mean of B parameters, unless value fixed
array[ itemSpecificBetas ? (Nitems-NfixedB) : 1] vector[(Nitems-NfixedB) ? size(BitemPreds) : 0] Bbeta;//regression weights for covariate effects on B params

vector[NitemScales-NfixedA] invspApars;// inverse softplus of free item-scale loading parameters
vector[fixedAMean ? 0 : 1] invspAMeanpar; // mean of inverse softplus of A parameters, unless value fixed
array[ itemSpecificBetas ? (NitemScales-NfixedA) : 1] vector[(NitemScales-NfixedA) ? size(AitemPreds) : 0] invspAbeta;//regression weights for covariate effects on inverse softplus A params

vector[Nitems-NfixedC] logitCpars;//logit of free C parameters
vector[fixedCMean ? 0 : 1] logitCMeanpar;//mean of logit C parameters, unless value fixed
array[itemSpecificBetas ? (Nitems-NfixedC) : 1] vector[(Nitems-NfixedC) ? size(CitemPreds) : 0] logitCbeta;//regression weights for covariate effects on logit C params

vector[Nitems-NfixedD] logitDpars;//logit of free C parameters
vector[fixedDMean ? 0 : 1] logitDMeanpar;//mean of logit D parameters, unless value fixed
array[itemSpecificBetas ? (Nitems-NfixedD) : 1] vector[(Nitems-NfixedD) ? size(CitemPreds) : 0] logitDbeta;//regression weights for covariate effects on logit D params

}

transformed parameters{ //this section combines any user input fixed values and free parameters

vector[Nobs] p=rep_vector(0,Nobs); //probability of observed response for responses in current parallel set
matrix[Nsubs,Nscales] sAbilitySD=rep_matrix(0,Nsubs,Nscales); //abilitySD matrix (computed from curvature when requested)
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
vector[Nobs] sB;
vector[Nobs] sC;
vector[Nobs] sD;
vector[Nobs] sAactive;
vector[Nobs] sAbilityActive;
vector[Nobs] e1;
vector[Nobs] e3;
vector[Nobs] e4;
vector[Nobs] e6;
vector[Nobs] e7;
vector[Nobs] e9;
vector[Nobs] e11;
array[Nobs] int scoreCoef;

//probability computation
for(i in 1:Nobs){
  vector[Nscales] sArow;
  vector[Nscales] sAbilityRow;
  real eta;
  for(si in 1:Nscales){
    int aidx = (item[i]-1) * Nscales + si;
    sArow[si]= fixedAlog[aidx] ? Adata[aidx] : invspApars[freeAref[aidx]];
    sAbilityRow[si]= fixedAbilityLogical[id[i],si] ? Abilitydata[id[i],si] : Abilitypars[Abilityparsindex[id[i],si]];
    if(doApreds && !fixedAlog[aidx]) sArow[si] += (itemPreds[i,AitemPreds] * invspAbeta[itemSpecificBetas ? freeAref[aidx] : 1,]);
    if(NpersonPreds && !fixedAbilityLogical[id[i],si]) sAbilityRow[si] += personPreds[i,] * Abilitybeta[si,];
    if(!fixedAlog[aidx]) sArow[si]=log1p_exp(sArow[si]);
  }

  sB[i]=fixedB[item[i]] ? Bdata[item[i]] : Bpars[freeBref[item[i]]];// + BMean;
  sC[i]=fixedClogit[item[i]] ? Cdata[item[i]] : logitCpars[freeCref[item[i]]];// +logitCMean;
  sD[i]=fixedDlogit[item[i]] ? Ddata[item[i]] : logitDpars[freeDref[item[i]]];// + logitDMean;
  if(doBpreds && !fixedB[item[i]]) sB[i] += (itemPreds[i,BitemPreds] * Bbeta[itemSpecificBetas ? freeBref[item[i]] : 1,]);
  if(doCpreds && !fixedClogit[item[i]]) sC[i] += (itemPreds[i,CitemPreds] * logitCbeta[itemSpecificBetas ? freeCref[item[i]] : 1,]);
  if(doDpreds && !fixedDlogit[item[i]]) sD[i] += (itemPreds[i,DitemPreds] * logitDbeta[itemSpecificBetas ? freeDref[item[i]] : 1,]);

  if(!fixedClogit[item[i]]) sC[i]=inv_logit(sC[i])*.5;
  if(!fixedDlogit[item[i]]) sD[i]=inv_logit(sD[i])*.5+.5;

   sAactive[i] = sArow[scale[i]];
   sAbilityActive[i] = sAbilityRow[scale[i]];
   eta = dot_product(sArow,sAbilityRow) - sB[i];
   e1[i] = eta;
   e4[i] = sD[i] - sC[i];
   e6[i] = e4[i] * inv_logit(e1[i]) + sC[i];
   scoreCoef[i] =  (score[i] *2 -1);

  p[i]= (1-score[i])+ scoreCoef[i] * e6[i] ;

  if(doGenQuant){ //if requested, compute per-subject/scale curvature for sAbilitySD
    e3[i] = exp(-e1[i]);
    e7[i] = 1 + e3[i];
    e9[i] = e6[i] * scoreCoef[i] + 1 - score[i];
    e11[i] = e9[i] * e7[i]^2;
    sAbilitySD[id[i],scale[i]] += -(sAactive[i]^2 * ((scoreCoef[i] * e4[i] - 2 * (e9[i] * e7[i])) * e3[i]/e11[i]^2 + inv(e11[i])) * scoreCoef[i] * e4[i] * e3[i]); //incremental addition to 2nd deriv
  }
} //end loop over rows

if(doGenQuant){ //compute subject sd's from accumulated curvature
  for(rowi in 1:Nsubs){
    for(coli in 1:Nscales){
      if(!fixedAbilityLogical[rowi,coli]) sAbilitySD[rowi,coli] = sqrt(inv(abs(sAbilitySD[rowi,coli])));
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
  if(NfixedA < NitemScales)  invspApars ~ normal(invspAMean,invspASD);
  if(NfixedB < Nitems) Bpars ~ normal(BMean,BSDx);
  if(NfixedC < Nitems)logitCpars ~ normal(logitCMean,logitCSD);
  if(NfixedD < Nitems) logitDpars ~ normal(logitDMean,logitDSD);

  if(Nscales==1) Abilitypars ~ normal(AbilityMean[1],AbilitySD[1]); //AbilityMean[1]
  if(Nscales > 1){
    for(i in 1:Nsubs) {
      array[Nscales - sum(fixedAbilityLogical[i,])] int selector = which(fixedAbilityLogical[i,],0); // which scales does this subject have estimated pars for
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
array[ itemSpecificBetas ? (NitemScales-NfixedA) : 1] vector[(NitemScales-NfixedA) ? NAitemPreds : 0] Abeta;//linearised regression weights for covariate effects on A params
array[ itemSpecificBetas ? (Nitems-NfixedC) : 1] vector[(Nitems-NfixedC) ? NCitemPreds : 0] Cbeta;//linearised regression weights for covariate effects on A params
array[ itemSpecificBetas ? (Nitems-NfixedD) : 1] vector[(Nitems-NfixedD) ? NDitemPreds : 0] Dbeta;//linearised regression weights for covariate effects on A params
matrix[Nitems,Nscales] A; // item-by-scale loading values
vector[Nitems] B; //item B values
vector[Nitems] C; //item C values
vector[Nitems] D; //item C values
vector[Nobs] b_row = rep_vector(0,Nobs); //row-level effective difficulty
vector[Nobs] c_row = rep_vector(0,Nobs); //row-level effective lower asymptote
vector[Nobs] d_row = rep_vector(0,Nobs); //row-level effective upper asymptote
vector[Nobs] eta_row = rep_vector(0,Nobs); //row-level linear predictor
matrix[Nobs,Nscales] row_loadings = rep_matrix(0,Nobs,Nscales); //row-level effective loadings
matrix[Nobs,Nscales] row_ability = rep_matrix(0,Nobs,Nscales); //row-level effective abilities

array[Nitems] row_vector[NitemPreds] itemPredsMean; //Values of item predictors
array[Nsubs] row_vector[NpersonPreds] personPredsMean; //Values of person predictors

if(doGenQuant || doRowEff){ //row-effective outputs are available for covariance/sampled-ability plumbing
  for(i in 1:Nobs){
    vector[Nscales] sArow;
    vector[Nscales] sAbilityRow;
    for(si in 1:Nscales){
      int aidx = (item[i]-1) * Nscales + si;
      sArow[si]= fixedAlog[aidx] ? Adata[aidx] : invspApars[freeAref[aidx]];
      sAbilityRow[si]= fixedAbilityLogical[id[i],si] ? Abilitydata[id[i],si] : Abilitypars[Abilityparsindex[id[i],si]];
      if(doApreds && !fixedAlog[aidx]) sArow[si] += (itemPreds[i,AitemPreds] * invspAbeta[itemSpecificBetas ? freeAref[aidx] : 1,]);
      if(NpersonPreds && !fixedAbilityLogical[id[i],si]) sAbilityRow[si] += personPreds[i,] * Abilitybeta[si,];
      if(!fixedAlog[aidx]) sArow[si]=log1p_exp(sArow[si]);
    }

    b_row[i]=fixedB[item[i]] ? Bdata[item[i]] : Bpars[freeBref[item[i]]];
    c_row[i]=fixedClogit[item[i]] ? Cdata[item[i]] : logitCpars[freeCref[item[i]]];
    d_row[i]=fixedDlogit[item[i]] ? Ddata[item[i]] : logitDpars[freeDref[item[i]]];
    if(doBpreds && !fixedB[item[i]]) b_row[i] += (itemPreds[i,BitemPreds] * Bbeta[itemSpecificBetas ? freeBref[item[i]] : 1,]);
    if(doCpreds && !fixedClogit[item[i]]) c_row[i] += (itemPreds[i,CitemPreds] * logitCbeta[itemSpecificBetas ? freeCref[item[i]] : 1,]);
    if(doDpreds && !fixedDlogit[item[i]]) d_row[i] += (itemPreds[i,DitemPreds] * logitDbeta[itemSpecificBetas ? freeDref[item[i]] : 1,]);
    if(!fixedClogit[item[i]]) c_row[i]=inv_logit(c_row[i])*.5;
    if(!fixedDlogit[item[i]]) d_row[i]=inv_logit(d_row[i])*.5+.5;

    eta_row[i] = dot_product(sArow,sAbilityRow) - b_row[i];
    row_loadings[i,] = to_row_vector(sArow);
    row_ability[i,] = to_row_vector(sAbilityRow);
  }
}

if(doGenQuant){ //only compute when single core, ie not performance orientation

//put the user supplied fixed values into the item parameter objects
B[whichfixedB] = Bdata[whichfixedB];
C[whichfixedC] = Cdata[whichfixedC];
D[whichfixedD] = Ddata[whichfixedD];

//put the free parameters into the item parameter objects
B[whichnotfixedB] = Bpars;// +BMean;
C[whichnotfixedC] = logitCpars;// + logitCMean;
D[whichnotfixedD] = logitDpars;// + logitDMean;
for(ai in 1:NitemScales){
  int itemi = 1 + (ai - 1) / Nscales;
  int scalei = ai - (itemi - 1) * Nscales;
  A[itemi,scalei] = fixedAlog[ai] ? Adata[ai] : invspApars[freeAref[ai]];
}


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
  if(doApreds || doBpreds || doCpreds || doDpreds) { //if any covariates, compute covariate mean
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

  for(si in 1:Nscales){
    int aidx = (i - 1) * Nscales + si;
    if(fixedAlog[aidx]==0){ //if free loading and item predictors, compute average item effect
      if(doApreds) A[i,si] += itemPredsMean[i,AitemPreds] * invspAbeta[itemSpecificBetas ? freeAref[aidx] : 1,];
      A[i,si]=log1p_exp(A[i,si]);
    }
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

