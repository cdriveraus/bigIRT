bigIRT
================
Charles Driver
12/05/2021

## Install

``` r
remotes::install_github('cdriveraus/bigIRT', INSTALL_opts = "--no-multiarch", dependencies = c("Depends", "Imports"))
```

## Example

``` r
library(bigIRT)
```

    ## Loading required package: data.table

    ## Warning: package 'data.table' was built under R version 4.2.2

``` r
#Generate some data (here 2pl model
require(data.table)
dat <- simIRT(Nsubs = 1000,Nitems = 100,Nscales = 1,
  logitCMean = -10,logitCSD = 0,AMean = 1,ASD = .3,
  BMean=0,BSD = .5,
  AbilityMean = 0,AbilitySD = 1)

#convert to wide for TAM
wdat <- data.frame(dcast(data.table(dat$dat),formula = 'id ~ Item',value.var='score')[,-1])


#fit using TAM
require(TAM)
```

    ## Loading required package: TAM

    ## Loading required package: CDM

    ## Loading required package: mvtnorm

    ## **********************************
    ## ** CDM 8.2-6 (2022-08-25 15:43:23)       
    ## ** Cognitive Diagnostic Models  **
    ## **********************************

    ## * TAM 4.1-4 (2022-08-28 16:03:54)

``` r
tfit <-tam.mml.2pl(resp = wdat,est.variance = TRUE)


#fit using bigIRT
fit <- fitIRT(dat$dat,cores=2,score = 'score',id = 'id',scale = 'Scale',item = 'Item',pl=2,verbose=0,plot=0)
```

    ## Free estimation step...

    ## Empirical Bayes step...

``` r
#some summary stuff:
plot(dat$Ability,(fit$pars$Ability-dat$Ability)^2) #Ability error given Ability
```

![](readme_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

``` r
sqrt(mean((fit$pars$Ability-dat$Ability)^2)) #rms error stat

#correlations of estimated vs true
cor(data.frame(True=dat$Ability,Est=fit$pars$Ability))
cor(data.frame(True=dat$A,Est=fit$pars$A))
cor(data.frame(True=dat$B,Est=fit$pars$B))
```
