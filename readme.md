bigIRT
================
Charles Driver
12/05/2021

## Install

This is an R Markdown document. Markdown is a simple formatting syntax
for authoring HTML, PDF, and MS Word documents. For more details on
using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that
includes both content as well as the output of any embedded R code
chunks within the document. You can embed an R code chunk like this:

``` r
remotes::install_github('cdriveraus/bigIRT', INSTALL_opts = "--no-multiarch", dependencies = c("Depends", "Imports"))
```

## Example

``` r
library(bigIRT)
```

    ## Loading required package: data.table

``` r
#Generate some data (here 2pl model
require(data.table)
dat <- bigIRT:::IRTsim(Nsubs = 1000,Nitems = 100,Nscales = 1,
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
    ## ** CDM 7.5-15 (2020-03-10 14:19:21)      
    ## ** Cognitive Diagnostic Models  **
    ## **********************************

    ## * TAM 3.5-19 (2020-05-05 22:45:39)

``` r
tfit <-tam.mml.2pl(resp = wdat,est.variance = TRUE)


#fit using bigIRT
fit <- fitIRT(dat$dat,cores=2,score = 'score',id = 'id',scale = 'Scale',item = 'Item',pl=2,verbose=0,plot=0)
```

    ## Narrow priors step...

    ## Free estimation step...

    ## Empirical Bayes step...

``` r
#some summary stuff:
plot(dat$Ability,(fit$pars$Ability-dat$Ability)^2) #ability error given ability
sqrt(mean((fit$pars$Ability-dat$Ability)^2)) #rms error stat

#correlations of estimated vs true
cor(data.frame(True=dat$Ability,Est=fit$pars$Ability))
cor(data.frame(True=dat$A,Est=fit$pars$A))
cor(data.frame(True=dat$B,Est=fit$pars$B))
```

![](readme_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->
