## compareIRTmodels() scores recovery against the simulation's own parameters.
## A "fit" that holds exactly those parameters must therefore reproduce every
## item curve with zero error, and every parameter must equal its reference.
##
## That fails if any generating parameter is lost on the way in. The upper
## asymptote was: until October 2026 extractMIRTpars() replaced it with 1 for
## every simulated item, so 4PL curve recovery -- the primary accuracy measure
## of the paper's simulations -- was scored against curves the data did not
## come from, for every engine alike. Nothing failed and nothing looked wrong.

## itemPars is in fitIRT's convention (intercept B). Its A column carries each
## item's primary loading only, so the full loading matrix comes from sim$A
## when there is more than one dimension (the mirt = TRUE path, which needs
## two or more).
truth_as_fit <- function(sim){
  ip <- sim$itemPars
  A <- if(ncol(as.matrix(sim$Ability)) > 1L) as.matrix(sim$A) else matrix(ip$A, ncol = 1L)
  list(pars = list(A = A, B = ip$B, C = ip$C, D = ip$D,
                   Ability = as.matrix(sim$Ability)))
}

for(mirt_path in c(FALSE, TRUE)){
  test_that(sprintf("a fit holding the generating 4PL parameters recovers them exactly (mirt = %s)", mirt_path), {
    set.seed(11)
    K <- if(mirt_path) 2L else 1L
    sim <- simIRT(Nsubs = 300L, Nitems = 16L, Nscales = K, NitemsAnswered = rep(8L, K),
      AMean = 1, ASD = .3, BMean = 0, BSD = 1,
      logitCMean = -1.4, logitCSD = .25, logitDMean = 2.4, logitDSD = .25, mirt = mirt_path)
    ## The upper asymptotes really are below one, so this can tell D from 1.
    expect_true(all(sim$itemPars$D < .99))

    z <- compareIRTmodels(list(truth = sim, estimated = truth_as_fit(sim)))
    raw <- z$itempars[form == "raw" & source == "estimated"]
    expect_lt(max(abs(raw$itemCurveRMSE)), 1e-10)
    for(p in c("A", "B", "C", "D"))
      expect_lt(max(abs(raw[parameter == p, value - referenceVal])), 1e-10, label = p)
  })
}
