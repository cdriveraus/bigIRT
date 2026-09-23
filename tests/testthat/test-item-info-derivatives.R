## The closed-form item information is built from row derivatives dp/d(item
## parameter), and everything downstream -- the Newton polish, the
## `method = "profile"` standard errors, both empirical-Bayes updates -- trusts
## them. So assert the property directly: each block must equal
## sum_r V_r V_r' w_r with V_r taken by finite differences of the row
## probability itself, at the same weights w_r. No fitting noise enters; the
## only tolerance is the difference quotient's.
##
## Two derivatives were wrong before this test existed, and neither shows on a
## 2PL without covariates, which is what the other information tests use:
## * the asymptotes are c = plogis(x)/2 and d = plogis(x)/2 + 1/2, so
##   dc/dx = c(1 - 2c) and dd/dx = (2d - 1)(1 - d); the code used c(1 - c) and
##   d(1 - d), overstating their information by (1-c)^2/(1-2c)^2 and
##   d^2/(2d-1)^2 -- 18 and 34 per cent at the worst item of a small 4PL;
## * discrimination multiplies the row ability, which includes the person
##   covariate term x_row'beta; the code used the person's base ability.

if(identical(Sys.getenv("NOT_CRAN"), "true") & .Machine$sizeof.pointer != 4){

  fd_info_check <- function(fit, h = 1e-6){
    d <- bigIRT:::bigIRT_direct_eval_from_fit(fit, keep_covariance = TRUE)
    th <- d$eval$posterior$theta_mode
    rowp <- function(state){
      re <- bigIRT:::bigIRT_laplace_row_effective(state = state, sdat = d$sdat,
        thetaBase = th, rows = d$context$train_rows, context = d$context$row_context)
      g <- stats::plogis(as.numeric(re$eta_row))
      list(p = as.numeric(re$c_row) + (as.numeric(re$d_row) - as.numeric(re$c_row)) * g, re = re)
    }
    base <- rowp(d$state)
    re <- base$re
    bl <- bigIRT:::bigIRT_item_info_blocks(state = d$state, sdat = d$sdat,
      context = d$context, row_effective = re, posterior = d$eval$posterior, thetaBase = th)
    ## The weights the construction uses: 1/r less the part the person's own
    ## uncertainty absorbs.
    eta <- as.numeric(re$eta_row); cr <- as.numeric(re$c_row); dr <- as.numeric(re$d_row)
    g <- stats::plogis(eta); s <- (dr - cr) * g * (1 - g)
    p <- pmin(pmax(cr + (dr - cr) * g, 1e-12), 1 - 1e-12); r <- pmax(p * (1 - p), 1e-12)
    ids <- d$context$row_context$ids
    lo <- as.numeric(re$loadings)
    aSa <- lo^2 * d$eval$posterior$covariance[1, 1, ids]
    w <- pmax(1 / r - (s^2 * aSa) / r^2, 0)
    slot <- c(B = "Bpars", A = "invspApars", C = "logitCpars", D = "logitDpars")
    items <- as.integer(d$context$B_ref)
    worst <- 0
    for(j in seq_len(bl$ni)){
      rows <- which(items == j)
      V <- sapply(bl$active, function(b){
        st <- d$state; st[[slot[[b]]]][j] <- st[[slot[[b]]]][j] + h
        up <- rowp(st)$p
        st <- d$state; st[[slot[[b]]]][j] <- st[[slot[[b]]]][j] - h
        dn <- rowp(st)$p
        ((up - dn) / (2 * h))[rows]
      })
      V <- matrix(V, ncol = length(bl$active))
      S_fd <- crossprod(V * sqrt(w[rows]))
      S <- matrix(bl$info[, , j], bl$P, bl$P)
      worst <- max(worst, max(abs(S - S_fd)) / max(abs(S_fd)))
    }
    list(worst = worst, active = bl$active)
  }

  test_that("closed-form item information has the right asymptote derivatives (4PL)", {
    set.seed(21)
    s <- simIRT(Nsubs = 600, Nitems = 8, AMean = 1.2, ASD = .3, BSD = 1,
      logitCMean = -2, logitCSD = .5, logitDMean = 2, logitDSD = .5)
    f <- suppressWarnings(fitIRT(s$dat, pl = 4, marginalApprox = "laplace", cores = 1,
      ebayes = FALSE, keepInternals = TRUE, laplaceKeepCovariance = TRUE))
    chk <- fd_info_check(f)
    expect_setequal(chk$active, c("B", "A", "C", "D"))
    expect_lt(chk$worst, 1e-5)
  })

  test_that("closed-form discrimination information uses the row ability with person covariates", {
    set.seed(22)
    X <- matrix(rnorm(600 * 2), ncol = 2)
    s <- simIRT(Nsubs = 600, Nitems = 8, AMean = 1.2, ASD = .3, BSD = 1,
      logitCMean = -20, logitCSD = 0, personPreds = X,
      AbilityPredEffects = matrix(c(.8, -.6), 1))
    f <- suppressWarnings(fitIRT(s$dat, pl = 2, marginalApprox = "laplace", cores = 1,
      ebayes = FALSE, keepInternals = TRUE, laplaceKeepCovariance = TRUE,
      personPreds = c("V1", "V2")))
    chk <- fd_info_check(f)
    expect_true("A" %in% chk$active)
    expect_lt(chk$worst, 1e-5)
  })
}
