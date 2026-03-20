if(identical(Sys.getenv("NOT_CRAN"), "true") & .Machine$sizeof.pointer != 4){
  library(bigIRT)
  library(testthat)

  test_that("sampled ability appends ability SD columns", {
    set.seed(123)
    require(data.table)

    dat <- simIRT(
      Nsubs = 60, Nitems = 20, Nscales = 1,
      logitCMean = -20, logitCSD = 0,
      AMean = 1, ASD = .1,
      BMean = 0, BSD = 1,
      AbilityMean = 0, AbilitySD = 1
    )

    fitSampled <- fitIRT(
      dat$dat, cores = 1, pl = 2,
      priors = FALSE, ebayes = FALSE,
      sampledAbilityStep = TRUE,
      sampledAbilityOuterIter = 8,
      noptimsteps = 3,
      sampledAbilityDiagnostics = TRUE,
      dropPerfectScores = TRUE
    )

    abilityCol <- grep("^(X1|1)$", colnames(fitSampled$personPars))
    sdCol <- grep("_SD$", colnames(fitSampled$personPars))

    expect_length(abilityCol, 1)
    expect_gte(length(sdCol), 1)
    expect_true(all(is.finite(fitSampled$personPars[[sdCol]])))
    expect_true(min(fitSampled$personPars[[sdCol]]) >= 0)
  })

  test_that("row-effective outputs are available in Stan-style shape", {
    sdat <- list(Nobs = 3L, Nscales = 2L, id = c(1L, 1L, 2L))
    pars <- list(
      b_row = c(0.1, -0.2, 0.0),
      c_row = c(0, 0, 0),
      d_row = c(1, 1, 1),
      eta_row = c(0.3, -0.1, 0.2),
      row_loadings = matrix(c(1, 0, 0.5, 0.5, 0, 1), nrow = 3, byrow = TRUE),
      row_ability = matrix(c(0.2, 0.1, 0.2, 0.1, -0.1, 0.4), nrow = 3, byrow = TRUE),
      Ability = matrix(c(0.2, 0.1, -0.1, 0.4), nrow = 2, byrow = TRUE)
    )

    expect_equal(length(pars$b_row), sdat$Nobs)
    expect_equal(length(pars$c_row), sdat$Nobs)
    expect_equal(length(pars$d_row), sdat$Nobs)
    expect_equal(dim(pars$row_loadings), c(sdat$Nobs, sdat$Nscales))
  })

  test_that("person posterior uses row-effective covariance backend", {
    sdat <- list(
      Nobs = 3L,
      Nsubs = 2L,
      Nscales = 2L,
      id = c(1L, 1L, 2L),
      AbilitySD = c(1, 1),
      AbilityCorr = diag(2)
    )
    fit <- list(pars = list(
      Ability = matrix(c(0.2, 0.1, -0.1, 0.4), nrow = 2, byrow = TRUE),
      b_row = c(0.1, -0.2, 0.0),
      c_row = c(0, 0, 0),
      d_row = c(1, 1, 1),
      eta_row = c(0.3, -0.1, 0.2),
      row_loadings = matrix(c(1, 0, 0.5, 0.5, 0, 1), nrow = 3, byrow = TRUE)
    ))

    mock_cov <- function(id, theta_mean, b, loadings, c = NULL, d = NULL,
      prior_precision = NULL, jitter = 1e-8, max_attempts = 8, return_precision = TRUE){
      Nsubs <- nrow(theta_mean)
      K <- ncol(theta_mean)
      cov <- array(0, dim = c(K, K, Nsubs))
      prec <- array(0, dim = c(K, K, Nsubs))
      pchol <- array(0, dim = c(K, K, Nsubs))
      for(i in seq_len(Nsubs)){
        cov[,,i] <- diag(K)
        prec[,,i] <- diag(K)
        pchol[,,i] <- diag(K)
      }
      list(covariance = cov, precision = prec, precision_chol = pchol, backend = "cpp")
    }

    local_mocked_bindings(personCovarianceMatrices = mock_cov, .package = "bigIRT")
    post <- bigIRT:::bigIRT_person_posterior(fit, sdat, jitter = 1e-6)

    expect_equal(post$backend, "cpp")
    expect_equal(length(post$cov), sdat$Nsubs)
    expect_equal(dim(post$cov[[1]]), c(sdat$Nscales, sdat$Nscales))
    expect_true(!is.null(post$rowEffective))
    expect_false("precision" %in% names(post))
    expect_false("precision_chol" %in% names(post))
  })

  test_that("sampled ability uses real covariance backend end-to-end", {
    set.seed(321)
    require(data.table)

    dat <- simIRT(
      Nsubs = 30, Nitems = 8, Nscales = 2, NitemsAnswered = 8,
      logitCMean = -20, logitCSD = 0,
      AMean = 1, ASD = .1,
      BMean = 0, BSD = 1,
      AbilityMean = 0, AbilitySD = 1
    )

    fit <- fitIRT(
      dat$dat,
      cores = 1,
      pl = 2,
      priors = TRUE,
      ebayes = FALSE,
      sampledAbilityStep = TRUE,
      sampledAbilityOuterIter = 1,
      noptimsteps = 3,
      sampledAbilityDiagnostics = TRUE,
      dropPerfectScores = FALSE,
      normalise = FALSE,
      verbose = 0,
      plot = FALSE
    )

    expect_equal(fit$personPosterior$backend, "cpp_sigma")
    expect_true(is.null(fit$personPosterior$cov))
    expect_true(is.matrix(fit$personPosterior$posteriorSDMat))
    expect_equal(nrow(fit$personPosterior$posteriorSDMat), fit$dat$Nsubs)
    expect_true(all(is.finite(fit$personPosterior$posteriorSDMat)))
    expect_false("precision" %in% names(fit$personPosterior))
    expect_false("precision_chol" %in% names(fit$personPosterior))
    expect_true(is.data.frame(fit$sampledAbilityDiagnostics))
    expect_true(is.data.frame(fit$sampledAbilityTiming))
    expect_true(is.list(fit$sampledAbilityControl))
    expect_true(is.list(fit$sampledAbilityStatus))
    expect_true(all(c(
      "outerIter", "stage", "accepted", "rejected", "reject_reason",
      "sigmaScaleUsed", "personStepDamping", "itemStepDamping",
      "personObjectiveBefore", "personObjectiveAfter",
      "itemObjectiveBefore", "itemObjectiveAfter",
      "meanPosteriorSD_ratio", "maxPosteriorSD_ratio",
      "combinedGradNorm", "person_opt_sec", "posterior_sec",
      "item_opt_sec", "person_target_evals", "item_target_evals", "total_sec"
    ) %in% colnames(fit$sampledAbilityTiming)))
    expect_true(all(c(
      "accepted", "rejected", "reject_reason", "sigmaScaleUsed",
      "personStepDamping", "itemStepDamping",
      "personObjectiveBefore", "personObjectiveAfter",
      "itemObjectiveBefore", "itemObjectiveAfter",
      "meanPosteriorSD_ratio", "maxPosteriorSD_ratio"
    ) %in% colnames(fit$sampledAbilityDiagnostics)))
    expect_true(all(is.finite(fit$sampledAbilityTiming$total_sec[fit$sampledAbilityTiming$stage == "outer"])))
    expect_true(all(fit$sampledAbilityTiming$person_target_evals[fit$sampledAbilityTiming$stage == "outer"] >= 1))
    expect_true(all(fit$sampledAbilityTiming$item_target_evals[fit$sampledAbilityTiming$stage == "outer"] >= 1))
    expect_true(is.logical(fit$sampledAbilityTiming$accepted))
    expect_true(is.logical(fit$sampledAbilityTiming$rejected))
    expect_equal(fit$sampledAbilityStatus$accepted_outer_iters + fit$sampledAbilityStatus$rejected_outer_iters,
      nrow(fit$sampledAbilityTiming[fit$sampledAbilityTiming$stage == "outer", , drop = FALSE]))
  })
}
