if(identical(Sys.getenv("NOT_CRAN"), "true") & .Machine$sizeof.pointer != 4){
  library(bigIRT)
  library(testthat)

  test_that("sampled-ability control clamps to sensible bounds", {
    ctrl <- bigIRT:::bigIRT_sampled_build_control(
      sampledAbilitySigmaScale = 2,
      sampledAbilityStepTol = 1e-4,
      sampledAbilitySpreadTol = 0.01,
      sampledAbilityPatience = 2,
      sampledAbilityControl = list(
        sigma_scale_min = 0.1,
        sigma_scale_max = 0.3,
        step_damping_min = 0.2,
        max_backtracks = 3L
      ),
      noptimgradtol = 1e-3
    )

    expect_equal(ctrl$sigmaScale, 0.3)
    expect_equal(ctrl$sigma_scale_min, 0.1)
    expect_equal(ctrl$sigma_scale_max, 0.3)
    expect_equal(ctrl$max_backtracks, 3L)
    expect_true(ctrl$step_damping_min > 0 && ctrl$step_damping_min <= ctrl$step_damping_init)
  })

  test_that("sigma-point weights sum to one", {
    sp <- bigIRT:::bigIRT_sigma_points(c(0, 0), diag(2), sigmaScale = 0.25)
    expect_equal(sum(sp$weights), 1, tolerance = 1e-10)
  })

  test_that("sampled-ability acceptance rejects worsening proposals and preserves state", {
    objective <- list(
      has_mask = TRUE,
      free_par_index = 1:2,
      target = function(parm){
        out <- -sum((parm - c(1, 1))^2)
        attributes(out) <- list(gradient = -2 * (parm - c(1, 1)))
        out
      }
    )
    control <- bigIRT:::bigIRT_sampled_build_control(
      sampledAbilitySigmaScale = 0.25,
      sampledAbilityControl = list(max_backtracks = 2L, step_damping_init = 1, step_damping_min = 0.25),
      noptimgradtol = 1e-3
    )
    current <- c(1, 1)
    proposed <- c(4, 4)
    acc <- bigIRT:::bigIRT_sampled_accept_proposal(current, proposed, objective, control)

    expect_false(acc$accepted)
    expect_equal(acc$full_par, current)
    expect_true(acc$damping >= control$step_damping_min)
  })

  test_that("sampled-ability acceptance uses damping in (0, 1]", {
    objective <- list(
      has_mask = TRUE,
      free_par_index = 1:2,
      target = function(parm){
        out <- -sum((parm - c(1, 1))^2)
        attributes(out) <- list(gradient = -2 * (parm - c(1, 1)))
        out
      }
    )
    control <- bigIRT:::bigIRT_sampled_build_control(
      sampledAbilitySigmaScale = 0.25,
      sampledAbilityControl = list(max_backtracks = 4L, step_damping_init = 1, step_damping_min = 0.0625),
      noptimgradtol = 1e-3
    )
    current <- c(0, 0)
    proposed <- c(1, 1)
    acc <- bigIRT:::bigIRT_sampled_accept_proposal(current, proposed, objective, control)

    expect_true(acc$accepted)
    expect_true(acc$damping > 0 && acc$damping <= 1)
  })

  test_that("adaptive sigma scale respects bounds for shrink and expansion", {
    control <- bigIRT:::bigIRT_sampled_build_control(
      sampledAbilitySigmaScale = 0.25,
      sampledAbilityControl = list(
        sigma_scale_min = 0.1,
        sigma_scale_max = 0.3,
        sigma_scale_expand = 1.5,
        sigma_scale_shrink = 0.5
      ),
      noptimgradtol = 1e-3
    )

    expect_equal(
      bigIRT:::bigIRT_sampled_update_sigma_scale(control, sigmaScaleUsed = 0.25,
        accepted = TRUE, gradientsImproved = TRUE, spreadStable = TRUE),
      0.3
    )
    expect_equal(
      bigIRT:::bigIRT_sampled_update_sigma_scale(control, sigmaScaleUsed = 0.25,
        accepted = FALSE),
      0.125
    )
    expect_equal(
      bigIRT:::bigIRT_sampled_update_sigma_scale(control, sigmaScaleUsed = 0.1,
        accepted = FALSE),
      0.1
    )
  })

  test_that("accepted-window convergence requires all metrics to stay below tolerance", {
    control <- bigIRT:::bigIRT_sampled_build_control(
      sampledAbilitySigmaScale = 0.25,
      sampledAbilityStepTol = 1e-3,
      sampledAbilitySpreadTol = 0.02,
      sampledAbilityPatience = 3L,
      noptimgradtol = 1e-2
    )
    good <- list(
      accepted = TRUE,
      combinedGradNorm = 5e-3,
      itemStepRms = 5e-4,
      personStepRms = 6e-4,
      meanPosteriorSD_ratio = 1.01
    )
    bad <- utils::modifyList(good, list(meanPosteriorSD_ratio = 1.05))

    window <- list(good, good, good)
    expect_true(bigIRT:::bigIRT_sampled_window_converged(window, control))
    expect_false(bigIRT:::bigIRT_sampled_window_converged(list(good, good, bad), control))
  })

  test_that("covariance jitter escalation is reported for near-singular precision", {
    theta_mean <- matrix(c(0, 0, 0, 0), nrow = 2, byrow = TRUE)
    ids <- c(1L, 1L, 2L, 2L)
    loadings <- matrix(c(1, 0, 1, 0, 1, 0, 1, 0), ncol = 2, byrow = TRUE)
    out <- personCovarianceMatrices_R(
      id = ids,
      theta_mean = theta_mean,
      b = c(0, 0, 0, 0),
      c = c(0, 0, 0, 0),
      d = c(1, 1, 1, 1),
      loadings = loadings,
      prior_precision = array(0, dim = c(2, 2, 2)),
      jitter = 1e-8,
      max_attempts = 4
    )

    expect_true(all(dim(out$covariance) == c(2, 2, 2)))
    expect_true(all(out$chol_jitter_used >= 1e-8))
  })
}
