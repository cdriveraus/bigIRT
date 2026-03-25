if(identical(Sys.getenv("NOT_CRAN"), "true")){
  library(bigIRT)
  library(testthat)
  library(data.table)

  build_confirmatory_loading_mask <- function(A_true, scale_names = NULL){
    mask <- matrix(0, nrow = nrow(A_true), ncol = ncol(A_true))
    mask[abs(A_true) > 1e-8] <- NA_real_
    rownames(mask) <- rownames(A_true)
    colnames(mask) <- if(is.null(scale_names)) colnames(A_true) else as.character(scale_names)
    mask
  }

  build_mirt_model <- function(A_true){
    lines <- vapply(seq_len(ncol(A_true)), function(k){
      active <- which(abs(A_true[, k]) > 1e-8)
      paste0("F", k, " = ", paste(active, collapse = ","))
    }, character(1))
    mirt::mirt.model(paste(lines, collapse = "\n"))
  }

  response_surface <- function(state){
    theta <- state$Ability
    bigIRT::IRTcurve(
      A = state$A,
      B = state$B,
      C = state$C,
      D = state$D,
      theta = theta,
      plot = FALSE
    )
  }

  normalised_state <- function(state){
    norm <- normaliseMIRT(
      B = state$B,
      Ability = state$Ability,
      A = state$A,
      AbilityCorr = state$corr
    )
    list(
      A = norm$A,
      B = norm$B,
      C = state$C,
      D = state$D,
      Ability = norm$Ability,
      corr = norm$AbilityCorr
    )
  }

  test_that("3D 4PL response curves are invariant to normaliseMIRT for bigIRT and mirt", {
    skip_if_not_installed("mirt")

    set.seed(2501)
    n_per_factor <- 4L
    n_scales <- 3L
    n_items <- n_per_factor * n_scales
    loadings <- matrix(0, nrow = n_items, ncol = n_scales)
    for(k in seq_len(n_scales)){
      rows <- ((k - 1L) * n_per_factor + 1L):(k * n_per_factor)
      loadings[rows, k] <- pmax(0.5, rnorm(n_per_factor, mean = 1, sd = 0.12))
    }
    rownames(loadings) <- as.character(seq_len(n_items))
    colnames(loadings) <- paste0("S", seq_len(n_scales))
    ability_corr <- matrix(
      c(1, 0.35, 0.15,
        0.35, 1, 0.25,
        0.15, 0.25, 1),
      nrow = n_scales, byrow = TRUE
    )

    sim <- simIRT(
      Nsubs = 1180,
      Nitems = n_items,
      Nscales = n_scales,
      NitemsAnswered = rep(n_per_factor, n_scales),
      mirt = TRUE,
      loadings = loadings,
      BSD = 0.9,
      BMean = 0,
      AMean = 1,
      ASD = 0.1,
      logitCMean = -2,
      logitCSD = 0.2,
      AbilityCorr = ability_corr,
      normalise = FALSE
    )

    loading_mask <- build_confirmatory_loading_mask(sim$A, scale_names = sort(unique(sim$dat$Scale)))

    fit_bigirt <- fitIRT(
      sim$dat,
      pl = 4,
      # cores = 1,
      # iter = 35,
      priors = TRUE,
      ebayes = FALSE,
      marginalApprox = "laplace_direct",
      estimateAbilityCorr = TRUE,
      loadings = loading_mask,
      dropPerfectScores = FALSE,
      normalise = FALSE,
      verbose = 0,
      plot = FALSE
    )

    wide <- dcast(copy(sim$dat), id ~ Item, value.var = "score")
    item_names <- colnames(wide)[-1]
    X <- as.data.frame(wide[, -1, with = FALSE])
    # colnames(X) <- paste0("I", seq_len(ncol(X)))
    mirt_fit <- suppressWarnings(mirt::mirt(
      X,
      model = build_mirt_model(sim$A),
      itemtype = rep("4PL", n_items),
      method = "EM",
      verbose = FALSE,
      technical = list(NCYCLES = 300L)
    ))




    bigirt_state <- extractMIRTpars(fit_bigirt)
    mirt_state <- extractMIRTpars(mirt_fit)

    rownames(bigirt_state$A) <- item_names
    names(bigirt_state$B) <- item_names
    names(bigirt_state$C) <- item_names
    names(bigirt_state$D) <- item_names

    rownames(mirt_state$A) <- item_names
    names(mirt_state$B) <- item_names
    names(mirt_state$C) <- item_names
    names(mirt_state$D) <- item_names

    bigirt_norm <- normalised_state(bigirt_state)
    mirt_norm <- normalised_state(mirt_state)

    p_bigirt_raw <- response_surface(bigirt_state)
    p_bigirt_norm <- response_surface(bigirt_norm)
    p_mirt_raw <- response_surface(mirt_state)
    p_mirt_norm <- response_surface(mirt_norm)

    expect_equal(dim(p_bigirt_raw), c(nrow(bigirt_state$Ability), n_items))
    expect_equal(dim(p_mirt_raw), c(nrow(mirt_state$Ability), n_items))
    expect_equal(p_bigirt_norm, p_bigirt_raw, tolerance = 1e-8, scale = 1)
    expect_equal(p_mirt_norm, p_mirt_raw, tolerance = 1e-8, scale = 1)
  })
}
