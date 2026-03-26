if(identical(Sys.getenv("NOT_CRAN"), "true") & .Machine$sizeof.pointer != 4){
  library(bigIRT)
  library(testthat)
  library(data.table)
  compare_models <- get("compareIRTmodels", envir = asNamespace("bigIRT"))

  build_mirt_model_from_loadings <- function(A_true){
    lines <- vapply(seq_len(ncol(A_true)), function(k){
      active <- which(abs(A_true[, k]) > 1e-8)
      paste0("F", k, " = ", paste(active, collapse = ","))
    }, character(1))
    mirt::mirt.model(paste(lines, collapse = "\n"))
  }

  build_loading_mask <- function(A_true, scale_names = NULL){
    mask <- matrix(0, nrow = nrow(A_true), ncol = ncol(A_true))
    mask[abs(A_true) > 1e-8] <- NA_real_
    rownames(mask) <- rownames(A_true)
    colnames(mask) <- if(is.null(scale_names)) colnames(A_true) else as.character(scale_names)
    mask
  }

  test_that("compareIRTmodels handles 1D 1PL-4PL bigIRT fits", {
    for(pl in 1:4){
      set.seed(4000 + pl)
      sim <- simIRT(
        Nsubs = 80,
        Nitems = 8,
        Nscales = 1,
        NitemsAnswered = 8,
        AMean = 1,
        ASD = 0.08,
        BSD = 0.7,
        BMean = 0,
        logitCMean = if(pl >= 3) -2 else -20,
        logitCSD = if(pl >= 3) 0.1 else 0
      )

      fit <- fitIRT(
        sim$dat,
        pl = pl,
        cores = 1,
        iter = 25,
        priors = TRUE,
        ebayes = FALSE,
        dropPerfectScores = FALSE,
        normalise = FALSE,
        verbose = 0,
        plot = FALSE
      )

      cmp <- compare_models(list(truth = sim, bigirt = fit))

      expect_identical(cmp$reference_model, "truth")
      expect_true(inherits(sim, "bigIRT_simIRT"))
      expect_true(data.table::is.data.table(cmp$itempars))
      expect_true(data.table::is.data.table(cmp$personpars))
      expect_true(data.table::is.data.table(cmp$loading_matrix))
      expect_true(data.table::is.data.table(cmp$ability_corr_matrix))
      expect_true(all(c("source", "value", "referenceVal") %in% colnames(cmp$itempars)))
      expect_true(all(c("truth", "bigirt") %in% cmp$itempars$source))
      expect_true(all(c("raw", "normalized") %in% cmp$itempars$form))
      expect_true(all(c("A", "B", "B_loading_corrected", "C", "D") %in% unique(cmp$itempars$parameter)))
      expect_true(all(is.finite(stats::na.omit(cmp$itempars[parameter == "B_loading_corrected", value]))))
      expect_true(all(is.finite(stats::na.omit(cmp$itempars$value))))
      expect_true(all(is.finite(stats::na.omit(cmp$itempars$referenceVal))))
      expect_true(all(is.finite(stats::na.omit(cmp$personpars$value))))
      expect_true(all(is.finite(stats::na.omit(cmp$personpars$referenceVal))))
      expect_true(all(cmp$itempars[source == "truth", value] == cmp$itempars[source == "truth", referenceVal]))
      expect_true(all(c("estimated", "empirical") %in% cmp$ability_corr_matrix$corr_type))
    }
  })

  test_that("compareIRTmodels handles 3D 4PL truth, bigIRT, and mirt fits", {
    skip_if_not_installed("mirt")

    set.seed(4010)
    n_scales <- 3L
    n_per_factor <- 3L
    n_items <- n_scales * n_per_factor
    loadings <- matrix(0, nrow = n_items, ncol = n_scales)
    for(k in seq_len(n_scales)){
      rows <- ((k - 1L) * n_per_factor + 1L):(k * n_per_factor)
      loadings[rows, k] <- pmax(0.5, rnorm(n_per_factor, 1, 0.1))
    }
    rownames(loadings) <- as.character(seq_len(n_items))
    colnames(loadings) <- paste0("S", seq_len(n_scales))
    ability_corr <- matrix(c(1, 0.3, 0.1, 0.3, 1, 0.2, 0.1, 0.2, 1), 3, 3, byrow = TRUE)

    sim <- simIRT(
      Nsubs = 120,
      Nitems = n_items,
      Nscales = n_scales,
      NitemsAnswered = rep(n_per_factor, n_scales),
      mirt = TRUE,
      loadings = loadings,
      AMean = 1,
      ASD = 0.08,
      BSD = 0.7,
      BMean = 0,
      logitCMean = -2,
      logitCSD = 0.15,
      AbilityCorr = ability_corr
    )

    fit_bigirt <- fitIRT(
      sim$dat,
      pl = 4,
      cores = 1,
      iter = 30,
      priors = TRUE,
      ebayes = FALSE,
      marginalApprox = "laplace_direct",
      estimateAbilityCorr = TRUE,
      loadings = build_loading_mask(sim$A, scale_names = sort(unique(sim$dat$Scale))),
      dropPerfectScores = FALSE,
      normalise = FALSE,
      verbose = 0,
      plot = FALSE
    )

    wide <- dcast(copy(sim$dat), id ~ Item, value.var = "score")
    X <- as.data.frame(wide[, -1, with = FALSE])
    colnames(X) <- paste0("I", seq_len(ncol(X)))
    fit_mirt <- suppressWarnings(mirt::mirt(
      X,
      model = build_mirt_model_from_loadings(sim$A),
      itemtype = rep("4PL", n_items),
      method = "EM",
      verbose = FALSE,
      technical = list(NCYCLES = 250L)
    ))

    cmp <- compare_models(list(truth = sim, bigirt = fit_bigirt, mirt = fit_mirt))

    expect_identical(cmp$reference_model, "truth")
    expect_true(data.table::is.data.table(cmp$itempars))
    expect_true(data.table::is.data.table(cmp$personpars))
    expect_true(data.table::is.data.table(cmp$loading_matrix))
    expect_true(data.table::is.data.table(cmp$ability_corr_matrix))
    expect_true(all(c("truth", "bigirt", "mirt") %in% cmp$itempars$source))
    expect_true(all(c("raw", "normalized") %in% cmp$itempars$form))
    expect_true(all(c("A", "B", "B_loading_corrected", "C", "D") %in% unique(cmp$itempars$parameter)))
    expect_true(all(is.finite(stats::na.omit(cmp$itempars[parameter == "B_loading_corrected", value]))))
    expect_true(all(c("estimated", "empirical") %in% unique(cmp$ability_corr_matrix$corr_type)))
    expect_true(all(c("source", "value", "referenceVal") %in% colnames(cmp$loading_matrix)))
    expect_true(all(c("source", "value", "referenceVal") %in% colnames(cmp$ability_corr_matrix)))
    expect_true(all(cmp$itempars[source == "truth", value] == cmp$itempars[source == "truth", referenceVal]))
    expect_true(all(cmp$personpars[source == "truth", value] == cmp$personpars[source == "truth", referenceVal]))
  })
}
