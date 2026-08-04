if(identical(Sys.getenv("NOT_CRAN"), "true") && .Machine$sizeof.pointer != 4){
  library(bigIRT)
  library(testthat)
  library(data.table)

  make_training_rows <- function(dat){
    dt <- copy(as.data.table(dat))[, row_id := .I]
    train_rows <- dt[, {
      n_train <- max(1L, min(.N - 1L, ceiling(.N / 2)))
      row_id[seq_len(n_train)]
    }, by = id]$V1
    missing_items <- setdiff(sort(unique(dt$Item)), sort(unique(dt[row_id %in% train_rows, Item])))
    if(length(missing_items)){
      extra_rows <- dt[Item %in% missing_items & !row_id %in% train_rows, .SD[1L], by = Item]$row_id
      train_rows <- sort(unique(c(train_rows, extra_rows)))
    }
    train_rows
  }

  test_that("trainingRows excludes held-out responses from fitIRT estimation", {
    set.seed(2601)
    sim <- simIRT(
      Nsubs = 120,
      Nitems = 24,
      Nscales = 1,
      NitemsAnswered = 12,
      AMean = 1.0,
      ASD = 0.12,
      BMean = 0,
      BSD = 0.8,
      logitCMean = -20,
      logitCSD = 0
    )

    training_rows <- make_training_rows(sim$dat)
    held_out_rows <- setdiff(seq_len(nrow(sim$dat)), training_rows)
    expect_true(length(training_rows) > 0L)
    expect_true(length(held_out_rows) > 0L)
    expect_true(all(sort(unique(sim$dat$id[training_rows])) == sort(unique(sim$dat$id))))
    expect_true(all(sort(unique(sim$dat$Item[training_rows])) == sort(unique(sim$dat$Item))))

    perturbed <- copy(sim$dat)
    perturbed[held_out_rows, score := 1L - score]

    fit_a <- fitIRT(
      sim$dat,
      pl = 2,
      cores = 1,
      priors = TRUE,
      ebayes = FALSE,
      marginalApprox = "laplace_direct",
      trainingRows = training_rows,
      dropPerfectScores = FALSE,
      normalise = FALSE,
      verbose = 0,
      plot = FALSE
    )

    fit_b <- fitIRT(
      perturbed,
      pl = 2,
      cores = 1,
      priors = TRUE,
      ebayes = FALSE,
      marginalApprox = "laplace_direct",
      trainingRows = training_rows,
      dropPerfectScores = FALSE,
      normalise = FALSE,
      verbose = 0,
      plot = FALSE
    )

    expect_length(fit_a$pars$pcorrect, nrow(sim$dat))
    expect_length(fit_b$pars$pcorrect, nrow(sim$dat))
    expect_true(all(is.finite(fit_a$pars$pcorrect)))
    expect_true(all(is.finite(fit_b$pars$pcorrect)))
    expect_equal(fit_a$pars$A, fit_b$pars$A, tolerance = 1e-6, scale = 1)
    expect_equal(fit_a$pars$B, fit_b$pars$B, tolerance = 1e-6, scale = 1)
    expect_equal(fit_a$pars$Ability, fit_b$pars$Ability, tolerance = 1e-6, scale = 1)
    expect_equal(fit_a$pars$pcorrect, fit_b$pars$pcorrect, tolerance = 1e-6, scale = 1)
  })
}
