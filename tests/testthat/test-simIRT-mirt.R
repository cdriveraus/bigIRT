if(identical(Sys.getenv("NOT_CRAN"), "true") & .Machine$sizeof.pointer != 4){
  library(bigIRT)
  library(testthat)

  test_that("simIRT mirt mode returns coherent multidimensional structures", {
    set.seed(777)
    sim <- simIRT(
      Nsubs = 40,
      Nitems = 12,
      Nscales = 3,
      NitemsAnswered = c(2, 2, 2),
      mirt = TRUE,
      loadingSparsity = 0.4,
      returnRowLoadings = TRUE
    )

    expect_equal(dim(sim$Ability), c(40, 3))
    expect_equal(dim(sim$A), c(12, 3))
    expect_equal(length(sim$B), 12)
    expect_equal(length(sim$C), 12)
    expect_true("primaryScale" %in% names(sim))
    expect_true(all(sim$primaryScale %in% 1:3))
    expect_true(all(c("A_1", "A_2", "A_3") %in% colnames(sim$dat)))
    expect_true(all(sim$dat$p >= 0 & sim$dat$p <= 1))
  })
}
