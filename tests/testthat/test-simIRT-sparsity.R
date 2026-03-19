if(identical(Sys.getenv("NOT_CRAN"), "true") & .Machine$sizeof.pointer != 4){
  library(bigIRT)
  library(testthat)

  test_that("NitemsAnswered scalar gives fixed sparse rows", {
    set.seed(1)
    sim <- simIRT(Nsubs = 20, Nitems = 12, Nscales = 2, NitemsAnswered = 5)

    counts <- aggregate(Item ~ id + Scale, data = sim$dat, FUN = length)

    expect_true(all(counts$Item == 5))
    expect_equal(nrow(sim$dat), 20 * 2 * 5)
  })

  test_that("NitemsAnswered vector applies by scale", {
    set.seed(2)
    sim <- simIRT(
      Nsubs = 15, Nitems = 10, Nscales = 2,
      NitemsAnswered = c(3, 7)
    )

    counts <- aggregate(Item ~ id + Scale, data = sim$dat, FUN = length)

    expect_true(all(counts$Item[counts$Scale == 1] == 3))
    expect_true(all(counts$Item[counts$Scale == 2] == 7))
    expect_equal(nrow(sim$dat), 15 * (3 + 7))
  })

  test_that("NitemsAnswered input is validated", {
    expect_error(
      simIRT(Nsubs = 10, Nitems = 8, Nscales = 2, NitemsAnswered = c(3, 4, 5)),
      "length 1 or Nscales"
    )

    expect_error(
      simIRT(Nsubs = 10, Nitems = 8, Nscales = 1, NitemsAnswered = 0),
      "between 1 and Nitems"
    )

    expect_error(
      simIRT(Nsubs = 10, Nitems = 8, Nscales = 1, NitemsAnswered = 8.5),
      "between 1 and Nitems"
    )
  })
}
