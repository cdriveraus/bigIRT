test_that("normalization and comparison helpers are publicly exported", {
  ns_exports <- getNamespaceExports("bigIRT")
  expect_true(all(c("normaliseMIRT", "extractMIRTpars", "IRTcurve") %in% ns_exports))
  expect_true(is.function(getExportedValue("bigIRT", "normaliseMIRT")))
  expect_true(is.function(getExportedValue("bigIRT", "extractMIRTpars")))
  expect_true(is.function(getExportedValue("bigIRT", "IRTcurve")))
})
