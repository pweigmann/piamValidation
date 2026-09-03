test_that("listConfigs and listReports return clean names", {
  configs <- listConfigs()
  expect_true("default" %in% configs)
  expect_true("SCI_REMIND_2026.8.3" %in% configs)
  expect_false(any(grepl("validationConfig_|\\.csv", configs)))

  reports <- listReports()
  expect_true("default" %in% reports)
  expect_true("SCI_REMIND" %in% reports)
  expect_false(any(grepl("validationReport_|\\.Rmd", reports)))
})
