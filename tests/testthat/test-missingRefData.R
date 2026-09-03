test_that("missing reference data triggers a warning", {
  cfg_path <- testthat::test_path("testdata", "validationConfig_testUseCases.csv")
  dat_path <- testthat::test_path("testdata", "data_testUseCases.rds")

  # relative historical comparison with a reference model that is not part
  # of the input data
  cfg <- suppressMessages(getConfig(cfg_path))[1, ]
  cfg$ref_model <- "Nonexistent-Model"
  w <- capture_warnings(try(validateScenarios(dat_path, cfg), silent = TRUE))
  expect_true(any(grepl("Nonexistent-Model", w, fixed = TRUE)))

  # if the missing model is part of reference data shipped with the package,
  # the warning suggests the respective file
  cfg$ref_model <- "range(CEDS-2025,CEDS-2025-COVIDaveraged)"
  w <- capture_warnings(try(validateScenarios(dat_path, cfg), silent = TRUE))
  expect_true(any(grepl("consider adding to 'dataPath'", w, fixed = TRUE)))

  # no warning if all reference models are found in the input data
  cfg <- suppressMessages(getConfig(cfg_path))[1, ]
  expect_no_warning(validateScenarios(dat_path, cfg))
})
