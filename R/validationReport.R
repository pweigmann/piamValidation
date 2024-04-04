#' perform validateScenarios and create an .html report using .Rmd templates
#'
#' @param mif one or multiple path(s) to scenario data in .mif or .csv
#'        format
#' @param cfg name of a config from inst/config
#' @param ref in case of historic comparison, choose path to ref data
#' @param report specify which .Rmd file should be used to create report

validationReport <- function(mif, cfg, report = "default") {

  yamlParams <- list(mif = mif, cfg = cfg)

  if (report == "default") {
    report_name <- "validation"
  } else {
    report_name <- report
  }

  output_path <- paste0(path.package("piamValidation"), "/output")
  if (!dir.exists(output_path)) dir.create(output_path)

  # create default report for given data
  rmarkdown::render(paste0("inst/markdown/", report_name, ".Rmd"),
                    params = yamlParams,
                    output_file = paste0(output_path, "/", report_name, "_", cfg,
                                        format(Sys.time(), "_%Y%m%d-%H%M%S"),
                                        ".html"))

}

