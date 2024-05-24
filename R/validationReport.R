#' perform validateScenarios and create an .html report using .Rmd templates
#'
#' @param dataPath one or multiple path(s) to scenario data in .mif or .csv
#'        format
#' @param config name a config from inst/config ("validationConfig_<name>.csv")
#'        or give a full path to a separate configuration file
#' @param report name a .Rmd from inst/markdown ("validationReport_<name>.Rmd")
#'        to be rendered or give a full path to a separate .Rmd file
#'
#' @importFrom piamutils getSystemFile
#'
#' @export

validationReport <- function(dataPath, config, report = "default") {

  # convert relative to absolute paths
  dataPath <- normalizePath(dataPath)

  # user has the option to enter name of files that are shipped with package
  # or provide full paths to manually created files for config and report

  if (file.exists(normalizePath(config, mustWork = F))) {
    # full path to config given
    config <- normalizePath(config)
    config_name <- "Custom"
  } else {
    # name of config file in inst/config given
    config_name <- config
  }

  if (file.exists(normalizePath(report, mustWork = F))) {
    # full path to report given
    report_path <- normalizePath(report)
    report_name <- "Custom"
  } else {
    # name of report file in inst/markdown given
    report_path <- piamutils::getSystemFile(
      paste0("markdown/validationReport_", report, ".Rmd"),
      package = "piamValidation")
    report_name <- report
  }

  # put rendered reports in output folder in working directory
  output_path <- paste0(getwd(), "/output")
  if (!dir.exists(output_path)) dir.create(output_path)

  # include chosen config and report name in output file except if it is default
  infix <- ""
  if (config_name != "default") infix <- paste0(infix, "_cfg", config_name)
  if (report_name != "default") infix <- paste0(infix, "_rep", report_name)

  # create specified report for given data and config
  yamlParams <- list(mif = dataPath, cfg = config)
  rmarkdown::render(input = report_path,
                    params = yamlParams,
                    output_file = paste0(output_path, "/validation", infix,
                                         format(Sys.time(), "_%Y%m%d-%H%M%S"),
                                         ".html"))
}

