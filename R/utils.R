#' List available configs
#'
#' List all validation configuration files that are delivered with the package
#' and can be directly imported with ``getConfig()`` or used in
#' ``validateScenarios()`` and ``validationReport()``.
#'
#' @export
listConfigs <- function() {

  configs <- list.files(system.file("config/", package = "piamValidation"))
  configs <- gsub("validationConfig_|.csv", "", configs)

  cat("Available configuration files\n")
  paste(configs)
}


#' Print a summary of the input data
#'
#' Print multiple metrics of all data objects given to ``validateScenarios()``
#' which might be helpful to spot and fix data inconsistencies.
#'
#' @param scen scenario data as used in ``validateScenarios()``
#' @param hist historical/reference data as used in ``validateScenarios()``
#' @param config config as used in ``validateScenarios()``
#'
#' @export
showInputSummary <- function(scen, hist, config) {
  nRows <- nrow(scen)
  nHist <- nrow(hist)
  nModels <- length(unique(scen$model))
  nScenarios <- length(unique(scen$scenario))
  nRegions <- length(unique(scen$region))
  nVariables <- length(unique(scen$variable))

  cfgVars <- unique(config$variable)
  scenVars <- unique(scen$variable)
  histVars <- unique(hist$variable)

  nCfgVars <- length(cfgVars)
  nMatchedScen <- sum(cfgVars %in% scenVars)
  nMatchedHist <- sum(cfgVars %in% histVars)

  # print summary
  message(paste(
    "\nValidation input summary:\n",
    "- Rows Scenario Data: ", nRows, "\n",
    "- Rows Historical Data: ", nRows, "\n",
    "- Models: ", nModels, "\n",
    "- Scenarios: ", nScenarios, "\n",
    "- Regions: ", nRegions, "\n",
    "- Variables (config): ", nCfgVars, "\n",
    "- Variables (data): ", nVariables, "\n",
    "- Variables (hist): ", nVariables, "\n",
    "- # of config vars in scen data: ", nMatchedScen, "\n",
    "- # of config vars in hist data: ", nMatchedHist, "\n"
  ))

  # warn if nothing matches
  if (nMatchedScen == 0 | nMatchedHist == 0)  {
    warning(paste(
      "No variables in data match variables in config.\n",
      "Validation will return empty results.\n\n",
      "Example data variables:\n",
      paste(head(scenVars, 5), collapse = ", "), "\n\n",
      "Example config variables:\n",
      paste(head(cfgVars, 5), collapse = ", ")
    ))
  }
}

#' Average 2020 to smoothen Covid shock in historical data
#'
#' Adds a new model for each model in reference data with smoothed 2020 period
#' and name "<model>_smoothed".
#'
#' @param hist reference data as used in ``validateScenarios()``
average_2020 <- function(hist) {
  hist_m <- hist %>%
    filter(period %in% seq(2018, 2022)) %>%
    magclass::as.magpie(spatial = "region")
  hist_m[, 2020, ] <- magclass::dimSums(hist_m, dim = 2)/5
  hist_smoothed <- quitte::as.quitte(hist_m[, , ]) %>%
    filter(period == 2020) %>%
    mutate(model = paste0(model, "_smoothed"))

  hist <- rbind(hist, hist_smoothed)
  return(hist)
  }

