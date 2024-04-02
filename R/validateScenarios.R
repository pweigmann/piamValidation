#' performs the validation checks from a config on a scenario dataset
#'
#' @param scenarioPath one or multiple path(s) to scenario data in .mif or .csv
#'        format
#' @param configName select config from inst/config
#' @param referencePath in case of historic comparison, choose path to ref data
#'
#' @importFrom dplyr filter select mutate group_by %>%
#' @export
validateScenarios <- function(scenarioPath, configName, referencePath = NULL) {

  data <- importScenarioData(scenarioPath)

  if (!is.null(referencePath)) {
    hist <- importReferenceData(referencePath)
  } else {
    hist <- NULL
  }

  cfg <- configName %>%
    getConfig() %>%
    fillInf() %>%
    expandPeriods(data) %>%
    expandVariables(data)

  # TODO: filter data for relevant variables

  # combine data for each row of the config and bind together
  df <- data.frame()
  for (i in 1:nrow(cfg)) {
    # TODO: hist should only be needed if category "historical" is in config
    #       validation generally should work without hist data
    df_row <- combineData(data, cfg[i, ], refData = hist)
    df <- rbind(df, df_row)
    cat(paste0("Combined config row ", i, " of ", nrow(cfg), "\n"))
  }

  df <- resolveDuplicates(df)

  df <- evaluateThresholds(df)

  return(df)
}



