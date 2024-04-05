#' performs the validation checks from a config on a scenario dataset
#'
#' @param scenarioPath one or multiple path(s) to scenario data in .mif or .csv
#'        format, in case of historic comparison, append path to ref data
#' @param configName select config from inst/config
#'
#' @importFrom dplyr filter select mutate group_by %>%
#' @export
validateScenarios <- function(scenarioPath, configName) {

  # TODO: filter data for relevant variables
  data <- importScenarioData(scenarioPath)

  # historical/reference data has to have "historical" as scenario name
  hist <- filter(data, scenario == "historical")
  data <- filter(data, scenario != "historical")

  # get config and convert it into a format that is easy to handle
  cfg <- configName %>%
    getConfig() %>%
    fillInf() %>%
    expandPeriods(data) %>%
    expandVariables(data)

  # combine data for each row of the config and bind together
  df <- data.frame()
  for (i in 1:nrow(cfg)) {
    # TODO: validation generally should work without hist data
    df_row <- combineData(data, cfg[i, ], histData = hist)
    df <- rbind(df, df_row)
    # cat(paste0("Combined config row ", i, " of ", nrow(cfg), "\n"))
  }

  df <- resolveDuplicates(df)

  df <- evaluateThresholds(df)

  return(df)
}



