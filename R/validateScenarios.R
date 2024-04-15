#' performs the validation checks from a config on a scenario data set
#'
#' @param dataPath one or multiple path(s) to scenario data in .mif or .csv
#'        format, in case of historic comparison, also path to reference data
#' @param config select config from inst/config
#'
#' @importFrom dplyr filter select mutate group_by %>%
#' @export
validateScenarios <- function(dataPath, config) {

  data <- importScenarioData(dataPath)

  # historical/reference data has to have "historical" as scenario name
  hist <- filter(data, scenario == "historical")
  scen <- filter(data, scenario != "historical")

  # get config and convert it into a format that is easy to handle
  cfg <- config %>%
    getConfig() %>%
    fillInf() %>%
    expandPeriods(scen) %>%
    expandVariables(scen)

  # filter data for relevant variables
  hist <- filter(hist, variable %in% unique(cfg$variable))
  scen <- filter(scen, variable %in% unique(cfg$variable))

  # combine scenario data (and reference data if needed) with the respective
  # thresholds for each row of the config and bind all into one data.frame
  df <- data.frame()
  for (i in 1:nrow(cfg)) {
    df_row <- combineData(scen, cfg[i, ], histData = hist)
    df <- rbind(df, df_row)
    # cat(paste0("Combined config row ", i, " of ", nrow(cfg), "\n"))
  }

  # "lower" rows of config overwrite "higher" rows when describing the same data
  df <- resolveDuplicates(df)

  # perform actual checks and write results in new columns of data.frame
  df <- evaluateThresholds(df)

  return(df)
}



