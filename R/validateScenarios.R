#' @importFrom dplyr filter select mutate summarise group_by %>%

# performs the validation checks from a config on a scenario dataset
#' @export
validateScenarios <- function(scenarioPath, configName, referencePath = NULL) {

  data <- importScenarioData(scenarioPath)

  hist <- importReferenceData(referencePath)

  cfg <- configName %>%
    getConfig() %>%
    fillInf() %>%
    expandPeriods(data) %>%
    expandVariables(data)


  # combine data for each row of the config and bind together
  df <- data.frame()
  for (i in 1:nrow(cfg)) {
    # TODO: hist should only be needed if category "historical" is in config
    #       validation generally should work without hist data
    df_row <- combineData(data, cfg[i, ], ref_data = hist)
    df <- rbind(df, df_row)
    cat(paste0("Combined config row ", i, " of ", nrow(cfg), "\n"))
  }

  df <- resolveDuplicates(df)

  df <- evaluateThresholds(df)

  return(df)
}



