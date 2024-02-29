#' @importFrom dplyr filter select mutate summarise group_by %>%

# bringing it all together
#' @export
validateScenarios <- function(scenarioPath, referencePath, configName) {

  data <- importScenarioData(scenarioPath)

  hist <- importReferenceData(referencePath)

  cfg <- getConfig(configName)

  cfg <- cleanConfig(cfg)


  # combine data for each row of the config and bind together
  df <- data.frame()
  for (i in 1:nrow(cfg)) {
    # TODO: hist should only be needed if category "historical" is in config
    #       validation generally should work without hist data
    df_row <- combineData(data, hist, cfg[i, ])
    df <- rbind(df, df_row)
  }

  df <- resolveDuplicates(df)

  df <- evaluateThresholds(df)

  return(df)
}



