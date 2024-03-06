#' @importFrom dplyr filter select mutate summarise group_by %>%

# bringing it all together
# keepNA
#' @export
validateScenarios <- function(scenarioPath, configName, referencePath = NULL,
                              keepNA = TRUE) {

  data <- importScenarioData(scenarioPath)

  hist <- importReferenceData(referencePath)

  cfg <- configName %>%
    getConfig() %>%
    cleanConfig() %>%
    expandPeriods(data) %>%
    expandVariables(data)


  # combine data for each row of the config and bind together
  df <- data.frame()
  for (i in 1:nrow(cfg)) {
    # TODO: hist should only be needed if category "historical" is in config
    #       validation generally should work without hist data
    # TODO: optimize performance
    df_row <- combineData(data, cfg[i, ], ref_data = hist)
    df <- rbind(df, df_row)
    cat(paste0("Combined row ", i, " of ", nrow(cfg), "\n"))
  }

  df <- resolveDuplicates(df)

  df <- evaluateThresholds(df)

  return(df)
}



