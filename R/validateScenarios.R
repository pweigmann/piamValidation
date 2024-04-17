#' performs the validation checks from a config on a scenario data set
#'
#' @param dataPath one or multiple path(s) to scenario data in .mif or .csv
#'        format, in case of historic comparison, also path to reference data
#' @param config select config from inst/config
#' @param outputFile give name of output file in case results should be exported
#'
#' @importFrom dplyr filter select mutate group_by %>%
#' @export
validateScenarios <- function(dataPath, config, outputFile = NULL) {

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

  # export df to file in case outputFile is specified
  if (!is.null(outputFile)) {
    output_path <- paste0(path.package("piamValidation"), "/output")
    if (!dir.exists(output_path)) dir.create(output_path)
    write.csv(df, paste0(output_path, "/", outputFile, ".csv"),
               row.names = FALSE, quote = FALSE)
  }

  return(df)
}



