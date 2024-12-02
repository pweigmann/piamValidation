#' performs the validation checks from a config on a scenario data set
#'
#' @param dataPath one or multiple path(s) to scenario data in .mif or .csv
#'        format, in case of historic comparison, also path to reference data
#' @param config select config from inst/config or give a full path to a config
#'        file on your computer
#' @param outputFile give name of output file in case results should be exported;
#'        include file extension
#' @param extraColors if TRUE, use cyan and blue for violation of min thresholds
#'        instead of using the same colors as for max thresholds (yel and red)
#'
#' @importFrom dplyr filter select mutate group_by %>% bind_rows
#'
#' @export
validateScenarios <- function(dataPath, config,
                              outputFile = NULL, extraColors = TRUE, loadBinary = TRUE) {

  data <- importScenarioData(dataPath, loadBinary)

  # historical/reference data has to have "historical" as scenario name
  hist <- filter(data, scenario == "historical")
  scen <- filter(data, scenario != "historical")

  # get config and convert it into a format that is easy to handle
  cfg <- config %>%
    getConfig() %>%
    fillInf() %>%
    expandPeriods(scen) %>%
    expandVariables(scen)
  # TODO: remove duplicate constraints here

  # TODO: check if all variables from config are in scenario data,
  # currently fails only with unit check

  # filter data for variables from config
  hist <- filter(hist, variable %in% unique(cfg$variable))
  scen <- filter(scen, variable %in% unique(cfg$variable))

  # combine scenario data (and reference data if needed) with the respective
  # thresholds for each row of the config and bind all into one data.frame
  # TODO: parallelization works but makes development harder, likely not needed
  # future::plan(future::multisession, workers = parallel::detectCores())
  valiData <- bind_rows(
    lapply(1:nrow(cfg), function(i) {
      combineData(
        scen[scen$variable %in% cfg[[i, "variable"]], ],
        cfg[i, ],
        histData = filter(hist, variable %in% cfg[[i, "variable"]])
        )
      }))

  # "lower" rows of config overwrite "higher" rows when describing the same data
  valiData <- resolveDuplicates(valiData)

  # perform actual checks and write results in new columns of data.frame
  valiData <- evaluateThresholds(valiData, extraColors = extraColors)

  if (nrow(valiData) == 0) {
    stop("Something went wrong, returned data.frame is empty.")
  }

  # export validated data to file in case outputFile is specified
  if (!is.null(outputFile)) {
    # in case full path is given
    if (file.exists(outputFile)) {
      write.csv(valiData, outputFile, row.names = FALSE, quote = FALSE)
    } else {
      output_path <- paste0(path.package("piamValidation"), "/output")
      if (!dir.exists(output_path)) dir.create(output_path)
      write.csv(valiData, paste0(output_path, "/", outputFile),
                row.names = FALSE, quote = FALSE)
    }
  }

  return(valiData)
}



