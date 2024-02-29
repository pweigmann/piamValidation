#' @importFrom dplyr filter select mutate summarise group_by %>%

# scenarioData: one or multiple paths to .mif or .csv file(s) containing
#               scenario data in IAM format
importData <- function(scenarioPath) {
  # for dev purposes use cache
  if (file.exists("mif.rds")) {
    cat("loading scenario data from cache\n")
    data <- readRDS("mif.rds")
  } else {
    cat("importing scenario data from file(s)\n")
    data <- remind2::deletePlus(quitte::as.quitte(scenarioPath))
    saveRDS(data, "mif.rds")
  }

  # change ordering of factors, put last element ("World") first
  # TODO: will not work if World is not last, put somewhere else?
  new_order <- c(tail(levels(data$region), 1), head(levels(data$region), -1))
  data$region <- factor(data$region, levels = new_order)

  return(data)
}

importReferenceData <- function(referencePath) {
  # for dev purposes use cache
  if (file.exists("hist.rds")) {
    cat("loading reference data from cache\n")
    hist <- readRDS("hist.rds")
  } else {
    cat("importing reference data from file\n")
    hist <- quitte::as.quitte(referencePath)
    hist <- filter(hist, period >= 1990)
    saveRDS(hist, "hist.rds")
  }

  # remove all historical data before 1990, (only historic?)
  hist <- filter(hist, period >= 1990)

  return(hist)
}

# required columns:
# - category: "historic" or "scenario"
# - can be empty, contain one element or multiple:
#   - model, scenario, region, period
# ...
getConfig <- function(configName) {
  # TODO: check if config is available, otherwise is it a local file? if yes load that one
  path <- system.file(paste0("config/", configName), package = "piamValidation")
  cfg <- read.csv2(path, na.strings = "")
  return(cfg)
}


cleanConfig <- function(cfg) {
  # TODO: insert functionality to expand a row of the config, that should be
  #       used for multiple variables, via "Var|*"
  # fill empty threshold columns with Infinity for easier evaluation
  cfg <- cfg %>%
    mutate(min_red = as.numeric(ifelse(is.na(min_red), -Inf, min_red)),
           min_yel = as.numeric(ifelse(is.na(min_yel), -Inf, min_yel)),
           max_yel = as.numeric(ifelse(is.na(max_yel),  Inf, max_yel)),
           max_red = as.numeric(ifelse(is.na(max_red),  Inf, max_red))
    )

  # insert tests here to check if there are forbidden fields in config
  return(cfg)
}
