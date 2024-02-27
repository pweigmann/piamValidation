#' @importFrom dplyr filter select mutate summarise group_by %>%

# import data
# supported formats:
# - .mif
# -


# dev helpers
#configName <- "validationConfig.csv"
#usethis::use_data(configName, internal = T)



importData <- function() {
  # for dev purposes
  if (file.exists("mif.rds")) {
    data <- readRDS("mif.rds")
  } else {
    path <- "C:/Users/pascalwe/Data/vs/"
    data <- remind2::deletePlus(quitte::read.quitte(
      c(paste0(path, "REMIND_generic_Bal.mif"),
        paste0(path, "REMIND_generic_NPi.mif"))
        )
      )
    saveRDS(data, "mif.rds")
  }

  # change ordering of factors, put last element ("World") first
  # TODO: will not work if World is not last
  new_order <- c(tail(levels(data$region), 1), head(levels(data$region), -1))
  data$region <- factor(data$region, levels = new_order)

  return(data)
}

importReferenceData <- function() {
  # for dev purposes
  if (file.exists("hist.rds")) {
    hist <- readRDS("hist.rds")
  } else {
    hist <- quitte::read.quitte(paste0(path, "historical.mif"))
    hist <- filter(hist, period >= 1990)
    saveRDS(hist, "hist.rds")
  }

  # remove all historical data before 1990
  hist <- filter(hist, period >= 1990)

  return(hist)
}

# required columns:
# - category: "historic" or "scenario"
# - can be empty, contain one element or multiple:
#   - model, scenario, region, period
# ...
loadConfig <- function(configName) {
  path <- system.file(paste0("config/", configName) , package = "piamValidation")
  cfg <- read.csv2(path, na.strings = "")
  return(cfg)
}


cleanConfig <- function(cfg) {
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


evaluateThreshold <- function(df) {

  return(df)
}



# bringing it all together
# TODO: introduce file path and name as argument
validateScenarios <- function() {

  data <- importData()

  hist <- importReferenceData()

  cfg <- loadConfig(configName)

  cfg <- cleanConfig(cfg)


  # combine data for each row of the config and bind together
  df <- data.frame()
  for (i in 1:nrow(cfg)) {
    # TODO: hist should only be needed if category "historical" is in config
    # validation generally should work without hist data
    df_row <- combineData(data, hist, cfg[i, ])
    df <- rbind(df, df_row)
  }

  df <- resolveDuplicates(df)

  df <- evaluateThresholds(df)

  df <- appendTooltips(df)

  return(df)
}



