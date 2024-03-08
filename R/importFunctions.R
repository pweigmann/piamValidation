#' @importFrom dplyr filter select mutate summarise group_by %>%

# scenarioPath: one or multiple paths to .mif or .csv file(s) containing
#               scenario data in IAM format
importScenarioData <- function(scenarioPath) {
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

  # remove rows without values
  data <- data[!is.na(data$value),]

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
    hist <- dplyr::filter(hist, period >= 1990)
    saveRDS(hist, "hist.rds")
  }

  # remove all historical data before 1990, (only historic?)
  hist <- dplyr::filter(hist, period >= 1990)

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
  cat(paste0("loading config file: ", configName, "\n"))
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


# replace period ranges using ":" with comma-separated list of years
expandPeriods <- function(cfg, data) {
  per_expand_idx <- grep("\\:", cfg$period)
  all_per <- unique(data$period)

  # iterate through rows with ":"
  for (i in per_expand_idx) {
    if (nchar(cfg[i, "period"]) != 9) {
      stop("Invalid range detected. Make sure to enter years as YYYY:YYYY.\n")
    } else {
      # find scenario-years that are withing indicated range
      start <- substr(cfg[i, "period"], 1, 4)
      if (start < min(all_per)) {
        warning("Selected period starts earlier than scenario data.\n")
        }
      stop <- substr(cfg[i, "period"], 6, 9)
      if (stop > max(all_per)) {
        warning("Selected period ends later than scenario data.\n")
      }
      selected_per <- all_per[all_per >= start & all_per <= stop]

      # overwrite ":" notation with list
      cfg[i, "period"] <- paste0(selected_per, collapse = ", ")
      }
  }
  return(cfg)
}


# takes config entries specifying a set of variables via "*" and expands it so
# that every variable corresponds to one row in cfg
# TODO: support "firstLevelOnly = TRUE" to not grab all sub-variables but just
#       one level
expandVariables <- function(cfg, data) {
  # create the expanded config, starting with the not-to-expand rows,
  # then appending the rows with expanded variable names
  var_expand <- cfg[grepl("\\*", cfg$variable), ]
  cfg_new <- dplyr::anti_join(cfg, var_expand, by = colnames(cfg))

  if (length(var_expand > 0)) {
    all_vars <- unique(data$variable)
    for (i in 1:nrow(var_expand)) {
      # prepare strings for grepping by adding escape characters and "."
      var <- gsub("\\*", "\\.\\*", var_expand$variable[i])
      var <- gsub("\\|", "\\\\|", var)
      selected_vars <- all_vars[grepl(var, all_vars)]
      cat(paste0(var_expand$variable[i], " was expanded into ",
                 length(selected_vars), " sub-variables.\n"))

      # take the original row for the current set of variables and repeat it
      # once for each sub-variable, overwrite with sub-variable names
      c <- cfg[i,] %>%
        dplyr::slice(rep(1, each = length(selected_vars)))
      c$variable <- selected_vars
      cfg_new <- rbind(cfg_new, c)
    }
  }
  return(cfg_new)
}
