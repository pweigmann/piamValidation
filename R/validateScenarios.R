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
importConfig <- function(configName) {
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


combineData <- function(data, hist, cfg) {

  # dimensions of data
  by <- c("variable", "region", "period", "scenario")

  # full dimensions and important slices
  all_reg <- unique(data$region)
  all_per <- unique(data$period)  # not a factor, convert?
  all_per <- all_per[all_per <= 2100]
  hist_per <- c(2005, 2010, 2015, 2020)
  all_sce <- unique(data$scenario)

  # revert the sorting of factors for better plots
  # data$region <- factor(data$region, levels = rev(levels(data$region)))

  # define colors here or later
  colors <- c(green = "#008450", yellow = "#EFB700", red = "#B81D13", grey = "#808080")

  ### here, category specifics start

  # category 1 data objects
  d1 <- data.frame()
  c1 <- cfg[cfg$category == 1, ]

  # assemble data by row of cfg
  for (i in 1:nrow(c1)) {
    # create filters
    # check whether regions, periods, scenarios are specified
    # TODO: extend to support excluding elements, multiple elements
    reg <- if (is.na(c1[i, "region"]))   all_reg else strsplit(c1[i, "region"], split = ", |,")[[1]]
    per <- if (is.na(c1[i, "period"]))  hist_per else strsplit(as.character(c1[i, "period"]), split = ", |,")[[1]]
    sce <- if (is.na(c1[i, "scenario"])) all_sce else strsplit(c1[i, "scenario"], split = ", |, ")[[1]]

    # filter data for each row in cfg
    # REMIND data for line i
    d <- data %>%
      filter(variable == c1[i, "variable"],
             region %in% reg,
             period %in% per,
             scenario %in% sce)
    # here?: skip if empty

    # historical data for line i
    h <- hist %>%
      filter(variable == c1[i, "variable"],
             region %in% reg,
             period %in% per) %>%
      mutate(ref_value = value, ref_model = model) %>%
      select(-c("scenario", "unit", "value", "model"))
    # remove NA here?

    # in case one or more sources are specified, filter for them
    if (!is.na(c1[i, "ref_model"])) {
      h <- filter(h, ref_model %in% strsplit(c1[i, "ref_model"], split = ", |,")[[1]])
    }

    # in case of multiple sources, calculate mean (using all available sources)
    if (length(unique(h$ref_model)) > 1) {
      h_mean <- group_by(h, period, region) %>%
        summarise(ref_value = mean(ref_value, na.rm = TRUE))
      h <- mutate(h_mean, variable = c1[i, "variable"], ref_model = "multiple")
    }

    # Merge REMIND, historical and config data
    # (works as long as we only have one set of thresholds per cfg row)
    d <- merge(d, h) %>%
      mutate(min_red = c1[i, ]$min_red,
             min_yel = c1[i, ]$min_yel,
             max_yel = c1[i, ]$max_yel,
             max_red = c1[i, ]$max_red,
             metric  = c1[i, ]$metric)

    d1 <- rbind(d1, d)
  }

  return(df)
}


evaluateThreshold <- function(df) {

  return(df)
}



# bringing it all together
validateScenario <- function() {

  data <- importData()

  hist <- importReferenceData()

  cfg <- importConfig(configName)

  cfg <- cleanConfig()

  df <- combineData(data, cfg)

  evaluateThreshold(df)

  df <- createTooltip(df)

  return(df)
}



