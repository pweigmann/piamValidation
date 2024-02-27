#' @importFrom dplyr filter select mutate summarise group_by %>%

# for one row of cfg: filter and merge relevant scenario data with cfg
# results in one df that contains scenario data, reference data and thresholds
combineData <- function(data, hist, cfg_row) {

  # shorten as it will be used a lot
  c <- cfg_row

  # full dimensions and important slices
  all_reg <- unique(data$region)
  all_per <- unique(data$period)  # not a factor, convert?
  all_per <- all_per[all_per <= 2100]
  hist_per <- c(2005, 2010, 2015, 2020)
  all_sce <- unique(data$scenario)

  # create filters
  # check whether regions, periods, scenarios are specified
  reg <- if (is.na(c$region))   all_reg else
    strsplit(c$region, split = ", |,")[[1]]
  sce <- if (is.na(c$scenario)) all_sce else
    strsplit(c$scenario, split = ", |, ")[[1]]

  # empty "period" field means different years for historic or scenario category
  if (c$category == "historic") {
    per <- if (is.na(c$period))  hist_per else
      strsplit(as.character(c$period), split = ", |,")[[1]]
  } else {
    per <- if (is.na(c$period))  all_per else
      strsplit(as.character(c$period), split = ", |,")[[1]]
  }

  # filter scenario data for row in cfg
  d <- data %>%
    filter(variable == c$variable,
           region %in% reg,
           period %in% per,
           scenario %in% sce)

  # attach respective cfg information to data slice
  d <- d %>%
    mutate(min_red = c$min_red,  # could be set to NA as it is not used
           min_yel = c$min_yel,  # could be set to NA as it is not used
           max_yel = c$max_yel,
           max_red = c$max_red,
           category = c$category,
           metric   = c$metric,
           critical = c$critical)

  # depending on category: filter and attach reference values if they are needed
  if (c$category == "historic") {
    # historic data for relevant variable and dimensions (all sources)
    h <- hist %>%
      filter(variable == c$variable,
             region %in% reg,
             period %in% per) %>%
      mutate(ref_value = value, ref_model = model) %>%
      select(-c("scenario", "unit", "value", "model"))

    # TODO: insert test here if ref_model exists and has data to compare to

    # in case one or more sources are specified, filter for them
    if (!is.na(c$ref_model)) {
      h <- filter(
        h, ref_model %in% strsplit(c$ref_model, split = ", |,")[[1]]
      )
    }

    # in case of multiple sources, calculate mean (using all available sources)
    if (length(unique(h$ref_model)) > 1) {
      h_mean <- group_by(h, period, region) %>%
        summarise(ref_value = mean(ref_value, na.rm = TRUE))
      h <- mutate(h_mean, variable = c$variable, ref_model = "multiple")
    }

    # merge with historical data adds columns ref_value and ref_model
    df <- merge(d, h)

    # add columns which are not used in this category, fill NA
    df <- df %>% mutate(ref_period = NA, ref_scenario = NA)

    # filter and attach reference values if they are needed; scenario data
  } else if (c$category == "scenario") {

    # no reference values needed for these metrics, fill NA
    if (c$metric %in% c("absolute", "growthrate")) {
      df <- d %>%
        mutate(ref_value = NA,
               ref_model = NA,
               ref_period = NA,
               ref_scenario = NA)

      # get reference values for these metrics
    } else if (c$metric %in% c("relative", "difference")) {
      # TODO: only works for ref_period, add support for model/scenario as ref
      # TODO: support choosing ref_period AND model/scenario
      ref <- data %>%
        filter(variable == c$variable,
               region %in% reg,
               period == c$ref_period,
               scenario %in% sce) %>%
        mutate(ref_value = value, ref_period = period) %>%
        select(-c(period, value))
      df <- merge(d, ref)

      # add columns which are not used in this category, fill NA
      df <- df %>%
        mutate(ref_model = NA,
               ref_scenario = NA)

    } else {
      # TODO: have this warning here or earlier when cleaning config?
      warning("'metric' for category 'scenario' must be either 'absolute',
              'relative', 'difference' or 'growthrate'.")
    }

  } else {
    warning("'category' must be either 'historic' or 'scenario'.")
  }

  return(df)
}
