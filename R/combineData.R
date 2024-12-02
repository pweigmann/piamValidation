#' Combine scenario and reference data with thresholds
#'
#' for one row of cfg: filter and merge relevant scenario data with cfg
#' results in one df that contains scenario data, reference data and thresholds
#'
#' @param scenData scenario data
#' @param cfgRow one row of a config file
#' @param histData reference data
#'
#' @importFrom dplyr filter select mutate summarise group_by %>%
#' @importFrom piamInterfaces areUnitsIdentical
#'
#'
combineData <- function(scenData, cfgRow, histData = NULL, smoothYears = c(2020)) {

  # shorten as it will be used a lot
  c <- cfgRow

  # full dimensions and important slices
  all_mod <- unique(scenData$model)
  all_sce <- unique(scenData$scenario)
  all_reg <- unique(scenData$region)
  all_per <- unique(scenData$period)  # not a factor, convert?
  all_per <- all_per[all_per <= 2100]
  # TODO: check if this works well
  # if (!is.null(histData))
  ref_per <- c(2005, 2010, 2015, 2020)


  # create filters ####
  # check whether regions, periods, scenarios are specified, else use all
  mod <- if (is.na(c$model)) all_mod else
    strsplit(c$model, split = ", |, ")[[1]]
  sce <- if (is.na(c$scenario)) all_sce else
    strsplit(c$scenario, split = ", |, ")[[1]]
  reg <- if (is.na(c$region))   all_reg else
    strsplit(c$region, split = ", |,")[[1]]


  # empty "period" field means different years for historic category
  if (c$ref_scenario == "historical" && !is.na(c$ref_scenario == "historical")) {
    per <- if (is.na(c$period))  ref_per else
      strsplit(as.character(c$period), split = ", |,")[[1]]
  } else {
    per <- if (is.na(c$period))  all_per else
      strsplit(as.character(c$period), split = ", |,")[[1]]
  }

  # apply filters ####
  # filter scenario data according to each row in cfg
  d <- scenData %>%
    filter(variable %in% c$variable,
           model    %in% mod,
           scenario %in% sce,
           region   %in% reg,
           period   %in% per)

  # attach cfg information which is independent of category to data slice
  d <- d %>%
    mutate(min_red  = c$min_red,
           min_yel  = c$min_yel,
           max_yel  = c$max_yel,
           max_red  = c$max_red,
           metric   = c$metric,
           critical = c$critical)

  # test whether units of config and scenario data match
  d <- checkUnits(d, c)

  # validate with historical values ####
  # depending on category: filter and attach reference values if they are needed
  if (c$ref_scenario == "historical" && !is.na(c$ref_scenario == "historical")) {
    # historic data for relevant variable and dimensions (all sources)
    h <- histData %>%
      filter(variable %in% c$variable,
             region %in% reg) %>%
      mutate(ref_value = value, ref_model = model) %>%
      select(-c("scenario", "value", "model", "ssp", "emi.scen", "def.var"))

    # calculating averages for 5-year periods
    h_smoothed <- h %>%
      group_by(
        ref_model, region, variable, unit,
        period = round(period / 5) * 5) %>% # nearest multiple of 5 (eg 2020 for 2018-2022)
      filter(period %in% smoothYears) %>%
      summarise(ref_value = mean(ref_value), .groups = "drop")


    # combine the original and averaged historical values
    h <- bind_rows(
      h %>% mutate(period_smoothing = ifelse(period %in% smoothYears, "max", "default")),
      h_smoothed %>% mutate(period_smoothing = "min")
    )


    h <- h %>% filter(period %in% per)

    # test whether units of config and reference data match
    h <- checkUnits(h, c)

    # test whether historical ref_model exists and has data to compare to
    if (nrow(h) == 0) {
      cat(paste0("No reference data found for variable ", c$variable, ".\n"))
      df <- data.frame()
    } else {

      # in case one or more sources are specified, filter for them
      if (!is.na(c$ref_model)) {
        h <- h %>%
          filter(ref_model %in% strsplit(c$ref_model, split = ", |,")[[1]])
      }

      # in case of multiple sources, use mean (of all available sources)
      if (length(unique(h$ref_model)) > 1) {
        h_mean <- h %>%
          group_by(period, region, period_smoothing) %>%
          summarise(ref_value = mean(ref_value, na.rm = TRUE)) %>%
          mutate(variable = c$variable, ref_model = "multiple")
      }

      # merge with historical data adds columns ref_value and ref_model from h
      df <- merge(d, h, by = c("region", "variable", "unit", "period"))

      # for smoothed years, keep the max for upper-bounds and min for lower-bounds
      df <- df %>%
        group_by(across(-c(ref_value, period_smoothing))) %>%
        mutate(
          ref_value = case_when(
            #!(period %in% smoothYears & n() == 2) ~ ref_value, # no modification where no smoothing
            period_smoothing == "max" ~ max(ref_value, na.rm = TRUE),
            period_smoothing == "min" ~ min(ref_value, na.rm = TRUE),
            TRUE ~ ref_value
          )
        ) %>%
        ungroup()

      df <- df %>%
        mutate(min_red = ifelse(period_smoothing == "max", NA, min_red),
               min_yel = ifelse(period_smoothing == "max", NA, min_yel),
               max_yel = ifelse(period_smoothing == "min", NA, max_yel),
               max_red = ifelse(period_smoothing == "min", NA, max_red),
        ) %>%
        select(-period_smoothing)

      # add columns which are not used in this category
      df <- df %>% mutate(ref_period = as.numeric(NA),
                          ref_scenario = "historical")
    }

  # validate with other scenarios ####
  # filter and attach reference values if they are needed; scenario data
  } else {

    # no reference values needed for these metrics, fill NA
    # TODO: columns turn out as logicals for some reason
    if (c$metric %in% c("absolute", "growthrate")) {
      df <- d %>%
        mutate(ref_value    = as.numeric(NA),
               ref_model    = as.character(NA),
               ref_scenario = as.character(NA),
               ref_period   = as.numeric(NA))


    # get reference values for these metrics
    # TODO: support choosing ref_period AND model/scenario?
    # TODO: support ref_variable
    } else if (c$metric %in% c("relative", "difference")) {

      # if a reference model should be used, same scenario, same period
      if (!is.na(c$ref_model)) {
        ref <- scenData %>%
          filter(variable %in% c$variable,
                 model    %in% c$ref_model, # expects exactly one model
                 scenario %in% sce,
                 region   %in% reg,
                 period   %in% per) %>%

          mutate(ref_value = value, ref_model = model) %>%
          select(-c(model, value)) %>%
          # add columns which are not used in this category, fill NA
          mutate(ref_scenario = as.character(NA),
                 ref_period   = as.numeric(NA))

        df <- merge(d, ref, by = c("scenario", "region", "variable", "unit", "period"))

      # if a reference scenario should be used, same period, same model
      } else if (!is.na(c$ref_scenario)) {
        ref <- scenData %>%
          filter(variable %in% c$variable,
                 model    %in% mod,
                 scenario %in% c$ref_scenario,  # expects exactly one scenario
                 region   %in% reg,
                 period   %in% per) %>%

          mutate(ref_value = value, ref_scenario = scenario) %>%
          select(-c(scenario, value)) %>%
          # add columns which are not used in this category, fill NA
          mutate(ref_model  = as.character(NA),
                 ref_period = as.numeric(NA))

        df <- merge(d, ref, by = c("model", "region", "variable", "unit", "period"))

      # if a reference period should be used, same scenario, same model
      } else if (!is.na(c$ref_period)) {
        ref <- scenData %>%
          filter(variable %in% c$variable,
                 model    %in% mod,
                 scenario %in% sce,
                 region   %in% reg,
                 period   %in% c$ref_period) %>% # expects exactly one period

          mutate(ref_value = value, ref_period = period) %>%
          select(-c(period, value)) %>%
          # add columns which are not used in this category, fill NA
          mutate(ref_model    = as.character(NA),
                 ref_scenario = as.character(NA))

        df <- merge(d, ref, by = c("model", "scenario", "region", "variable", "unit"))
      }



    } else {
      # TODO: have this warning here or earlier when cleaning config?
      warning("'metric' must be either 'absolute',
              'relative', 'difference' or 'growthrate'.")
    }

  }

  return(df)
}
