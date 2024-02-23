#' @importFrom dplyr filter select mutate summarise group_by %>%

resolveDuplicates <- function(df) {

  # TODO: needs to be double checked for edge-cases
  #       might be easier or safer to handle categories separately

  # check for duplicates
  # these are the duplicates that are from the lower rows and should be kept
  duplicates <- df[duplicated(df[c("model", "scenario", "variable",
                                       "region", "period", "category", "metric")]), ]

  # here all instances of the data that was duplicated is removed
  no_dupl <- dplyr::anti_join(df, duplicates_all, by = c("model", "scenario", "variable",
                                                         "region", "period", "category", "metric"))

  df <- rbind(no_dupl, duplicates)

  # different approach: allows partial overwriting
  # chosen to not use this anymore, less robust

  # these are missing the duplicated entries that were in the lower rows
  # no_dupl <- dplyr::anti_join(df, duplicates_all)

  # overwrite "earlier" values using "later" ones
  # BEWARE: requires specific country to come after "all regions"
  # d1 <- merge(no_dupl, duplicates_all, by=c("variable", "region", "period",
  #                                           "scenario", "unit", "model", "value",
  #                                           "ref_model", "ref_value","ref_scenario",
  #                                           "ref_period", "category", "metric", "critical"),
  #             all.x = T) %>%
  #   mutate(min_red = ifelse(is.na(min_red.y), min_red.x, min_red.y)) %>%
  #   mutate(min_yel = ifelse(is.na(min_yel.y), min_yel.x, min_yel.y)) %>%
  #   mutate(max_yel = ifelse(is.na(max_yel.y), max_yel.x, max_yel.y)) %>%
  #   mutate(max_red = ifelse(is.na(max_red.y), max_red.x, max_red.y)) %>%
  #   select(-c("min_red.x", "min_yel.x", "max_yel.x", "max_red.x",
  #             "min_red.y", "min_yel.y", "max_yel.y", "max_red.y"))

  return(df)
}
