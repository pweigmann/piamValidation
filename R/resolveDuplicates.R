# check for duplicates that are a result of config definition overlaps
# only the latest occurrence of a data point is kept (lowest row of config)
resolveDuplicates <- function(df) {

  # TODO: needs to be double checked for edge-cases (ref_scen = "historical"?)
  #       might be easier or safer to handle categories separately

  # these are the duplicates that are from the lower rows and should be kept
  duplicates <- df[duplicated(df[c("model",
                                   "scenario",
                                   "variable",
                                   "region",
                                   "period",
                                   "metric",
                                   "ref_model",
                                   "ref_scenario",
                                   "ref_period")]), ]

  # here, all instances of the data that was duplicated are removed
  no_dupl <- dplyr::anti_join(df, duplicates, by = c("model",
                                                     "scenario",
                                                     "variable",
                                                     "region",
                                                     "period",
                                                     "metric"))

  # reattach one instance to get a complete data set without duplicates
  df <- rbind(no_dupl, duplicates)


  return(df)
}
