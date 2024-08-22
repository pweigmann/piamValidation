#' Check variable consistency
#'
#' Test whether unit of on row of config and data for this variable match.
#'
#' @importFrom piamInterfaces areUnitsIdentical
#' @importFrom dplyr filter
#'
#' @param data scenario or reference data for one variable
#' @param cfgRow one row of a config file containing the same variable as the
#'        data object
#'
checkUnits <- function(data, cfgRow) {
  # no unit in config or empty data object: skip the check
  if (nrow(data) != 0 & !is.na(cfgRow$unit)) {
    # identify data type
    dataType <-
      if(cfgRow$ref_scenario %in% "historical") "reference" else "scenario"

    units <- as.character(unique(data$unit))
    for (n in 1:length(units)) {
      if (!piamInterfaces::areUnitsIdentical(cfgRow$unit, units[n])) {
        warning(paste0(
          "Non-matching units in config and ", dataType," data found.\n",
          "variable: ", cfgRow$variable, "\n",
          "config unit: ", cfgRow$unit, "\n",
          dataType, " unit: ", units[n]), "\n")
        # in case of the presence of non-matching units:
        # filter data for correct unit as it might also available, if not this
        # will result in an empty data object being returned
        data <- filter(data, data$unit %in% cfgRow$unit)
      }
    }
  }
  return(data)
}
