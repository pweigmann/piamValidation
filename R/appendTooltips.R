#' construct tooltips for interactive plots
#' @param df data.frame as returned from `validateScenarios()`
#'
#' @export
appendTooltips <- function(df) {

  df$text <- NA
  region <- period <- NULL

  # scenario - relative
  df[df$metric == "relative", ] <-
    df[df$metric == "relative", ] %>%
    mutate(text = paste0(region, "\n",
                         period, "\n",
                         "Value: ", round(value, 2), "\n",
                         "Ref ", ifelse(!is.na(ref_period),
                                               paste("Period:", ref_period),
                                        ifelse(!is.na(ref_scenario) &
                                                 ref_scenario != "historical",
                                               paste("Scenario:", ref_scenario),
                                               paste("Model:", ref_model))
                                               ), "\n",
                         "Ref Value: ", round(ref_value, 2), "\n",
                         "Rel Deviation: ", round(check_value, 2)*100, "% \n",
                         "Thresholds (yel/red): \n",
                         "Min: ", min_yel*100, "% / ", min_red*100,"% \n",
                         "Max: ", max_yel*100, "% / ", max_red*100, "%")
           )

  # scenario - difference
  df[df$metric == "difference", ] <-
    df[df$metric == "difference", ] %>%
    mutate(text = paste0(region, "\n",
                         period, "\n",
                         "Value: ", round(value, 2), "\n",
                         "Ref ", ifelse(!is.na(ref_period),
                                        paste("Period:", ref_period),
                                        ifelse(!is.na(ref_scenario) &
                                                 ref_scenario != "historical",
                                               paste("Scenario:", ref_scenario),
                                               paste("Model:", ref_model))
                         ), "\n",
                         "Ref Value: ", round(ref_value, 2), "\n",
                         "Difference: ", round(check_value, 2), "\n",
                         "Thresholds (yel/red): \n",
                         "Min: ", min_yel, " / ", min_red,"\n",
                         "Max: ", max_yel, " / ", max_red
    )
    )

  # scenario - absolute
  df[df$metric == "absolute", ] <-
    df[df$metric == "absolute", ] %>%
    mutate(text = paste0(region, "\n",
                         period, "\n",
                         "Value: ", round(value, 2), "\n",
                         "Thresholds (yel/red): \n",
                         "Min: ", min_yel, " / ", min_red,"\n",
                         "Max: ", max_yel, " / ", max_red
                         )
           )

  # scenario - growthrate
  df[df$metric == "growthrate", ] <-
    df[df$metric == "growthrate", ] %>%
    mutate(text = paste0(region, "\n",
                         period, "\n",
                         "Avg. growth/yr: ", round(check_value*100), "% \n",
                         "Absolute value: ", round(value, 2), " \n",
                         "Thresholds (yel/red): \n",
                         "Min: ", min_yel*100, "% / ", min_red*100, "% \n",
                         "Max: ", max_yel*100, "% / ", max_red*100, "%"
                         )
           )

  return(df)
}
