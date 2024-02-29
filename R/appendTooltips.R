# construct tooltips for interactive plots
appendTooltips <- function(df) {

  df$text <- NA

  # historic - relative
  df[df$category == "historic" & df$metric == "relative", ] <-
    df[df$category == "historic" & df$metric == "relative", ] %>%
    mutate(text = paste0(region, "\n",
                         period, "\n",
                         "Value: ", round(value, 2), "\n",
                         "Ref_Value: ", round(ref_value, 2), "\n",
                         "Ref_Source: ", ref_model, "\n",
                         "Deviation:", round(check_value)*100, "%\n",
                         "Thresholds (yel/red): \n",
                         paste0("Max: ", max_yel*100, "% / ", max_red*100, "%")
    )
    )

  # historic - difference
  df[df$category == "historic" & df$metric == "difference", ] <-
    df[df$category == "historic" & df$metric == "difference", ] %>%
    mutate(text = paste0(region, "\n",
                         period, "\n",
                         "Value: ", round(value, 2), "\n",
                         "Ref_Value: ", round(ref_value, 2), "\n",
                         "Ref_Source: ", ref_model, "\n",
                         "Thresholds (yel/red): \n",
                         "Max: ", max_yel, " / ", max_red
    )
    )

  # scenario - relative
  df[df$category == "scenario" & df$metric == "relative", ] <-
    df[df$category == "scenario" & df$metric == "relative", ] %>%
    mutate(text = paste0(region, "\n",
                         period, "\n",
                         "Value: ", round(value, 2), "\n",
                         "Ref ", ifelse(!is.na(ref_period),
                                               paste("Period:", ref_period),
                                               paste("Scenario:", ref_scenario)
                                               ), "\n",
                         "Ref Value: ", round(ref_value, 2), "\n",
                         "Rel Deviation: ", round(check_value)*100, "% \n",
                         "Thresholds (yel/red): \n",
                         "Min: ", min_yel*100, "% / ", min_red*100,"% \n",
                         "Max: ", max_yel*100, "% / ", max_red*100, "%")
           )

  # scenario - difference
  df[df$category == "scenario" & df$metric == "difference", ] <-
    df[df$category == "scenario" & df$metric == "difference", ] %>%
    mutate(text = paste0(region, "\n",
                         period, "\n",
                         "Value: ", round(value, 2), "\n",
                         "Ref Value: ", round(ref_value, 2), "\n",
                         "Difference: ", round(check_value, 2), "\n",
                         "Thresholds (yel/red): \n",
                         "Min: ", min_yel, " / ", min_red,"\n",
                         "Max: ", max_yel, " / ", max_red
    )
    )

  # scenario - absolute
  df[df$category == "scenario" & df$metric == "absolute", ] <-
    df[df$category == "scenario" & df$metric == "absolute", ] %>%
    mutate(text = paste0(region, "\n",
                         period, "\n",
                         "Value: ", round(value, 2), "\n",
                         "Thresholds (yel/red): \n",
                         "Min: ", min_yel, " / ", min_red,"\n",
                         "Max: ", max_yel, " / ", max_red
                         )
           )

  # scenario - growthrate
  df[df$category == "scenario" & df$metric == "growthrate", ] <-
    df[df$category == "scenario" & df$metric == "growthrate", ] %>%
    mutate(text = paste0(region, "\n",
                         period, "\n",
                         "Avg. growth/yr: ", round(check_value)*100, "% \n",
                         "Absolute value: ", round(value, 2), " \n",
                         "Thresholds (yel/red): \n",
                         "Min: ", min_yel*100, "% / ", min_red*100, "% \n",
                         "Max: ", max_yel*100, "% / ", max_red*100, "%"
                         )
           )

  return(df)
}
