#' @importFrom dplyr filter select mutate summarise group_by %>%

# cleanInf = TRUE: choose to replace "Inf" and "-Inf" which were introduced
# for ease of calculations with "-"
evaluateThresholds <- function(df, cleanInf = TRUE) {

  # perform evaluation for each category and metric separately
  # (shortcuts are possible but will decrease readability of the code for minor
  # performance improvements)

  # calculate values that will be compared to thresholds
  # historic - relative
  his_rel <- df[df$category == "historic" & df$metric == "relative", ] %>%
    mutate(check_value = ifelse(
      is.na(ref_value),
      NA,
      # absolute relative deviation above/below reference
      abs((value - ref_value) / ref_value)
      )
    )

  # historic - difference
  his_dif <- df[df$category == "historic" & df$metric == "difference", ] %>%
    # absolute difference to reference
    mutate(check_value = abs(value - ref_value))

  # scenario - relative
  sce_rel <- df[df$category == "scenario" & df$metric == "relative", ] %>%
    mutate(check_value = ifelse(
      is.na(ref_value),
      NA,
      # relative deviation above/below reference
      (value - ref_value)/ref_value
      )
    )

  # scenario - difference
  sce_dif <- df[df$category == "scenario" & df$metric == "difference", ] %>%
    mutate(check_value = ifelse(
      is.na(ref_value),
      NA,
      # difference to  reference
      value - ref_value
      )
    )

  # scenario - absolute
  sce_abs <- df[df$category == "scenario" & df$metric == "absolute", ] %>%
    mutate(check_value = value)

  # scenario - growthrate
  # calculate average growth rate of the last 5 years between 2010 and 2060
  # TODO: in case of a need to look further than 2060, split df and add
  # calculations for 10-year step periods
  sce_gro <- filter(df[df$category == "scenario" & df$metric == "growthrate", ],
                    period <= 2060 & period >= 2010)

  # add a column with the value 5 years ago for later calculations
  tmp <- sce_gro
  sce_gro$period <- sce_gro$period + 5
  sce_gro <- sce_gro %>%
    mutate(value_5y_ago = value) %>%
    select(-value) %>%
    merge(tmp)

  # calculate the yearly average growth rate
  sce_gro <- sce_gro %>%
    mutate(check_value = (value/value_5y_ago)^(1/5) - 1) %>%
    select(-value_5y_ago)


  # reassemble data.frame
  df <- do.call("rbind",
                list(his_rel, his_dif, sce_rel, sce_dif, sce_abs, sce_gro))

  # perform comparison to thresholds for whole data.frame at once
  df <- df %>%
    mutate(check = ifelse(is.na(check_value),
                        "grey",
                        ifelse(
                          # first check whether red threshold is violated...
                          check_value > max_red | check_value < min_red,
                          "red",
                          # otherwise check if yellow threshold is violated...
                          ifelse(
                            check_value > max_yel | check_value < min_yel,
                            "yellow",
                            # ... else green
                            "green"
                            )
                          )
                        )
           )

  # after evaluation, "Inf" can be removed again
  if (cleanInf) df[df == "Inf" | df == "-Inf"] <- NA

  return(df)
}
