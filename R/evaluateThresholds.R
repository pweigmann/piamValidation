#' @importFrom dplyr filter select mutate summarise group_by %>%

# cleanInf = TRUE: replace "Inf" and "-Inf" which were introduced
#                  for ease of calculations with "-"
evaluateThresholds <- function(df, cleanInf = TRUE) {

  # first calculate values that will be compared to thresholds for each category
  # ("check_value") and metric separately, then perform evaluation for all together

  # relative ####
  rel <- df[df$metric == "relative", ] %>%
    mutate(check_value = ifelse(
      is.na(ref_value),
      NA,
      # relative deviation above/below reference
      (value - ref_value) / ref_value
      )
    ) %>%
    # special case: ref_value and value are both zero should show as 0 deviation
    mutate(check_value = ifelse(value == 0 & ref_value == 0,
                                0,
                                check_value)
    )

  # difference ####
  dif <- df[df$metric == "difference", ] %>%
    # difference to reference
    mutate(check_value = value - ref_value)

  # absolute ####
  abs <- df[df$metric == "absolute", ] %>%
    mutate(check_value = value)

  # growthrate ####
  # calculate average growth rate of the last 5 years between 2010 and 2060
  # TODO: in case of a need to look further than 2060, split df and add
  # calculations for 10-year step periods
  gro <- filter(df[df$metric == "growthrate", ],
                    period <= 2060 & period >= 2010)

  # add a column with the value 5 years ago for later calculations
  tmp <- gro
  gro$period <- gro$period + 5
  gro <- gro %>%
    mutate(value_5y_ago = value) %>%
    select(-value) %>%
    merge(tmp)

  # calculate the yearly average growth rate
  gro <- gro %>%
    mutate(check_value = (value/value_5y_ago)^(1/5) - 1) %>%
    select(-value_5y_ago)


  # reassemble data.frame
  df <- do.call("rbind",
                list(rel, dif, abs, gro))

  # evaluation ####
  # perform comparison to thresholds for whole data.frame at once
  # TODO: not as robust as previously thought. Partially fails if only max_red is given
  df <- df %>%
    mutate(check = ifelse(is.na(check_value) | is.infinite(check_value),
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

  if (any(is.infinite(df$check_value))) {
    cat(
    "A relative check to a reference value of zero was performed. Make sure you
    use the right reference data or try checking for a difference instead. \n")
  }

  # after evaluation, "Inf" can be removed
  if (cleanInf) {
    df <- df %>%
      mutate(min_red = ifelse(is.infinite(min_red), NA, min_red),
             min_yel = ifelse(is.infinite(min_yel), NA, min_yel),
             max_yel = ifelse(is.infinite(max_yel), NA, max_yel),
             max_red = ifelse(is.infinite(max_red), NA, max_red)
             )
  }

  return(df)
}
