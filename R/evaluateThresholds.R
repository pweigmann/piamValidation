#' @importFrom dplyr filter select mutate summarise group_by ungroup %>% lag arrange

# cleanInf = TRUE: replace "Inf" and "-Inf" which were introduced
#                  for ease of calculations with "-"
evaluateThresholds <- function(df, cleanInf = TRUE, extraColors = TRUE) {

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
  # calculate average growth rate between periods
  gro <- df %>%
    filter(.data$metric == "growthrate") %>%
    group_by(.data$model, .data$scenario, .data$region, .data$variable) %>%
    arrange(.data$period) %>%
    mutate(diffyear = .data$period - lag(.data$period),
           check_value =
             ifelse(lag(.data$value) %in% c(0, NA),
                    NA,
                    (.data$value/lag(.data$value))^(1/.data$diffyear) - 1)) %>%
    select(-"diffyear") %>%
    ungroup()

  # reassemble data.frame
  df <- do.call("rbind",
                list(rel, dif, abs, gro))

  # evaluation ####
  # perform comparison to thresholds for whole data.frame at once
  df <- df %>%
    mutate(check = case_when(
      is.na(check_value) | is.infinite(check_value) ~ "grey",
      !is.na(min_red) & check_value < min_red ~ ifelse(extraColors, "blue", "red"),    # Below min red
      !is.na(min_yel) & check_value < min_yel ~ ifelse(extraColors, "cyan", "yellow"), # Below min yellow
      !is.na(max_red) & check_value > max_red ~ "red",                                   # Above max red
      !is.na(max_yel) & check_value > max_yel ~ "yellow",                                # Above max yellow
      TRUE ~ "green"                                                                     # Everything else is green
    ))

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
