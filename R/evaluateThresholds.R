#' @importFrom dplyr filter select mutate summarise group_by ungroup %>% lag arrange

# cleanInf = TRUE: replace "Inf" and "-Inf" which were introduced
#                  for ease of calculations with "-"
evaluateThresholds <- function(df, cleanInf = TRUE, extraColors = TRUE) {

  # first calculate values that will be compared to thresholds for each category
  # ("check_value") and metric separately, then perform evaluation for all together

  # relative ####
  rel <- df[df$metric == "relative", ] %>%
    mutate(
      check_value_min = ifelse(
        is.na(ref_value_min),
        NA,
        # relative deviation above/below min reference
        (value - ref_value_min) / ref_value_min
        ),
      check_value_max = ifelse(
        is.na(ref_value_max),
        NA,
        # relative deviation above/below max reference
        (value - ref_value_max) / ref_value_max
        )
    ) %>%
    # special case: ref_value and value are both zero should show as 0 deviation
    mutate(check_value_min = ifelse(value == 0 & ref_value_min == 0,
                                    0,
                                    check_value_min),
           check_value_max = ifelse(value == 0 & ref_value_max == 0,
                                    0,
                                    check_value_max)
    )

  # difference ####
  dif <- df[df$metric == "difference", ] %>%
    # difference to reference
    mutate(check_value_min = value - ref_value_min,
           check_value_max = value - ref_value_max)

  # absolute ####
  abs <- df[df$metric == "absolute", ] %>%
    mutate(check_value_min = value,
           check_value_max = value)

  # growthrate ####
  # calculate average growth rate between periods
  gro <- df %>%
    filter(.data$metric == "growthrate") %>%
    group_by(.data$model, .data$scenario, .data$region, .data$variable) %>%
    arrange(.data$period) %>%
    mutate(diffyear = .data$period - lag(.data$period),
           check_value_min =
             ifelse(lag(.data$value) %in% c(0, NA),
                    NA,
                    (.data$value/lag(.data$value))^(1/.data$diffyear) - 1),
           check_value_max = check_value_min) %>%
    select(-"diffyear") %>%
    ungroup()

  # reassemble data.frame
  df <- do.call("rbind",
                list(rel, dif, abs, gro))

  # color evaluation ####
  df <- df %>%
    mutate(check = dplyr::case_when(
      (is.na(check_value_min) | is.na(check_value_max) |
       is.infinite(check_value_min) | is.infinite(check_value_max)) ~ "grey",
      # below min red
      !is.na(min_red) & check_value_min < min_red ~ ifelse(
        extraColors, "blue", "red"),
      # below min yellow
      !is.na(min_yel) & check_value_min < min_yel ~ ifelse(
        extraColors, "cyan", "yellow"),
      # above max red
      !is.na(max_red) & check_value_max > max_red ~ "red",
      # above max yellow
      !is.na(max_yel) & check_value_max > max_yel ~ "yellow",
      # everything else is green
      TRUE ~ "green"
      ))

  if (any(is.infinite(c(df$check_value_min, df$check_value_max)))) {
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
