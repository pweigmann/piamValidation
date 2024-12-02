#' takes the output of "validateScenarios()" and plots heat maps per variable
#'
#' @param df data.frame to be plotted, as returned by ``validateScenarios()``
#'        plus optional filtering. Only works for data point with "reference"
#' @param var choose variable to plot
#' @param reg choose region to plot
#' @param scenData decide whether scenario data should be part of the plot
#' @param refData decide whether reference data should be part of the plot
#' @importFrom dplyr filter mutate %>%
#' @import ggplot2
#' @export

linePlotThresholds <- function(df, var, reg, scenData = FALSE, refData = FALSE) {
  # for now just for relative - historical checks
  # TODO extend to other categories, probably checking if is.na(ref_value) will be useful
  d <- df %>%
    filter(variable == var,
           region == reg,
           metric == "relative",
           ref_scenario == "historical")

  # convert relative to absolute thresholds
  # if several references (regional overwriting or period-smoothing), use the last one
  d_background <- d %>%
    group_by(period) %>%
    summarise(
      min_red = ((1+min_red) * ref_value)[max(which(!is.na(min_red)))],
      min_yel = ((1+min_yel) * ref_value)[max(which(!is.na(min_yel)))],
      max_yel = ((1+max_yel) * ref_value)[max(which(!is.na(max_yel)))],
      max_red = ((1+max_red) * ref_value)[max(which(!is.na(max_red)))]
    )

# plot thresholds as colored background areas
  p <- ggplot() +
    ylab(var) +
    geom_ribbon(data = d_background, aes(x = period, ymin = min_yel, ymax = max_yel),
                fill = "lightgreen", alpha = 0.2, color = NA, inherit.aes = FALSE) +
    geom_ribbon(data = d_background, aes(x = period, ymin = max_yel, ymax = max_red),
                fill = "orange", alpha = 0.2, color = NA, inherit.aes = FALSE) +
    geom_ribbon(data = d_background, aes(x = period, ymin = min_red, ymax = min_yel),
                fill = "lightblue", alpha = 0.3, color = NA, inherit.aes = FALSE)

  # add scenario data as lines
  if (scenData) {
    p <- p +
      geom_line(data = d,
                aes(x = period, y = value, color = model, linetype = scenario))
  }

  # add reference data as points
  if (refData) {
    p <- p +
      geom_point(data = d, aes(x = period, y = ref_value), #, shape = ref_model),
                 size = 1, color = "black")
  }

  # layout
  p <- p +
    ggtitle(paste(var, "-", reg))

  p
}
