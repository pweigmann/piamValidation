#' takes the output of "validateScenarios()" and plots heatmaps per variable
#'
#' @param df data.frame as returned by ``validateScenarios()``
#'        and ``appendTooltips()``
#' @param var variable to be plotted
#' @param met choose metric from "relative", "difference", "absolute" or
#'        "growthrate"
#' @param historical should this be a plot comparing to historical data
#' @param interactive return plots as interactive plotly plots by default
#' @param x_plot choose dimension to display on x-axis of plot, default: region
#' @param y_plot choose dimension to display on y-axis of plot, default: period
#' @param x_facet choose dimension to display on x-dim of facets, default: model
#' @param y_facet choose dimension to display on x-dim of facets, default: scenario
#'
#' @importFrom dplyr filter select mutate %>%
#' @import ggplot2
#' @importFrom ggthemes theme_tufte
#' @importFrom plotly ggplotly
#' @export

validationHeatmap <- function(df,
                              var,
                              met,
                              historical = TRUE,
                              interactive = TRUE,
                              x_plot = "region",
                              y_plot = "period",
                              x_facet = "model",
                              y_facet = "scenario") {

  # wip: when giving multiple vars, plot as facets in same row
  if (length(var) > 1) {
    d <- df3 %>%
      filter(.data$metric == met)
    if (historical) {
      d <- filter(d, ref_scenario == "historical")
      plot_title <- paste0("Summary ", met, " (historical)")
    } else {
      d <- filter(d, (ref_scenario != "historical" | is.na(ref_scenario)))
      plot_title <- paste0("Summary ", met)
    }

    # gg tile plot using data along dimensions as given in function call
    x_plot <- "scenario"
    y_plot <- "variable"

    p <- ggplot(d, aes(x = .data[[x_plot,]],
                       y = .data[[y_plot,]],
                       fill = score)) +
      geom_tile(color="white", linewidth=0.0) +
      scale_fill_gradient2(low="#008450", high="#B81D13", guide="colorbar") +
      labs(x = NULL, y = NULL, title = plot_title) +
      theme_tufte(base_family = "Helvetica") +  # creates warnings
      theme(axis.ticks = element_blank()) +
      theme(axis.text = element_text(size = 10)) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      theme(strip.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      coord_equal() +
      theme(legend.position = "none")

    # create small gap to "World" data by creating white outline
    if("World" %in% d$region) {
      p <- p + geom_vline(xintercept = 1.5, linewidth = 0.8, color = "white")
    }
    fig <- ggplotly(p, tooltip = "text")

    # if only one variable if passed to function
  } else {

    # prepare data slice which will be plotted
    d <- df %>%
      filter(.data$variable == var,
             .data$metric == met)

    if (historical) {
      d <- filter(d, ref_scenario == "historical")
      plot_title <- paste0(var, " [", d$unit[1], "] - ", met, " (historical)")
    } else {
      d <- filter(d, (ref_scenario != "historical" | is.na(ref_scenario)))
      plot_title <- paste0(var, " [", d$unit[1], "] - ", met)
    }

    # warn if no data is found for combination of var, cat and met
    # TODO: fix for case without category
    # if (nrow(d) == 0) {
    #   data$cm <- paste(metric, sep = "-")
    #   warning(
    #     paste0(
    #       "No data found for variable in this category and metric.\n
    #       variable ", var ," is available for the following category-metric
    #       combinations: ", unique(data[data$variable == var, "cm"])
    #       )
    #     )
    # }

    d$period <- as.character(d$period)
    colors <- c(green  = "#008450",
                yellow = "#EFB700",
                red    = "#B81D13",
                grey   = "#808080")


    # gg tile plot using data along dimensions as given in function call
    p <- ggplot(d, aes(x = .data[[x_plot,]],
                       y = .data[[y_plot,]],
                       fill = check,
                       text = text)) +
      geom_tile(color="white", linewidth=0.0) +
      scale_fill_manual(values = colors, breaks = colors) +
      facet_grid(.data[[y_facet,]] ~ .data[[x_facet,]]) +
      labs(x = NULL, y = NULL, title = plot_title) +
      theme_tufte(base_family = "Helvetica") +  # creates warnings
      theme(axis.ticks = element_blank()) +
      theme(axis.text = element_text(size = 9)) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      theme(strip.text.x = element_text(angle = 0, vjust = 0.5, hjust=1)) +
      theme(strip.text.y = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      coord_equal() +
      theme(legend.position = "none")

    # create small gap to "World" data by creating white outline
    if("World" %in% d$region) {
      p <- p + geom_vline(xintercept = 1.5, linewidth = 0.8, color = "white")
    }
    fig <- ggplotly(p, tooltip = "text")
  }

  if (interactive) {
    return(fig)
  } else {
    return(p)
  }

}
