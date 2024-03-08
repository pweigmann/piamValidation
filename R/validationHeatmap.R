#' @importFrom dplyr filter select mutate %>%
#' @import ggplot2
#' @importFrom ggthemes theme_tufte
#' @importFrom plotly ggplotly
#' @export

# takes the output of "validateScenarios()" and plots heatmaps per variable
validationHeatmap <- function(df, var, cat, met, interactive = T, compareModels = T) {

  # possible extension: when giving multiple vars, plot as facets in same row

  # prepare data slice
  d <- df %>%
    filter(variable == var,
           category == cat,
           metric == met)

  # warn if no data is found for combination of var, cat and met
  if (nrow(d) == 0) {
    df$cm <- paste(category, metric, sep = "-")
    warning(
      paste0(
        "No data found for variable in this category and metric.\n
        variable ", var ," is available for the following category-metric
        combinations: ", unique(df[df$variable == var, "cm"])
        )
      )
  }

  d$period <- as.character(d$period)
  colors <- c(green  = "#008450",
              yellow = "#EFB700",
              red    = "#B81D13",
              grey   = "#808080")


  # classic ggplot, with text in aes
  p <- ggplot(d, aes(x = region, y = period, fill=check, text=text)) +
    geom_tile(color="white", linewidth=0.0) +
    scale_fill_manual(values = colors, breaks = colors) +
    facet_grid(model~scenario)

  # make it beautiful
  # from https://www.r-bloggers.com/2016/02/making-faceted-heatmaps-with-ggplot2
  p <- p + labs(x = NULL, y = NULL, title = paste0(var,
                                                   " [", d$unit[1], "] - ",
                                                   cat, "/", met))
  p <- p + theme_tufte(base_family = "Helvetica")  # creates warnings
  p <- p + theme(axis.ticks = element_blank())
  p <- p + theme(axis.text = element_text(size = 7))
  p <- p + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  p <- p + coord_equal()
  p <- p + theme(legend.position = "none")


  # p + theme(panel.spacing = unit(2, "lines"))

  # not great, only works with "World" being the first region
  if("World" %in% d$region) {
    p <- p + geom_vline(xintercept = 1.5, linewidth = 0.8, color = "white")
  }
  fig <- ggplotly(p, tooltip = "text")

  # improve plotly layout, kinda works but very manual
  # TODO: can this be extended to a general, useful function?
  #fig <- fig %>% subplot(heights = 0.3) %>%
  #   layout(title = list(y=0.64))

  if (interactive) {
    return(fig)
  } else {
    return(p)
  }

}
