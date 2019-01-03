#' Create serial position curves
#'
#' This function takes a dataframe (data),to create serial position curves of proportions.
#' To get graphs to print, must wrap function call in print().
#' @param data: dataframe of summarized data

graph_serial_position_curves <- function(data){
  ggplot2::ggplot(data = data,
                  ggplot2::aes(x = position,
                             y = mean_responses,
                             # to get rid of the `geom_path` error,
                             # http://stackoverflow.com/questions/27082601/ggplot2-line-chart-gives-geom-path-each-group-consist-of-only-one-observation
                             group = 1)) +
  ggplot2::geom_line() +
  ggplot2::geom_point(size = 5) +
  ggplot2::ggtitle("Responses by presentation position") +
  ggplot2::xlab("Presentation position") +
  ggplot2::ylab("Probability of recall") +
  ggplot2::ylim(0, 1) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(size = 20), axis.title.x = ggplot2::element_text(size = 25, vjust = -0.4),
                 strip.text.x = ggplot2::element_text(size = 20),
                 axis.text.y = ggplot2::element_text(size = 20), axis.title.y=ggplot2::element_text(size = 25, vjust = 1),
                 legend.title = ggplot2::element_text(size = 15), legend.text = ggplot2::element_text(size = 15),
                 plot.title = ggplot2::element_text(size=30, vjust = 2),
                 panel.background = ggplot2::element_rect(fill = "white"),
                 panel.grid.major.y = ggplot2::element_line(color = "lightgray", linetype = "solid"), panel.grid.minor.y = ggplot2::element_line(color = "lightgray", linetype = "solid"),
                 panel.grid.major.x = ggplot2::element_line(color = "lightgray", linetype = "solid"))
}
