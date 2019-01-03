#' Create bar graphs split by sublist and response type
#'
#' graph_split_by_sublist takes a dataframe (data),to create a graph of responses split by response type by sublist.
#' To get graphs to print, must wrap function call in print().
#' @param data: dataframe of summarized data

graph_split_by_sublist <- function(data){
  # alpha vs. fill based on
  # http://stackoverflow.com/questions/11170656/r-ggplot-barplot-fill-based-on-two-separate-variables
  ggplot2::ggplot(data = data,
                  ggplot2::aes(x = sublist,
                               y = mean_value,
                               group = response_type,
                               fill = response_type)) +
    ggplot2::geom_bar(stat = "identity",
                      position = "dodge") +
    ggplot2::scale_fill_manual(values = c("#bdc9e1", "#74a9cf", "#0570b0")) +
    ggplot2::ggtitle("Responses by sublist") +
    ggplot2::xlab("Sublist") +
    ggplot2::ylab("Proportion of responses") +
    # to accommodate error bars, set min to be < 0
    ggplot2::ylim(-0.001,0.37) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size=20), axis.title.x = ggplot2::element_text(size = 25),
                   axis.text.y = ggplot2::element_text(size = 20), axis.title.y=ggplot2::element_text(size = 25, vjust = 1),
                   legend.title = ggplot2::element_text(size = 15), legend.text = ggplot2::element_text(size = 15),
                   plot.title = ggplot2::element_text(size=30, vjust = 2),
                   panel.background = ggplot2::element_rect(fill = "white"),
                   panel.grid.major.y = ggplot2::element_line(color = "lightgray", linetype = "solid"), panel.grid.minor.y = ggplot2::element_line(color = "lightgray", linetype = "solid"),
                   panel.grid.major.x = ggplot2::element_line(color = "lightgray", linetype = "solid")) +
    ggplot2::geom_errorbar(ggplot2::aes(ymax = upper,
                                        ymin = lower),
                           width = 0.25,
                           # based on: http://docs.ggplot2.org/0.9.3.1/geom_errorbar.html
                           position = ggplot2::position_dodge(width = 0.9))
}
