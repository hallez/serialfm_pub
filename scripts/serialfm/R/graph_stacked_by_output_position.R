#' Create stacked bar graphs
#'
#' graph_stacked_by_output_position takes a dataframe (data),to create a graph of responses stacked by type and split into bars by sublist across output positions.
#' To get graphs to print, must wrap function call in print().
#' @param data: dataframe of summarized data

graph_stacked_by_output_position <- function(data){
  ggplot2::ggplot(data = data,
                  ggplot2::aes(x = response_type,
                               y = mean_value,
                               fill = sublist,
                               color = response_type)) +
  # use `stat = "identity"` b/c actually want to plot the *value* of `mean_value`
  ggplot2::geom_bar(stat="identity",
                    position = "stack") +
  # `switch = "x"` puts the facet labels at the bottom of the graph instead of the top
  ggplot2::facet_grid( ~ output_position, switch = "x") +
  ggplot2::scale_color_manual(values = c("blue", "red")) +
  ggplot2::scale_fill_manual(values = c("#cccccc", "#969696", "#525252")) +
  ggplot2::ggtitle("Responses by output position") +
  ggplot2::xlab("Output position") +
  ggplot2::ylab("Proportion of responses") +
  ggplot2::ylim(0,1) +
  ggplot2::theme(axis.text.x = ggplot2::element_blank(), axis.title.x = ggplot2::element_text(size = 25),
                   strip.text.x = ggplot2::element_text(size = 20),
                   axis.text.y = ggplot2::element_text(size = 20), axis.title.y=ggplot2::element_text(size = 25, vjust = 1),
                   legend.title = ggplot2::element_text(size = 15), legend.text = ggplot2::element_text(size = 15),
                   plot.title = ggplot2::element_text(size=30, vjust = 2),
                   panel.background = ggplot2::element_rect(fill = "white"))
}

graph_stacked_by_output_position_no_sublist <- function(data){
  ggplot2::ggplot(data = data,
                  ggplot2::aes(x = response_type,
                               y = mean_value,
                               color = response_type)) +
    # use `stat = "identity"` b/c actually want to plot the *value* of `mean_value`
    ggplot2::geom_bar(stat="identity",
                      position = "stack") +
    ggplot2::facet_grid( ~ output_position, switch = "x") +
    ggplot2::scale_color_manual(values = c("green2")) +
    ggplot2::scale_fill_manual(values = c("#cccccc", "#969696", "#525252")) +
    ggplot2::ggtitle("Misc responses by output position") +
    ggplot2::xlab("Output position") +
    ggplot2::ylab("Proportion of responses") +
    ggplot2::ylim(0,0.15) +
    ggplot2::theme(axis.text.x = ggplot2::element_blank(), axis.title.x = ggplot2::element_text(size = 25),
                   strip.text.x = ggplot2::element_text(size = 20),
                   axis.text.y = ggplot2::element_text(size = 20), axis.title.y=ggplot2::element_text(size = 25, vjust = 1),
                   legend.title = ggplot2::element_text(size = 15), legend.text = ggplot2::element_text(size = 15),
                   plot.title = ggplot2::element_text(size=30, vjust = 2),
                   panel.background = ggplot2::element_rect(fill = "white"))
}

graph_stacked_by_output_position_w_misc <- function(data){
  ggplot2::ggplot(data = data,
                  ggplot2::aes(x = response_type,
                               y = mean_value,
                               fill = sublist,
                               color = response_type)) +
    # use `stat = "identity"` b/c actually want to plot the *value* of `mean_value`
    ggplot2::geom_bar(stat="identity",
                      position = "stack") +
    ggplot2::facet_grid( ~ output_position, switch = "x") +
    ggplot2::scale_color_manual(values = c("blue", "green2", "red")) +
    ggplot2::scale_fill_manual(values = c("#cccccc", "#969696", "#525252")) +
    ggplot2::ggtitle("Responses by output position") +
    ggplot2::xlab("Output position") +
    ggplot2::ylab("Proportion of responses") +
    ggplot2::ylim(0,1) +
    ggplot2::theme(axis.text.x = ggplot2::element_blank(), axis.title.x = ggplot2::element_text(size = 25),
                   strip.text.x = ggplot2::element_text(size = 20),
                   axis.text.y = ggplot2::element_text(size = 20), axis.title.y=ggplot2::element_text(size = 25, vjust = 1),
                   legend.title = ggplot2::element_text(size = 15), legend.text = ggplot2::element_text(size = 15),
                   plot.title = ggplot2::element_text(size=30, vjust = 2),
                   panel.background = ggplot2::element_rect(fill = "white"))
  }
