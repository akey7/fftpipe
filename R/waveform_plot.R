library(ggplot2)

#' waveform_plot()
#'
#' @param incoming Required. A data.frame with the incoming waveform to plot.
#' @param ... Optional. If specified, extra arguments are passed to the ggplot2 labs() function to place labels on the plot.
#'
#' @return Returns a ggplot object that can be saved or displayed.
#' @export
#'
#' @examples
#' waveform() %>% cos_sum(1) %>% waveform_plot()
waveform_plot <- function(incoming, ...) {
  stopifnot("incoming must be a dataframe of a waveform." = is.data.frame(incoming))

  ggplot(incoming, aes(x = .sec, y = .value)) +
    geom_line() +
    labs(...)
}
