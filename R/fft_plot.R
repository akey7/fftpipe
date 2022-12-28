utils::globalVariables(c(".idx", ".value", ".psd", ".re", ".im", "component", "value", "hz"))

#' fft_plot()
#'
#' Plots the results of an FFT. It converts the x-axis scale to hz during the plotting.
#'
#' @param incoming Required. A data.frame with the incoming waveform.
#' @param show Optional. Either "psd" (which shows the power spectral density of the FFT up to half of its values to display frequencies up to the Nyquist limit) or "everything" (which shows modulus, real, and imaginary components of all FFT values.)
#' @param ... Optional. If specified, passed to the ggplot2 labs() function.
#'
#' @return ggplot() object of the FFT
#' @export
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_segment
#' @importFrom ggplot2 labs
#' @importFrom dplyr transmute
#' @importFrom dplyr filter
#' @importFrom tidyr pivot_longer
#'
#' @examples
#' waveform(duration_s = 1.0, sr = 10) %>%
#'   cos_sum(freqs = c(2.0, 3.0), amplitudes = c(0.5, 1.0)) %>%
#'   length_norm() %>%
#'   compute_fft() %>%
#'   fft_plot(show = "everything")
#'
#' waveform(duration_s = 1.0, sr = 10) %>%
#'  cos_sum(freqs = c(2.0, 3.0), amplitudes = c(0.5, 1.0)) %>%
#'  length_norm() %>%
#'  compute_fft() %>%
#'  fft_plot(show = "psd")
fft_plot <- function(incoming, show = "psd", ...) {
  stopifnot("incoming must be a data.frame with an FFT." = is.data.frame(incoming))
  stopifnot("show argument must be \"everything\" or \"half\"." = show %in% c("psd", "everything"))

  plot_data <- incoming %>%
    dplyr::transmute(
      hz = .idx - 1,
      .psd,
      .re = Re(.value),
      .im = Im(.value),
      .mod = Mod(.value)
    ) %>%
    tidyr::pivot_longer(-hz, names_to = "component", values_to = "value")

  if (show == "everything") {
    result <- ggplot2::ggplot(plot_data, aes(x = hz, y = 0, xend = hz, yend = value)) +
      ggplot2::geom_segment() +
      ggplot2::facet_wrap(~ component, nrow = 3)
  }
  else {
    result <- plot_data %>%
      dplyr::filter(component == ".psd", hz <= max(hz) / 2) %>%
      ggplot2::ggplot(aes(x = hz, y = 0, xend = hz, yend = value)) +
      ggplot2::geom_segment()
  }

  result +
    labs(...)
}
