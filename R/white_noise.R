utils::globalVariables(c(".value"))

#' white_noise()
#'
#' Adds Gaussian white noise into a waveform. You should set the random seed before invoking this function if you want consistent results.
#'
#' @param incoming Required. The incoming waveform to which Gaussian will be added to.
#' @param mean Optional. The mean of the Gaussian distribution of the noise. Defaults to 0.
#' @param sd Optional. The standard deviation of the Gaussian distribution of the noise. Defaults to 1.
#'
#' @return data.frame of the waveform with noise added.
#' @export
#' @importFrom stats rnorm
#' @importFrom dplyr mutate
#'
#' @examples
#' waveform() %>% white_noise() %>% waveform_plot()
white_noise <- function(incoming, mean = 0, sd = 1) {
  stopifnot("incoming must be a data.frame specifying a waveform." = is.data.frame(incoming))

  waveform_matrix <- matrix(nrow = 2, ncol = length(incoming$.value))
  waveform_matrix[1,] <- incoming$.value
  waveform_matrix[2,] <- stats::rnorm(length(incoming$.value))

  dplyr::mutate(incoming, .value = colSums(waveform_matrix))
}
