#' length_norm()
#'
#' Normalizes a waveform by dividing the vector of values by the length of the vector of values. Used to prepare waveforms for FFT.
#'
#' @param incoming Required. A data.frame with the waveform.
#'
#' @return A data.frame with the normalized waveform.
#' @export
#' @importFrom dplyr mutate
#'
#' @examples
#' waveform(duration_s = 1.0, sr = 100) %>%
#'   cos_sum(freqs = c(1.0, 3.0), amplitudes = NULL, phases = NULL) %>%
#'   length_norm()
length_norm <- function(incoming) {
  stopifnot("incoming must be a data.frame." = is.data.frame(incoming))

  new_value <- incoming$.value / length(incoming$.value)
  dplyr::mutate(incoming, ".value" = new_value)
}
