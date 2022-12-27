#' cos_sum
#' cos_sum() builds a waveform by summing cos() functions together. It mixes the results of the cos() sum with the incoming waveform by a summation. It obeys the sample rate and duration specified by the incoming waveform.
#'
#' @param incoming Required. A data.frame of the incoming waveform.
#' @param freqs Required. A double or vector of doubles specifying frequencies in Hz of component functions. If NULL, cos_sum() assumes amplitudes of 1.0 for all components. Defaults to NULL.
#' @param amplitudes Optional. A double or vector of doubles specifying amplitudes of the component functions. If specified, the length must match the length of the frequencies vector. If NULL, cos_sum() assumes amplitudes of 1.0 for all components. Defaults to NULL.
#' @param phases Optional. A double or vector of doubles specifying phase shifts of the components in radians. If specified, the length must match the length of the freqs vector. If NULL, cos_sum() assumes a phase shift of 0.0 for all components. Defaults to NULL.
#'
#' @return Returns a data.frame in the format of the waveform() function describing the sum of the incoming waveform with the sum of the components specified by this function.
#' @export
#' @importFrom dplyr mutate
#'
#' @examples
#' waveform(duration_s = 1.0, sr = 100) %>%
#'   cos_sum(freqs = c(1.0, 3.0), amplitudes = NULL, phases = NULL)
cos_sum <- function(incoming, freqs, amplitudes = NULL, phases = NULL) {
  stopifnot("incoming waveform must a data.frame." = is.data.frame(incoming))
  stopifnot("freqs must be a vector of doubles or a single double." = is.numeric(freqs))

  if (is.null(amplitudes)) {
    amplitudes <- rep(1.0, length(freqs))
  }

  if (is.null(phases)) {
    phases <- rep(0.0, length(freqs))
  }

  sr <- length(incoming$.sec) / tail(incoming$.sec, n = 1)

  waveform_matrix <- matrix(nrow = length(freqs) + 1, ncol = nrow(incoming))
  waveform_matrix[1, ] <- incoming$.value

  for (i in seq_along(freqs)) {
    freq <- freqs[i]
    amplitude <- amplitudes[i]
    phase <- phases[i]
    waveform_matrix[i + 1,] <- amplitude * cos((2 * pi * freq * incoming$.sample / sr) + phase)
  }

  dplyr::mutate(incoming, .value = colSums(waveform_matrix))
}
