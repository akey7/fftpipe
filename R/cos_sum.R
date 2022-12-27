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
    waveform_matrix[i + 1,] <- amplitude * cos(2 * pi * freq * incoming$.sample / sr)
  }

  dplyr::mutate(incoming, .value = colSums(waveform_matrix))
}
