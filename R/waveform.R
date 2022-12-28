#' waveform
#'
#' Builds the skeleton of a waveform for a given duration and sample rate filled with zero values.
#'
#' @param duration_s Waveform duration in seconds.
#' @param sr Waveform sample rate in samples / second.
#'
#' @return Returns a data.frame with the following columns: .sample, the integer sample of the row; .sec, the double second of that row; and .value, the value for that row.
#' @export
#'
#' @examples
#' waveform(duration_s = 1.0, sr = 200)
waveform <- function(duration_s = 1.0, sr = 200) {
  .sample <- seq(duration_s * sr)
  .sec <- seq(0.0, duration_s, by = duration_s / (length(.sample) - 1))
  .value <- rep(0.0, length(.sample))

  data.frame(
    .sample = .sample,
    .sec = .sec,
    .value = .value
  )
}
