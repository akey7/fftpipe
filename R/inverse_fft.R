#' inverse_fft()
#'
#' Runs the inverse FFT specified by incoming_fft, applies the result to incoming_waveform, and returns the result. Unlilke the functions that sum the incoming waveform, this function overwrites the incoming waveform.
#'
#' @param incoming_waveform Required. A data.frame specifying an incoming waveform. Hint: this incoming waveform can be created by the waveform() function.
#' @param incoming_fft Required. The incoming FFT as computed by compute_fft()
#'
#' @return A data.frame with the result of the inverse FFT.
#' @export
#' @importFrom stats fft
#'
#' @examples
#' wv <- waveform(duration_s = 1.0, sr = 200)
#' wv_fft <- wv %>%
#'   cos_sum(freqs = c(2.0, 3.0), amplitudes = c(0.5, 1.0)) %>%
#'   length_norm() %>%
#'   compute_fft()
#' wv %>%
#'   inverse_fft(wv_fft) %>%
#'   waveform_plot()
inverse_fft <- function(incoming_waveform, incoming_fft) {
  stopifnot("incoming_waveform must be a data.frame specifying a waveform." = is.data.frame(incoming_waveform))
  stopifnot("incoming_fft must be a data.frame specifying an FFT." = is.data.frame(incoming_fft))

  new_value <- stats::fft(incoming_fft$.value, inverse = TRUE)

  data.frame(
    .idx = incoming_waveform$.sample,
    .sec = incoming_waveform$.sec,
    .value = Re(new_value)
  )
}
