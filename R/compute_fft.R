#' compute_fft()
#'
#' Computes the FFT of the given waveform.
#'
#' @param incoming data.frame containing the incoming waveform for the FFT.
#'
#' @return A data.frame with the following columns: .values with the values from the FFT, .idx with the indexes for these values (this makes it easier to plot with ggplot later). .psd contains the power spectral density of the fourier coefficient.
#' @export
#' @importFrom stats fft
#'
#' @references
#' Brunton, S. L. & Kutz, J. N. Data-Driven Science and Engineering: Machine Learning, Dynamical Systems, and Control. (Cambridge University Press, 2022). doi:10.1017/9781009089517. Pg 68.
#'
#'
#' @examples
#' waveform(duration_s = 1.0, sr = 10) %>%
#'   cos_sum(freqs = c(2.0, 3.0), amplitudes = c(0.5, 1.0)) %>%
#'   length_norm() %>%
#'   compute_fft()
compute_fft <- function(incoming) {
  stopifnot("incoming must be a data.frame with a waveform." = is.data.frame(incoming))

  .value <- stats::fft(incoming$.value)
  .psd <- Re(.value * Conj(.value))

  data.frame(
    .idx = seq_along(.value),
    .value = .value,
    .psd = .psd
  )
}
