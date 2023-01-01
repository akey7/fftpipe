#' denoise_fft()
#'
#' Applies a denoise algorithm to an FFT to uncover a signal from noise. It applies a threshold to the power spectral density (PSD) to distinguish signal from noise. Fourier coefficients that are below the PSD threshold are reduced to zeros, while coefficients that are above the PSD threshold are maintained.
#'
#' @param incoming_fft Required. The data.frame with an FFT to denoise.
#' @param psd_thresh Required. The PSD threshold under which coefficients are dropped to zero as described above.
#'
#' @return data.frame with a denoised FFT
#' @export
#'
#' @references
#' Brunton, S. L. & Kutz, J. N. Data-Driven Science and Engineering: Machine Learning, Dynamical Systems, and Control. (Cambridge University Press, 2022). doi:10.1017/9781009089517. Pg 68.
#'
#' @examples
#' wv <- waveform(duration_s = 1.0, sr = 100)
#' wv_denoise <- wv %>%
#'  cos_sum(1) %>%
#'  white_noise(sd = 1e-5) %>%
#'  length_norm() %>%
#'  compute_fft() %>%
#'  denoise_fft(psd_thresh = 0.1) %>%
#'  inverse_fft(wv, .) %>%
#'  length_norm()
denoise_fft <- function(incoming_fft, psd_thresh) {
  stopifnot("incoming_fft must be a data.frame that contains an FFT." = is.data.frame(incoming_fft))

  new_psd <- incoming_fft$.psd
  new_value <- incoming_fft$.value
  mask <- incoming_fft$.psd < psd_thresh
  new_psd[mask] <- 0.0
  new_value[mask] <- complex(real = 0.0, imaginary = 0.0)

  data.frame(
    .idx = incoming_fft$.idx,
    .value = new_value,
    .psd = new_psd
  )
}
