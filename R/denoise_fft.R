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
#' @examples
denoise_fft <- function(incoming_fft, psd_thresh) {
  stopifnot("incoming_fft must be a data.frame that contains an FFT." = is.data.frame(incoming_fft))

  new_psd <- incoming_fft$.psd
  new_value <- incoming_fft$.value
  mask <- incoming_fft$.psd < psd_thresh
  new_psd[mask] <- 0.0
  new_value[mask] <- 0.0

  data.frame(
    .idx = incoming_fft$.idx,
    .value = new_value,
    .psd = new_psd
  )
}
