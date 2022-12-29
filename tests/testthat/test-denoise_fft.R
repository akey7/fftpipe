test_that("denoise_fft() expects a data.frame of an FFT as its first argument", {
  expect_error(fftpipe::denoise_fft(psd_thresh = 0.1))
})

test_that("denoise_fft() denoises a waveform", {
  set.seed(123)

  wv <- waveform(duration_s = 1.0, sr = 100)

  wv_clean <- wv %>%
    cos_sum(1) %>%
    length_norm()

  wv_denoise <- wv %>%
    cos_sum(1) %>%
    white_noise(sd = 1e-5) %>%
    length_norm() %>%
    compute_fft() %>%
    denoise_fft(psd_thresh = 0.1) %>%
    inverse_fft(wv, .) %>%
    length_norm()

  expect_equal(wv_denoise$.value, wv_clean$.value, tolerance = 0.01)
})
