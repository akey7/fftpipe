test_that("inverse_fft() Requires two data.frames.", {
  expect_error(inverse_fft(c(1, 2, 3)))
})

test_that("inverse_fft Correctly reconstructs an FFT.", {
  wv <- waveform(duration_s = 1.0, sr = 10)

  wv_fft <- wv %>%
    cos_sum(freqs = c(2.0, 3.0), amplitudes = c(0.5, 1.0)) %>%
    length_norm() %>%
    compute_fft()

  inverse_fft_wv <- wv %>%
    inverse_fft(wv_fft)

  actual <- inverse_fft_wv$.value
  expected <- c(-0.1545085, -1.2135255, 0.4045085, 0.4635255, -0.5000000, 0.4635255, 0.4045085, -1.2135255, -0.1545085, 1.5000000)

  expect_equal(expected, actual, tolerance = 1e-3)
})
