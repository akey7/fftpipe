test_that("compute_fft() expects a data.frame", {
  expect_error(compute_fft(1.0))
})

test_that("compute_fft() computes an FFT of the sum of two cosines", {
  wv <- waveform(duration_s = 1.0, sr = 10) %>%
    cos_sum(freqs = c(2.0, 3.0), amplitudes = c(0.5, 1.0)) %>%
    length_norm()

  actual <- compute_fft(wv)$.value

  expected <- c(
    -4.857226e-17+0.000000e+00i, 0.000000e+00-8.326673e-17i,
    7.725425e-02+2.377641e-01i, -1.545085e-01+4.755283e-01i,
    -5.551115e-17+5.551115e-17i, -8.326673e-17+2.775558e-17i,
    -5.551115e-17-5.551115e-17i, -1.545085e-01-4.755283e-01i,
    7.725425e-02-2.377641e-01i, 0.000000e+00+5.551115e-17i
  )

  expect_equal(expected, actual, tolerance = 1e-3)
})
