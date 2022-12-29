test_that("white_noise() expects a data.frame specifying a waveform", {
  expect_error(white_noise(mean = 1, sd = 1))
})

test_that("white_noise() gives predictable Gaussian noise with a seed", {
  set.seed(123)

  wv <- fftpipe::waveform(duration_s = 1.0, sr = 10) %>%
    fftpipe::white_noise()

  actual <- wv$.value
  expected <- c(-0.56047565, -0.23017749, 1.55870831, 0.07050839, 0.12928774, 1.71506499, 0.46091621, -1.26506123, -0.68685285, -0.44566197)

  expect_equal(actual, expected, tolerance = 1e-3)
})
