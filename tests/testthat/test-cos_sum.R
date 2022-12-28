test_that("cos_sum() stops when invoked bad arguments.", {
  expect_error(cos_sum(5, 5))
})

test_that("cos_sum() creates the sum of a single cosine.", {
  result <- fftpipe::waveform(duration_s = 1.0, sr = 10) %>%
    cos_sum(1)

  .value <- c(0.809017, 0.309017, -0.309017, -0.809017, -1.000000, -0.809017, -0.309017, 0.309017, 0.809017, 1.000000)
  expect_identical(round(.value, 5), round(result$.value, 5))
})
