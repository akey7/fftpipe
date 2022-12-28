test_that("length_norm() expects a dataframe", {
  expect_error(length_norm(5.0))
})

test_that("length_norm() divides value vector by its length", {
  wv <- waveform(duration_s = 1.0, sr = 10) %>%
    cos_sum(freqs = 2.0) %>%
    length_norm()

  actual <- wv$.value
  expected <- c(0.0309017, -0.0809017, -0.0809017, 0.0309017, 0.1000000, 0.0309017, -0.0809017, -0.0809017, 0.0309017, 0.1000000)

  expect_equal(expected, actual, tolerance = 1e-5)
})
