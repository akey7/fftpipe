test_that("exp_decay() expects a data.frame as the first argument.", {
  expect_error(exp_decay(0.5))
})

test_that("exp_decay() decays a dc offset correctly.", {
  wv <- waveform(duration_s = 1.0, sr = 10)
  wv$.value <- 1
  actual <- exp_decay(wv, decay_tau = 5)$.value
  expected <- c(0.9801987, 0.9607894, 0.9417645, 0.9231163, 0.9048374, 0.8869204, 0.8693582, 0.8521438, 0.8352702, 0.8187308)

  expect_equal(round(actual, 5), round(expected, 5))
})
