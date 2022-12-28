test_that("waveform() creates waveform correctly", {
  wv <- waveform(duration_s = 1.0, sr = 10)

  expect_true(identical(wv$.sample, seq(10)))
  expect_true(identical(wv$.sec, seq(0.0, 1.0, by = 1.0 / 9)))
  expect_true(identical(wv$.value, rep(0.0, 10)))
})
