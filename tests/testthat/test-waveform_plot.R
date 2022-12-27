test_that("waveform_plot() stops if first argument is not a waveform.", {
  expect_error(waveform_plot("foo"))
})
