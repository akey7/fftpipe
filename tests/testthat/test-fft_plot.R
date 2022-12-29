test_that("fft_plot() expects incoming to be a data.frame", {
  expect_error(fft_plot(show = "psd"))
})

test_that("fft_plot() expects show to be \"psd\" or \"everything\".", {
  expect_error(fft_plot(data.frame(), show = "nothing"))
})
