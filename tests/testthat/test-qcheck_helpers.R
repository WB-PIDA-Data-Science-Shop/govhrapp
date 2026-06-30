test_that("compute_coverage works correctly", {
  data <- data.frame(
    a = c(1, 2, NA, 4),
    b = c(NA, 2, 3, 4)
  )

  expect_equal(
    compute_global_coverage(data),
    75
  )
})
