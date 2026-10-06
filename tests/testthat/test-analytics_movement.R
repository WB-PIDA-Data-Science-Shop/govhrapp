# Tests for the helpers behind the workforce movement and retirement panels,
# the key-indicator boxes and the report's indicator table.

movement <- data.frame(
  ref_date = as.Date(c("2020-01-01", "2021-01-01", "2022-01-01")),
  headcount = c(100L, 110L, 120L),
  hires = c(NA, 20L, 15L),
  separations = c(10L, 5L, NA),
  hire_rate = c(NA, 20 / 110, 15 / 120),
  separation_rate = c(10 / 100, 5 / 110, NA),
  replacement_rate = c(NA, 4, NA)
)

retirement <- data.frame(
  ref_date = as.Date(c("2020-01-01", "2021-01-01", "2022-01-01")),
  headcount = c(100L, 110L, 120L),
  retirements = c(2L, 3L, NA),
  retirement_rate = c(2 / 100, 3 / 110, NA)
)

test_that("movement_measure_col names the compute_movement() columns", {
  expect_identical(movement_measure_col("hire", "count"), "hires")
  expect_identical(movement_measure_col("separation", "rate"), "separation_rate")
  expect_identical(movement_measure_col("retirement", "count"), "retirements")
  # replacement is a ratio whatever the measurement type
  expect_identical(movement_measure_col("replacement", "count"), "replacement_rate")
})

test_that("summarise_movement_box reads the latest date its measure exists for", {
  hire <- summarise_movement_box(movement, retirement, "hire")
  separation <- summarise_movement_box(movement, retirement, "separation")

  expect_equal(hire$ref_date, as.Date("2022-01-01"))
  expect_equal(hire$count, 15L)
  expect_equal(separation$ref_date, as.Date("2021-01-01"))
  expect_equal(separation$rate, 5 / 110)
})

test_that("summarise_movement_box reads retirements from the retirement table", {
  box <- summarise_movement_box(movement, retirement, "retirement")

  expect_equal(box$count, 3L)
  expect_equal(box$rate, 3 / 110)
})

test_that("summarise_movement_box reports replacement as a ratio only", {
  box <- summarise_movement_box(movement, retirement, "replacement")

  expect_true(is.na(box$count))
  expect_equal(box$rate, 4)
})

test_that("format_movement_rate shows rates as percentages", {
  expect_identical(format_movement_rate(0.0314, "hire"), "3.1%")
  expect_identical(format_movement_rate(1.4932, "replacement"), "1.49")
})

test_that("compute_projected_retirement divides by the current headcount", {
  personnel <- data.frame(
    personnel_id = c("a", "b", "c", "a", "b"),
    ref_date = as.Date(c(
      "2020-01-01", "2020-01-01", "2020-01-01", "2021-01-01", "2021-01-01"
    )),
    employment_status = "active",
    # a turns 60 in 2023, b long after the cut-off; c left before the latest
    # date, so is not projected even though they turn 60 in 2022
    birth_date = as.Date(c(
      "1963-06-01", "1990-01-01", "1962-06-01", "1963-06-01", "1990-01-01"
    ))
  )

  result <- compute_projected_retirement(personnel, threshold_age = 60)

  expect_equal(result$ref_date, as.Date("2023-12-31"))
  expect_equal(result$projected_retirements, 1L)
  expect_equal(result$headcount, 2L)
  expect_equal(result$projected_retirement_rate, 0.5)
})
