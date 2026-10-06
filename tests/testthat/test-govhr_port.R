# Tests for the functions ported ahead of govhr. compute_retirement() mirrors
# govhr::compute_movement(), so the data.frame and tbl_dbi methods must agree.

# p1 retires in 2021, pensioner in 2022
# p2 leaves in 2020 without a pension
# p3 stays throughout, with two contracts in 2020
# p4 draws a pension alongside an active contract, so never leaves
# p5 retires in 2020, pensioner in 2021
# p6 retires in 2020, but the pension is only registered in 2022
# p7 leaves in 2020 and returns in 2022 before any pension, so only the 2022
#   exit is a retirement
retirement_panel <- utils::read.csv(
  text = "
personnel_id,ref_date,employment_status,unit
p1,2020-01-01,active,A
p2,2020-01-01,active,A
p3,2020-01-01,active,B
p3,2020-01-01,active,B
p4,2020-01-01,active,B
p5,2020-01-01,active,B
p6,2020-01-01,active,A
p7,2020-01-01,active,A
p1,2021-01-01,active,A
p3,2021-01-01,active,B
p4,2021-01-01,active,B
p4,2021-01-01,pensioner,B
p5,2021-01-01,pensioner,B
p1,2022-01-01,pensioner,A
p3,2022-01-01,active,B
p4,2022-01-01,active,B
p6,2022-01-01,pensioner,A
p7,2022-01-01,active,A
p3,2023-01-01,active,B
p4,2023-01-01,active,B
p7,2023-01-01,pensioner,A
",
  colClasses = c(ref_date = "Date")
)

test_that("compute_retirement counts exits whose next status is pensioner", {
  result <- compute_retirement(retirement_panel)

  expect_equal(
    result$ref_date,
    as.Date(c("2020-01-01", "2021-01-01", "2022-01-01", "2023-01-01"))
  )
  expect_equal(result$headcount, c(7L, 3L, 3L, 2L))
  expect_equal(result$retirements, c(2L, 1L, 1L, NA))
  expect_equal(result$retirement_rate, c(2 / 7, 1 / 3, 1 / 3, NA))
})

test_that("compute_retirement dates a lagged pension registration by the exit", {
  # p3 keeps the 2021 date in the calendar, as in a full panel
  result <- retirement_panel |>
    dplyr::filter(.data[["personnel_id"]] %in% c("p3", "p6")) |>
    compute_retirement()

  expect_equal(result$retirements, c(1L, 0L, 0L, NA))
})

test_that("compute_retirement ignores an exit followed by a return to work", {
  result <- retirement_panel |>
    dplyr::filter(.data[["personnel_id"]] %in% c("p3", "p7")) |>
    compute_retirement()

  expect_equal(result$retirements, c(0L, 0L, 1L, NA))
})

test_that("compute_retirement counts each person in their group on that date", {
  result <- compute_retirement(retirement_panel, group_cols = "unit")

  expect_equal(result$unit, c("A", "B", "A", "B", "A", "B", "B"))
  expect_equal(result$headcount, c(4L, 3L, 1L, 2L, 1L, 2L, 2L))
  expect_equal(result$retirements, c(1L, 1L, 1L, 0L, 1L, 0L, NA))
})

test_that("compute_retirement counts only separations as retirements", {
  retirement <- compute_retirement(retirement_panel)
  movement <- govhr::compute_movement(retirement_panel)

  expect_true(
    all(retirement$retirements <= movement$separations, na.rm = TRUE)
  )
  expect_identical(retirement$headcount, movement$headcount)
})

test_that("compute_retirement rejects ref_date as a group", {
  expect_error(
    compute_retirement(retirement_panel, group_cols = "ref_date"),
    "ref_date"
  )
})

test_that("compute_retirement gives the same result on a database table", {
  skip_if_not_installed("dbplyr")
  skip_if_not_installed("RSQLite")

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:", extended_types = TRUE)
  remote <- dplyr::copy_to(con, retirement_panel, "retirement_panel")

  for (group_cols in list(NULL, "unit")) {
    expected <- compute_retirement(retirement_panel, group_cols = group_cols) |>
      as.data.frame()

    result <- compute_retirement(remote, group_cols = group_cols) |>
      dplyr::collect() |>
      dplyr::mutate(ref_date = as.Date(.data[["ref_date"]])) |>
      dplyr::arrange(dplyr::across(dplyr::all_of(c("ref_date", group_cols)))) |>
      as.data.frame()

    # SQL returns every count as a double
    expect_equal(result, expected, ignore_attr = TRUE, tolerance = 1e-12)
  }

  DBI::dbDisconnect(con)
})
