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

# the latest date is 2021-07-01, so the default 10-year horizon projects the
# year-ends from 2021 to 2030
# a turns 60 in 2022 and holds two contracts in unit A on the latest date
# b turned 60 before the latest date, so is not projected
# c turns 60 in 2025, in unit B
# d left before the latest date and e is a pensioner on it, so neither is
#   projected
# f turns 60 beyond the horizon and has no unit
projection_panel <- utils::read.csv(
  text = "
personnel_id,ref_date,employment_status,unit,birth_date,pay
a,2020-07-01,active,A,1962-03-01,100
d,2020-07-01,active,A,1963-01-01,50
a,2021-07-01,active,A,1962-03-01,100
a,2021-07-01,active,A,1962-03-01,40
b,2021-07-01,active,A,1961-03-01,70
c,2021-07-01,active,B,1965-10-01,200
e,2021-07-01,pensioner,B,1964-01-01,30
f,2021-07-01,active,,1980-01-01,10
",
  colClasses = c(ref_date = "Date", birth_date = "Date"),
  na.strings = ""
)

test_that("project_retirement projects only the current active workforce", {
  result <- project_retirement(projection_panel)

  expect_equal(result$ref_date, as.Date(paste0(2021:2030, "-12-31")))
  # a, b, c and f, with a counted once despite two contracts
  expect_equal(result$headcount, rep(4L, 10))
  expect_equal(result$projected_retirements, c(0, 1, 0, 0, 1, 0, 0, 0, 0, 0))
  expect_equal(result$projected_retirement_rate, result$projected_retirements / 4)
  expect_false("projected_cost" %in% names(result))
})

test_that("project_retirement costs each group's retirees separately", {
  result <- project_retirement(
    projection_panel,
    group_cols = "unit",
    measure_col = "pay"
  )

  # every unit in the current workforce appears in every projected year
  expect_equal(nrow(result), 3L * 10L)

  retiring <- result[result$projected_retirements > 0, ]
  expect_equal(retiring$ref_date, as.Date(c("2022-12-31", "2025-12-31")))
  expect_equal(retiring$unit, c("A", "B"))
  expect_equal(retiring$headcount, c(2L, 1L))
  # both of a's contracts are costed
  expect_equal(retiring$projected_cost, c((100 + 40) * 0.6, 200 * 0.6))
  expect_equal(sum(result$projected_cost), (100 + 40 + 200) * 0.6)
})

test_that("project_retirement stops at the horizon", {
  result <- project_retirement(projection_panel, horizon = 3)

  expect_equal(result$ref_date, as.Date(paste0(2021:2023, "-12-31")))
  expect_equal(result$projected_retirements, c(0, 1, 0))
})

test_that("project_retirement rejects ref_date as a group", {
  expect_error(
    project_retirement(projection_panel, group_cols = "ref_date"),
    "ref_date"
  )
})

test_that("project_retirement gives the same result on a database table", {
  skip_if_not_installed("dbplyr")
  skip_if_not_installed("duckdb")

  con <- DBI::dbConnect(duckdb::duckdb())
  remote <- dplyr::copy_to(con, projection_panel, "projection_panel")

  for (group_cols in list(NULL, "unit")) {
    for (measure_col in list(NULL, "pay")) {
      sort_keys <- c("ref_date", group_cols)

      expected <- project_retirement(
        projection_panel,
        group_cols = group_cols,
        measure_col = measure_col
      ) |>
        dplyr::arrange(dplyr::across(dplyr::all_of(sort_keys))) |>
        as.data.frame()

      result <- project_retirement(
        remote,
        group_cols = group_cols,
        measure_col = measure_col
      ) |>
        dplyr::collect() |>
        dplyr::arrange(dplyr::across(dplyr::all_of(sort_keys))) |>
        as.data.frame()

      # SQL returns every count as a double
      expect_equal(result, expected, ignore_attr = TRUE)
    }
  }

  DBI::dbDisconnect(con, shutdown = TRUE)
})
