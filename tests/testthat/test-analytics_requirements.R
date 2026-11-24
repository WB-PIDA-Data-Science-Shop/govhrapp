# Tests for the column checks run before the analytics app starts.

personnel <- data.frame(
  ref_date = as.Date("2020-01-01"),
  personnel_id = "p1",
  employment_status = "active",
  birth_date = as.Date("1980-01-01"),
  gender = "F",
  educat7 = "Tertiary"
)

contracts <- data.frame(
  ref_date = as.Date("2020-01-01"),
  personnel_id = "p1",
  employment_status = "active",
  birth_date = as.Date("1980-01-01"),
  contract_type = "permanent",
  gross_salary_lcu = 100
)

test_that("check_analytics_data accepts data with the required columns", {
  expect_no_error(check_analytics_data(personnel, contracts))
})

test_that("check_required_columns names every missing column", {
  incomplete <- personnel[, c("ref_date", "personnel_id")]

  expect_error(
    check_required_columns(
      incomplete,
      analytics_required_cols[["workforce"]],
      arg = "workforce_data"
    ),
    "`workforce_data` is missing required columns: `employment_status`, `birth_date`, `gender`, `educat7`",
    fixed = TRUE
  )
})

test_that("check_analytics_data requires contract_type only for transitions", {
  no_contract_type <- contracts[, setdiff(names(contracts), "contract_type")]

  expect_error(
    check_analytics_data(personnel, no_contract_type),
    "`wagebill_data` is missing required columns: `contract_type`",
    fixed = TRUE
  )
  expect_no_error(
    check_analytics_data(wagebill_data = no_contract_type, transition = FALSE)
  )
})

test_that("run_govhrapp_analytics stops before building the cache", {
  # building the cache from incomplete data would fail inside govhr instead
  expect_error(
    run_govhrapp_analytics(personnel[, c("ref_date", "personnel_id")], contracts),
    "`workforce_data` is missing required columns",
    fixed = TRUE
  )
})

test_that("check_required_columns reads a database table's columns", {
  skip_if_not_installed("dbplyr")
  skip_if_not_installed("duckdb")

  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  remote <- dplyr::copy_to(con, personnel, "personnel")

  expect_no_error(
    check_required_columns(
      remote,
      analytics_required_cols[["workforce"]],
      arg = "workforce_data"
    )
  )
})
