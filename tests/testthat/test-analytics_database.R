# The analytics helpers aggregate through govhr's database-aware compute
# functions, so on a duckdb table they must give the same result as on a data
# frame without the caller collecting the raw data first.

# p2 retires in 2021; p4 is a pensioner throughout
hr_panel <- utils::read.csv(
  text = "
personnel_id,ref_date,employment_status,gender,gross_salary_lcu
p1,2020-01-01,active,F,100
p2,2020-01-01,active,M,200
p3,2020-01-01,active,F,150
p4,2020-01-01,pensioner,M,80
p1,2021-01-01,active,F,110
p3,2021-01-01,active,F,160
p5,2021-01-01,active,M,300
p2,2021-01-01,pensioner,M,90
p4,2021-01-01,pensioner,M,85
p1,2022-01-01,active,F,120
p3,2022-01-01,active,F,170
p5,2022-01-01,active,M,310
p2,2022-01-01,pensioner,M,95
p4,2022-01-01,pensioner,M,90
",
  colClasses = c(ref_date = "Date")
)

test_that("filter_data filters a database table by subgroup and date range", {
  skip_if_not_installed("dbplyr")
  skip_if_not_installed("duckdb")

  con <- DBI::dbConnect(duckdb::duckdb())
  remote <- dplyr::copy_to(con, hr_panel, "hr_panel")

  date_range <- as.Date(c("2021-01-01", "2022-01-01"))

  result <- filter_data(remote, "gender", "F", date_range) |>
    dplyr::collect() |>
    dplyr::arrange(.data[["ref_date"]], .data[["personnel_id"]])

  expected <- filter_data(hr_panel, "gender", "F", date_range) |>
    dplyr::arrange(.data[["ref_date"]], .data[["personnel_id"]])

  expect_equal(result, expected, ignore_attr = TRUE)
  expect_equal(nrow(result), 4L)

  DBI::dbDisconnect(con, shutdown = TRUE)
})

test_that("summarise_wagebill_box gives the same totals on a database table", {
  skip_if_not_installed("dbplyr")
  skip_if_not_installed("duckdb")

  con <- DBI::dbConnect(duckdb::duckdb())
  remote <- dplyr::copy_to(con, hr_panel, "hr_panel")

  for (measure_type in c("total_wagebill", "total_pension_liabilities")) {
    expect_equal(
      summarise_wagebill_box(remote, measure_type),
      summarise_wagebill_box(hr_panel, measure_type)
    )
  }

  expect_equal(
    summarise_wagebill_box(remote, "total_wagebill"),
    list(ref_date = as.Date("2022-01-01"), total = 600)
  )

  DBI::dbDisconnect(con, shutdown = TRUE)
})

test_that("summarise_movement_box reads a database table's movements", {
  skip_if_not_installed("dbplyr")
  skip_if_not_installed("duckdb")

  con <- DBI::dbConnect(duckdb::duckdb())
  remote <- dplyr::copy_to(con, hr_panel, "hr_panel")

  for (movement_type in c("hire", "separation", "retirement", "replacement")) {
    expect_equal(
      summarise_movement_box(
        govhr::compute_movement(remote),
        compute_retirement(remote),
        movement_type
      ),
      summarise_movement_box(
        govhr::compute_movement(hr_panel),
        compute_retirement(hr_panel),
        movement_type
      )
    )
  }

  DBI::dbDisconnect(con, shutdown = TRUE)
})
