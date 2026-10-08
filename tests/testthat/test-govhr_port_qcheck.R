# Tests for the quality-check functions ported ahead of govhr. The data.frame
# and tbl_dbi methods must agree.

# x is missing in 2 of 6 records, y in 2; on 2020-01-01 unit B has neither
coverage_panel <- data.frame(
  ref_date = as.Date(c(
    "2020-01-01", "2020-01-01", "2020-01-01",
    "2021-01-01", "2021-01-01", "2021-01-01"
  )),
  unit = c("A", "A", "B", "A", "B", "B"),
  x = c(1, 2, NA, 3, 4, NA),
  y = c(NA, 1, NA, 1, 1, 1)
)

test_that("compute_coverage gives the share of non-missing values per column", {
  result <- compute_coverage(coverage_panel)

  expect_equal(names(result), c("variable", "coverage"))
  expect_equal(result[["variable"]], c("ref_date", "unit", "x", "y"))
  expect_equal(result[["coverage"]], c(100, 100, 4 / 6 * 100, 4 / 6 * 100))
})

test_that("compute_coverage covers every column but the groups", {
  result <- compute_coverage(coverage_panel, group_cols = "unit")

  expect_equal(result[["unit"]], rep(c("A", "B"), each = 3))
  expect_equal(result[["variable"]], rep(c("ref_date", "x", "y"), 2))
  expect_equal(
    result[["coverage"]],
    c(100, 100, 2 / 3 * 100, 100, 1 / 3 * 100, 2 / 3 * 100)
  )
})

test_that("compute_coverage averages the columns of each group and date", {
  result <- compute_coverage(
    coverage_panel,
    group_cols = "unit",
    include_ref_date = TRUE,
    aggregate = TRUE
  )

  expect_equal(names(result), c("ref_date", "unit", "coverage"))
  expect_equal(
    result[["ref_date"]],
    as.Date(c("2020-01-01", "2020-01-01", "2021-01-01", "2021-01-01"))
  )
  expect_equal(result[["unit"]], c("A", "B", "A", "B"))
  # mean of x and y coverage: (100 + 50) / 2, (0 + 0) / 2, ...
  expect_equal(result[["coverage"]], c(75, 0, 100, 75))
})

test_that("compute_coverage counts ref_date once when also a group", {
  result <- compute_coverage(
    coverage_panel,
    group_cols = "ref_date",
    include_ref_date = TRUE,
    aggregate = TRUE
  )

  expect_equal(names(result), c("ref_date", "coverage"))
  expect_equal(nrow(result), 2L)
})

test_that("compute_coverage gives the same result on a database table", {
  skip_if_not_installed("dbplyr")
  skip_if_not_installed("duckdb")

  # a missing unit is kept as its own group
  hr <- rbind(
    coverage_panel,
    data.frame(ref_date = as.Date("2021-01-01"), unit = NA, x = 5, y = NA)
  )

  con <- DBI::dbConnect(duckdb::duckdb())
  remote <- dplyr::copy_to(con, hr, "coverage_panel")

  for (group_cols in list(NULL, "unit")) {
    for (include_ref_date in c(FALSE, TRUE)) {
      for (aggregate in c(FALSE, TRUE)) {
        sort_keys <- c(
          if (include_ref_date) "ref_date",
          group_cols,
          if (!aggregate) "variable"
        )

        expected <- compute_coverage(
          hr,
          group_cols = group_cols,
          include_ref_date = include_ref_date,
          aggregate = aggregate
        ) |>
          dplyr::arrange(dplyr::across(dplyr::all_of(sort_keys))) |>
          as.data.frame()

        result <- compute_coverage(
          remote,
          group_cols = group_cols,
          include_ref_date = include_ref_date,
          aggregate = aggregate
        ) |>
          dplyr::collect() |>
          dplyr::arrange(dplyr::across(dplyr::all_of(sort_keys))) |>
          as.data.frame()

        expect_equal(result, expected, ignore_attr = TRUE)
      }
    }
  }

  DBI::dbDisconnect(con, shutdown = TRUE)
})

test_that("plot_coverage_trend draws the computed coverage as given", {
  coverage <- compute_coverage(
    coverage_panel,
    include_ref_date = TRUE,
    aggregate = TRUE
  )

  plotted <- ggplot2::ggplot_build(plot_coverage_trend(coverage))[["data"]][[1]]

  # govhr's version recomputed coverage from this summary, drawing 100%
  expect_equal(plotted[["y"]], coverage[["coverage"]])
})

test_that("plot_coverage_trend indexes each group to its first date", {
  coverage <- compute_coverage(
    coverage_panel,
    group_cols = "unit",
    include_ref_date = TRUE,
    aggregate = TRUE
  )

  plot <- plot_coverage_trend(coverage, group_col = "unit", toggle_growth = TRUE)
  plotted <- ggplot2::ggplot_build(plot)[["data"]][[1]]

  # unit A goes from 75 to 100; unit B has no coverage on its first date, so
  # its index is undefined
  unit_a <- plotted[plotted[["group"]] == 1, ]
  expect_equal(unit_a[["y"]], c(100, 100 / 75 * 100))
})

test_that("plot_coverage_trend draws from a database table", {
  skip_if_not_installed("dbplyr")
  skip_if_not_installed("duckdb")

  con <- DBI::dbConnect(duckdb::duckdb())
  remote <- dplyr::copy_to(con, coverage_panel, "coverage_panel")

  coverage <- compute_coverage(
    remote,
    group_cols = "unit",
    include_ref_date = TRUE,
    aggregate = TRUE
  )

  expect_s3_class(plot_coverage_trend(coverage, group_col = "unit"), "ggplot")
  expect_no_error(
    ggplot2::ggplot_build(plot_coverage_trend(coverage, group_col = "unit"))
  )

  DBI::dbDisconnect(con, shutdown = TRUE)
})
