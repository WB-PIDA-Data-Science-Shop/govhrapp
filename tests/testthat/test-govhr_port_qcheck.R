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

test_that("compute_global_coverage gives the share of non-missing cells", {
  # 20 of the 24 cells are present: x and y each miss 2
  expect_equal(compute_global_coverage(coverage_panel), round(20 / 24 * 100, 2))
})

test_that("plot_coverage_bar draws the computed coverage as given", {
  coverage <- compute_coverage(coverage_panel)

  plotted <- ggplot2::ggplot_build(plot_coverage_bar(coverage))[["data"]][[1]]

  expect_setequal(plotted[["x"]], coverage[["coverage"]])
})

test_that("plot_coverage_heatmap draws coverage from 0 to 1", {
  coverage <- compute_coverage(coverage_panel, group_cols = "unit")

  plot <- plot_coverage_heatmap(coverage, group_col = "unit")
  built <- plotly::plotly_build(plot)[["x"]][["data"]][[1]]

  expect_s3_class(plot, "plotly")
  expect_true(all(unlist(built[["z"]]) <= 1, na.rm = TRUE))
})

# a has two records on 2020-01-01; b moves from unit A to B and changes value;
# c changes value
consistency_panel <- data.frame(
  id = c("a", "a", "b", "c", "a", "b", "c"),
  ref_date = as.Date(c(rep("2020-01-01", 4), rep("2021-01-01", 3))),
  unit = c("A", "A", "A", "B", "A", "B", "B"),
  value = c(1, 1, 2, 3, 1, 5, 4)
)

test_that("compute_record_consistency gives the share of single records", {
  # 5 of the 6 identifier-dates have a single record
  overall <- compute_record_consistency(consistency_panel, id_col = "id")
  expect_equal(names(overall), "record_consistency")
  expect_equal(overall[["record_consistency"]], round(5 / 6 * 100, 2))

  by_date <- compute_record_consistency(
    consistency_panel,
    id_col = "id",
    group_cols = "ref_date"
  )
  expect_equal(by_date[["record_consistency"]], c(round(2 / 3 * 100, 2), 100))

  by_unit <- compute_record_consistency(
    consistency_panel,
    id_col = "id",
    group_cols = "unit"
  )
  expect_equal(by_unit[["unit"]], c("A", "B"))
  expect_equal(by_unit[["record_consistency"]], c(round(2 / 3 * 100, 2), 100))
})

test_that("compute_value_consistency gives the share of single values", {
  # only a keeps one value across dates
  overall <- compute_value_consistency(
    consistency_panel,
    id_col = "id",
    value_col = "value"
  )
  expect_equal(overall[["value_consistency"]], round(1 / 3 * 100, 2))

  # within a date, everyone has one value
  by_date <- compute_value_consistency(
    consistency_panel,
    id_col = "id",
    value_col = "value",
    group_cols = "ref_date"
  )
  expect_equal(by_date[["value_consistency"]], c(100, 100))

  # in unit B, c has two values and b one
  by_unit <- compute_value_consistency(
    consistency_panel,
    id_col = "id",
    value_col = "value",
    group_cols = "unit"
  )
  expect_equal(by_unit[["value_consistency"]], c(100, 50))
})

test_that("compute_value_consistency counts a missing value as a value", {
  hr <- data.frame(id = c("a", "a"), value = c(1, NA))

  result <- compute_value_consistency(hr, id_col = "id", value_col = "value")

  expect_equal(result[["value_consistency"]], 0)
})

test_that("compute_global_consistency averages record and value consistency", {
  result <- compute_global_consistency(
    consistency_panel,
    id_col = "id",
    value_cols = "value"
  )

  expect_equal(result, round(mean(c(5 / 6, 1 / 3)) * 100, 2))
})

test_that("plot_consistency_trend draws the chosen consistency", {
  consistency <- compute_value_consistency(
    consistency_panel,
    id_col = "id",
    value_col = "value",
    group_cols = c("unit", "ref_date")
  )

  plot <- plot_consistency_trend(consistency, group_col = "unit", type_plot = "value")
  plotted <- ggplot2::ggplot_build(plot)[["data"]][[1]]

  expect_setequal(plotted[["y"]], consistency[["value_consistency"]])
  expect_error(plot_consistency_trend(consistency, type_plot = "other"))
})

test_that("plot_consistency_heatmap draws stacked value consistency", {
  consistency <- c("unit", "value") |>
    purrr::map(
      \(value_col) {
        compute_value_consistency(
          consistency_panel,
          id_col = "id",
          value_col = value_col,
          group_cols = "ref_date"
        ) |>
          dplyr::mutate(variable = value_col)
      }
    ) |>
    dplyr::bind_rows()

  plot <- plot_consistency_heatmap(consistency)

  expect_s3_class(plot, "plotly")
  expect_no_error(plotly::plotly_build(plot))
})

test_that("the consistency functions give the same result on a database table", {
  skip_if_not_installed("dbplyr")
  skip_if_not_installed("duckdb")

  # a missing identifier, unit and value are each kept as their own value
  hr <- rbind(
    consistency_panel,
    data.frame(
      id = c(NA, "d", "d"),
      ref_date = as.Date("2021-01-01"),
      unit = c("A", NA, NA),
      value = c(1, NA, 2)
    )
  )

  con <- DBI::dbConnect(duckdb::duckdb())
  remote <- dplyr::copy_to(con, hr, "consistency_panel")

  for (group_cols in list(NULL, "ref_date", "unit", c("unit", "ref_date"))) {
    expected <- compute_record_consistency(hr, "id", group_cols = group_cols) |>
      dplyr::arrange(dplyr::across(dplyr::all_of(group_cols))) |>
      as.data.frame()
    result <- compute_record_consistency(remote, "id", group_cols = group_cols) |>
      dplyr::collect() |>
      dplyr::arrange(dplyr::across(dplyr::all_of(group_cols))) |>
      as.data.frame()
    expect_equal(result, expected, ignore_attr = TRUE)

    expected <- compute_value_consistency(hr, "id", "value", group_cols = group_cols) |>
      dplyr::arrange(dplyr::across(dplyr::all_of(group_cols))) |>
      as.data.frame()
    result <- compute_value_consistency(remote, "id", "value", group_cols = group_cols) |>
      dplyr::collect() |>
      dplyr::arrange(dplyr::across(dplyr::all_of(group_cols))) |>
      as.data.frame()
    expect_equal(result, expected, ignore_attr = TRUE)
  }

  expect_equal(
    compute_global_consistency(remote, "id", value_cols = c("unit", "value")),
    compute_global_consistency(hr, "id", value_cols = c("unit", "value"))
  )
  expect_equal(compute_global_coverage(remote), compute_global_coverage(hr))

  # a lazy table straight from the database is collected before plotting
  lazy_trend <- compute_record_consistency(
    remote,
    "id",
    group_cols = c("unit", "ref_date")
  )
  expect_no_error(
    ggplot2::ggplot_build(plot_consistency_trend(lazy_trend, group_col = "unit"))
  )

  DBI::dbDisconnect(con, shutdown = TRUE)
})
