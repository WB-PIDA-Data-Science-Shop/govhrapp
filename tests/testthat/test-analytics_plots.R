test_data <- data.frame(
  ref_date = as.Date(c(
    "2020-01-01", "2020-01-01", "2021-01-01", "2021-01-01",
    "2022-01-01", "2022-01-01"
  )),
  group = c("A", "B", "A", "B", "A", "B"),
  salary = c(100, 200, 150, 250, 120, 300),
  stringsAsFactors = FALSE
)

# ---- compute_trend_summary ---------------------------------------------------

test_that("compute_trend_summary counts rows when measure_col is NULL", {
  result <- compute_trend_summary(test_data, group = "ref_date")
  expect_equal(names(result), c("ref_date", "value"))
  expect_equal(nrow(result), 3L)
  expect_true(all(result$value == 2))
})

test_that("compute_trend_summary sums measure_col when supplied", {
  result <- compute_trend_summary(test_data, group = "group", measure_col = "salary")
  expect_setequal(names(result), c("ref_date", "indicator", "group", "value"))
  expect_equal(result$value[result$group == "A" & result$ref_date == as.Date("2020-01-01")], 100)
})

# ---- apply_baseline_index ----------------------------------------------------

test_that("apply_baseline_index sets first period to 100 when ungrouped", {
  summary <- compute_trend_summary(test_data, group = "ref_date")
  result <- apply_baseline_index(summary, group = "ref_date")
  expect_equal(result$value[result$ref_date == min(result$ref_date)], 100)
})

test_that("apply_baseline_index rescales independently per group", {
  summary <- compute_trend_summary(test_data, group = "group", measure_col = "salary")
  result <- apply_baseline_index(summary, group = "group")
  first_vals <- result |>
    dplyr::group_by(group) |>
    dplyr::slice_min(ref_date) |>
    dplyr::pull(value)
  expect_true(all(first_vals == 100))
})

# ---- compute_cross_section_summary -------------------------------------------

test_that("compute_cross_section_summary uses only the latest date", {
  result <- compute_cross_section_summary(test_data, group = "group")
  expect_equal(nrow(result), 2L)
  expect_equal(result$value[result$group == "A"], 1L)  # 1 row per group at 2022
})

test_that("compute_cross_section_summary sums measure_col at latest date", {
  result <- compute_cross_section_summary(test_data, group = "group", measure_col = "salary")
  expect_equal(result$value[result$group == "B"], 300)
})

# ---- compute_growth_summary --------------------------------------------------

test_that("compute_growth_summary returns growth_rate column with correct sign", {
  result <- compute_growth_summary(test_data, measure_col = "salary", group = "group")
  expect_true("growth_rate" %in% names(result))
  expect_gt(result$growth_rate[result$group == "A"], 0)
})

test_that("compute_growth_summary computes correct pct change with measure_col", {
  result <- compute_growth_summary(test_data, group = "group", measure_col = "salary")
  # B: first = 200, last = 300 → growth = 50
  expect_equal(result$growth_rate[result$group == "B"], 50)
})

# ---- plot_trend --------------------------------------------------------------

test_that("plot_trend returns a ggplot object", {
  summary <- compute_trend_summary(test_data, group = "ref_date")
  expect_s3_class(plot_trend(summary, group = "ref_date"), "ggplot")
})

test_that("plot_trend toggle_growth adds a hline layer", {
  summary <- compute_trend_summary(test_data, group = "ref_date")
  p <- plot_trend(summary, group = "ref_date", toggle_growth = TRUE)
  layer_geoms <- sapply(p$layers, \(l) class(l$geom)[1])
  expect_true("GeomHline" %in% layer_geoms)
})

# ---- plot_bar_total ----------------------------------------------------------

test_that("plot_bar_total returns a ggplot with a GeomCol layer", {
  cs <- compute_cross_section_summary(test_data, group = "group")
  p <- plot_bar_total(cs, group = "group", x_label = "Headcount")
  layer_geoms <- sapply(p$layers, \(l) class(l$geom)[1])
  expect_s3_class(p, "ggplot")
  expect_true("GeomCol" %in% layer_geoms)
})

# ---- plot_bar_growth ---------------------------------------------------------

test_that("plot_bar_growth returns a ggplot with a GeomVline layer at 0", {
  growth <- compute_growth_summary(test_data, group = "group", measure_col = "salary")
  p <- plot_bar_growth(growth, group = "group")
  expect_s3_class(p, "ggplot")
  layer_geoms <- sapply(p$layers, \(l) class(l$geom)[1])
  expect_true("GeomVline" %in% layer_geoms)
})
