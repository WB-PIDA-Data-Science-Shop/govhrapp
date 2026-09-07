# Tests for the helpers shared across every analytics module. These three sit on
# the path of all eight module servers, so a regression here surfaces as a wrong
# chart in the wage bill, workforce, equity, movement, transition and retirement
# panels at once.

# ---- filter_data -------------------------------------------------------------
# Called by every module server to apply the sidebar selections.

panel <- tibble::tibble(
  ref_date = as.Date(c(
    "2020-01-01", "2020-01-01", "2021-01-01", "2021-01-01",
    "2022-01-01", "2022-01-01"
  )),
  paygrade = c("G1", "G2", "G1", "G2", "G1", "G2"),
  gross_salary_lcu = c(100, 200, 150, 250, 120, 300)
)

test_that("filter_data applies subgroup and date filters together", {
  result <- filter_data(
    panel,
    group_filter = "paygrade",
    subgroup_filter = "G1",
    date_range = as.Date(c("2021-01-01", "2022-01-01"))
  )

  expect_equal(nrow(result), 2L)
  expect_true(all(result$paygrade == "G1"))
  expect_true(all(result$ref_date >= as.Date("2021-01-01")))
})

test_that("filter_data ignores subgroup_filter when grouping by ref_date", {
  result <- filter_data(
    panel,
    group_filter = "ref_date",
    subgroup_filter = "G1",
    date_range = NULL
  )

  expect_equal(nrow(result), nrow(panel))
  expect_setequal(result$paygrade, c("G1", "G2"))
})

test_that("filter_data drops rows with a missing ref_date", {
  # the single-pass subset must not readmit NA comparisons as all-NA rows
  panel_na <- panel
  panel_na$ref_date[1] <- as.Date(NA)

  result <- filter_data(
    panel_na,
    group_filter = "ref_date",
    subgroup_filter = NULL,
    date_range = as.Date(c("2020-01-01", "2022-01-01"))
  )

  expect_equal(nrow(result), nrow(panel) - 1L)
  expect_false(anyNA(result$ref_date))
})

test_that("filter_data preserves the class of its input", {
  # downstream modules index the result with data.table syntax
  result <- filter_data(panel, "paygrade", "G1", NULL)
  expect_s3_class(result, "tbl_df")

  result_dt <- filter_data(
    data.table::as.data.table(panel), "paygrade", "G1", NULL
  )
  expect_s3_class(result_dt, "data.table")
  expect_equal(nrow(result_dt), 3L)
})

# ---- identify_group_choices --------------------------------------------------
# Called by default_ui_controls(), so it runs for every module sidebar.

test_that("identify_group_choices lists only dictionary variables in the data", {
  choices <- identify_group_choices(panel)

  expect_equal(choices[["All"]], "ref_date")

  offered <- unlist(choices, use.names = FALSE)
  expect_true("paygrade" %in% offered)
  # numeric measures and absent variables are never grouping options
  expect_false("gross_salary_lcu" %in% offered)
  expect_false("gender" %in% offered)
})

test_that("identify_group_choices excludes identifier columns", {
  with_ids <- panel
  with_ids$personnel_id <- as.character(seq_len(nrow(panel)))
  with_ids$contract_id <- as.character(seq_len(nrow(panel)))

  offered <- unlist(identify_group_choices(with_ids), use.names = FALSE)

  expect_false("personnel_id" %in% offered)
  expect_false("contract_id" %in% offered)
})

test_that("identify_group_choices tracks the columns of the data it is given", {
  narrow <- identify_group_choices(panel)

  wide <- panel
  wide$gender <- rep(c("male", "female"), length.out = nrow(panel))
  wide_choices <- identify_group_choices(wide)

  expect_identical(identify_group_choices(panel), narrow)
  expect_true("gender" %in% unlist(wide_choices, use.names = FALSE))
  expect_false("gender" %in% unlist(narrow, use.names = FALSE))
})

# ---- detect_career_transition ------------------------------------------------
# Backs both the transition panel and the pre-computed analytics cache.

careers <- data.frame(
  contract_id = c("a", "a", "a", "a", "b", "b"),
  ref_date = as.Date(c(
    "2020-01-01", "2021-01-01", "2022-01-01", "2023-01-01",
    "2020-01-01", "2021-01-01"
  )),
  paygrade = c("G1", "G1", "G2", "G3", "G5", "G5"),
  unit = c("U1", "U1", "U1", "U1", "U2", "U2"),
  stringsAsFactors = FALSE
)

test_that("detect_career_transition collapses consecutive periods into spells", {
  result <- detect_career_transition(
    careers,
    id_col = "contract_id",
    group_cols = "paygrade"
  )

  # a: G1 (2020-2021) -> G2 (2022) -> G3 (2023) is two transitions, not three
  expect_equal(nrow(result), 2L)
  expect_equal(result$from, c("G1", "G2"))
  expect_equal(result$to, c("G2", "G3"))
})

test_that("detect_career_transition dates each row to the start of the from spell", {
  result <- detect_career_transition(
    careers,
    id_col = "contract_id",
    group_cols = "paygrade"
  )

  # G1 began in 2020 even though the move to G2 happened in 2022
  expect_equal(result$ref_date, as.Date(c("2020-01-01", "2022-01-01")))
})

test_that("detect_career_transition keeps non-movers only when return_all is TRUE", {
  movers_only <- detect_career_transition(
    careers,
    id_col = "contract_id",
    group_cols = "paygrade"
  )
  everyone <- detect_career_transition(
    careers,
    id_col = "contract_id",
    group_cols = "paygrade",
    return_all = TRUE
  )

  # b never leaves G5
  expect_false("b" %in% movers_only$contract_id)
  expect_true("b" %in% everyone$contract_id)
  expect_true(is.na(everyone$to[everyone$contract_id == "b"]))
})

test_that("detect_career_transition combines multiple grouping columns", {
  result <- detect_career_transition(
    careers,
    id_col = "contract_id",
    group_cols = c("paygrade", "unit")
  )

  expect_equal(result$from, c("G1 | U1", "G2 | U1"))
  expect_equal(result$to, c("G2 | U1", "G3 | U1"))
})

# ---- compute_growth_summary --------------------------------------------------
# Shared by the wage bill overview, workforce overview and movement panels.
# Guards the aggregate-first rewrite: endpoints are each group's own first and
# last reference date, independent of the order rows arrive in.

test_that("compute_growth_summary is invariant to input row order", {
  shuffled <- panel[c(4, 1, 6, 3, 2, 5), ]

  expect_equal(
    compute_growth_summary(panel, group_col = "paygrade", measure_col = "gross_salary_lcu"),
    compute_growth_summary(shuffled, group_col = "paygrade", measure_col = "gross_salary_lcu")
  )
})

test_that("compute_growth_summary uses each group's own first and last date", {
  # G1 is observed 2020-2022, G2 only 2021-2022
  uneven <- panel[!(panel$paygrade == "G2" & panel$ref_date == as.Date("2020-01-01")), ]

  result <- compute_growth_summary(
    uneven,
    group_col = "paygrade",
    measure_col = "gross_salary_lcu"
  )

  # G1: 100 -> 120 = +20%; G2: 250 -> 300 = +20%
  expect_equal(result$growth_rate[result$paygrade == "G1"], 20)
  expect_equal(result$growth_rate[result$paygrade == "G2"], 20)
})

test_that("compute_growth_summary drops unlabelled groups", {
  with_na <- panel
  with_na$paygrade[c(2, 4, 6)] <- NA_character_

  result <- compute_growth_summary(with_na, group_col = "paygrade")

  expect_false(anyNA(result$paygrade))
  expect_equal(result$paygrade, "G1")
})

# ---- "no grouping" convention ------------------------------------------------
# Module sidebars pass input$group_filter straight through, and its "All" option
# is the string "ref_date", never NULL. Every plot helper must therefore treat
# "ref_date" as "no grouping" -- a helper that only checks for NULL tries to
# facet or colour by a column the summary frames need not carry.

binned <- tibble::tibble(
  bin = c(0, 100, 200, 0, 100, 200),
  count = c(3L, 5L, 2L, 1L, 4L, 6L),
  pct = c(0.3, 0.5, 0.2, 0.1, 0.4, 0.6),
  cum_pct = c(0.3, 0.8, 1.0, 0.1, 0.5, 1.0),
  paygrade = rep(c("G1", "G2"), each = 3)
)

test_that("plot_histogram treats ref_date as no grouping", {
  # the cached percentile frame carries no ref_date column at all, so faceting
  # by it errored on the equity panel's first paint
  ungrouped <- binned[binned$paygrade == "G1", c("bin", "count", "pct", "cum_pct")]

  expect_no_error(plot_histogram(ungrouped, "histogram", group_col = "ref_date"))
  expect_no_error(plot_histogram(ungrouped, "cumulative", group_col = NULL))
})

test_that("plot_histogram facets when a real group is supplied", {
  built <- ggplot2::ggplot_build(
    ggplot2::ggplot(binned, ggplot2::aes(x = .data[["bin"]], y = .data[["pct"]])) +
      ggplot2::geom_col() +
      ggplot2::facet_wrap(ggplot2::vars(.data[["paygrade"]]))
  )
  expect_equal(nrow(built$layout$layout), 2L)

  expect_no_error(plot_histogram(binned, "histogram", group_col = "paygrade"))
})

test_that("grouped plot helpers accept ref_date without a ref_date column", {
  trend <- tibble::tibble(ref_date = as.Date(c("2020-01-01", "2021-01-01")), value = c(10, 20))
  deciles <- tibble::tibble(decile = 1:10, mean_value = seq(100, 1000, by = 100))
  compression <- tibble::tibble(
    ref_date = as.Date(c("2020-01-01", "2021-01-01")),
    percentile_lower = c(1, 1.1),
    percentile_50 = c(2, 2.1),
    percentile_upper = c(3, 3.1)
  )
  costs <- tibble::tibble(ref_date = as.Date("2020-01-01"), movement_cost = 500)

  expect_no_error(plot_trend(trend, group_col = "ref_date"))
  expect_no_error(plot_decile(deciles, group_col = "ref_date"))
  expect_no_error(plot_compression_ratio(compression, group_col = "ref_date"))
  expect_no_error(plot_movement_cost(costs, group_col = "ref_date"))
})
