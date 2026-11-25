# Tests for the "of which" composition card shared by the overview panels.

composition_panel <- data.frame(
  personnel_id = c("1", "2", "3", "4", "1", "2", "3", "5"),
  ref_date = as.Date(rep(c("2020-01-01", "2021-01-01"), each = 4)),
  paygrade = c("G1", "G1", "G2", "G2", "G1", "G1", "G2", "G2"),
  gender = c("female", "male", "male", NA, "female", "female", "male", "male")
)

by_paygrade_gender <- govhr::compute_headcount(
  composition_panel,
  group_cols = c("paygrade", "gender")
)

# ---- identify_breakdown_choices ----------------------------------------------

test_that("identify_breakdown_choices offers None in place of All", {
  choices <- identify_breakdown_choices(composition_panel)

  expect_equal(choices[["None"]], "none")
  expect_false("All" %in% names(choices))

  offered <- unlist(choices, use.names = FALSE)
  expect_true(all(c("gender", "paygrade") %in% offered))
  expect_false("ref_date" %in% offered)
})

# ---- compute_composition_share -----------------------------------------------

test_that("compute_composition_share adds up to 100 within each date and group", {
  shares <- compute_composition_share(
    by_paygrade_gender,
    value_col = "headcount",
    group_col = "paygrade",
    breakdown_col = "gender"
  )

  totals <- tapply(
    shares[["share"]],
    paste(shares[["ref_date"]], shares[["paygrade"]]),
    sum
  )

  expect_equal(as.vector(totals), rep(100, 4))
})

test_that("compute_composition_share fills absent categories with zero", {
  # G1 has no men and no missing gender in 2021, yet every panel needs every
  # category on every date for the stack to draw
  shares <- compute_composition_share(
    by_paygrade_gender,
    value_col = "headcount",
    group_col = "paygrade",
    breakdown_col = "gender"
  )

  g1_2021 <- shares[
    shares[["paygrade"]] == "G1" &
      shares[["ref_date"]] == as.Date("2021-01-01"),
  ]

  expect_equal(nrow(shares), 2L * 2L * 3L)
  expect_equal(
    g1_2021[["share"]][g1_2021[["category"]] == "male"],
    0
  )
})

test_that("compute_composition_share shows a missing breakdown as its own band", {
  shares <- composition_panel |>
    govhr::compute_headcount(group_cols = "gender") |>
    compute_composition_share(
      value_col = "headcount",
      group_col = "ref_date",
      breakdown_col = "gender"
    )

  shares_2020 <- shares[shares[["ref_date"]] == as.Date("2020-01-01"), ]

  expect_false("paygrade" %in% names(shares))
  expect_equal(levels(shares[["category"]]), c("male", "female", "Missing"))
  expect_equal(
    shares_2020[["share"]][shares_2020[["category"]] == "Missing"],
    25
  )
})

test_that("compute_composition_share combines the smallest categories", {
  many <- data.frame(
    ref_date = as.Date("2020-01-01"),
    occupation = c("a", "b", "c", "d"),
    headcount = c(40, 30, 20, 10)
  )

  shares <- compute_composition_share(
    many,
    value_col = "headcount",
    group_col = "ref_date",
    breakdown_col = "occupation",
    max_categories = 2
  )

  expect_equal(levels(shares[["category"]]), c("a", "b", "Other"))
  expect_equal(shares[["share"]][shares[["category"]] == "Other"], 30)
})

test_that("compute_composition_share drops periods with no measure", {
  # compute_wagebill() gives a series absent on a date an NA total
  with_gap <- data.frame(
    ref_date = as.Date(c("2020-01-01", "2020-01-01", "2021-01-01")),
    gender = c("female", "male", "female"),
    wagebill = c(60, 40, NA)
  )

  shares <- compute_composition_share(
    with_gap,
    value_col = "wagebill",
    group_col = "ref_date",
    breakdown_col = "gender"
  )

  expect_false(anyNA(shares[["share"]]))
  expect_equal(unique(shares[["ref_date"]]), as.Date("2020-01-01"))
})

# ---- composition_palette -----------------------------------------------------

test_that("composition_palette greys out the residual categories", {
  colours <- composition_palette(c("a", "b", "Other", "Missing"))

  expect_named(colours, c("a", "b", "Other", "Missing"))
  expect_equal(unname(colours[c("Other", "Missing")]), c("#9E9E9E", "#D9D9D9"))
})

# ---- plot_composition and composition_plot_height ----------------------------

test_that("plot_composition draws one panel per group only when grouped", {
  grouped <- compute_composition_share(
    by_paygrade_gender,
    value_col = "headcount",
    group_col = "paygrade",
    breakdown_col = "gender"
  )

  pooled <- composition_panel |>
    govhr::compute_headcount(group_cols = "gender") |>
    compute_composition_share(
      value_col = "headcount",
      group_col = "ref_date",
      breakdown_col = "gender"
    )

  grouped_built <- ggplot2::ggplot_build(
    plot_composition(grouped, "paygrade", "Share (%)", "Gender")
  )
  pooled_built <- ggplot2::ggplot_build(
    plot_composition(pooled, "ref_date", "Share (%)", "Gender")
  )

  expect_equal(nrow(grouped_built[["layout"]][["layout"]]), 2L)
  expect_equal(nrow(pooled_built[["layout"]][["layout"]]), 1L)
})

test_that("composition_plot_height grows with each row of panels", {
  one_row <- data.frame(paygrade = c("G1", "G2", "G3"))
  two_rows <- data.frame(paygrade = c("G1", "G2", "G3", "G4"))

  expect_equal(composition_plot_height(one_row, "ref_date"), 350)
  expect_equal(composition_plot_height(one_row, "paygrade"), 350)
  expect_equal(composition_plot_height(two_rows, "paygrade"), 440)
})

# ---- variable_label ----------------------------------------------------------

test_that("variable_label falls back to the ID for unlisted variables", {
  expect_equal(variable_label("gender"), "Gender")
  expect_equal(variable_label("not_a_variable"), "not_a_variable")
})

# ---- composition_server ------------------------------------------------------

composition_args <- function(selections) {
  list(
    filtered = \() composition_panel,
    group_col = \() selections[["group"]],
    breakdown_col = \() selections[["breakdown"]],
    apply = \() selections[["apply"]],
    compute_fn = govhr::compute_headcount,
    value_col = "headcount",
    measure_label = "headcount"
  )
}

test_that("composition_server computes nothing until a breakdown is applied", {
  selections <- shiny::reactiveValues(
    group = "paygrade",
    breakdown = "none",
    apply = 0
  )

  shiny::testServer(
    composition_server,
    args = composition_args(selections),
    {
      expect_null(applied())

      # an unapplied breakdown does not reach the card
      selections[["breakdown"]] <- "gender"
      session$flushReact()
      expect_null(applied())

      selections[["apply"]] <- 1
      session$flushReact()
      expect_setequal(
        levels(applied()[["shares"]][["category"]]),
        c("female", "male", "Missing")
      )
    }
  )
})

test_that("composition_server ignores a breakdown equal to the group", {
  selections <- shiny::reactiveValues(
    group = "gender",
    breakdown = "gender",
    apply = 1
  )

  shiny::testServer(
    composition_server,
    args = composition_args(selections),
    {
      expect_true(applied()[["same_as_group"]])
      expect_null(applied()[["shares"]])
      expect_match(as.character(output$card[["html"]]), "different from the group")
    }
  )
})
