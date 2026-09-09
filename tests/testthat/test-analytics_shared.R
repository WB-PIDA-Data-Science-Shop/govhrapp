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

