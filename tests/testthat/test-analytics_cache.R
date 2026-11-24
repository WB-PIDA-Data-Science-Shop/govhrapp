# The analytics cache calls every govhr compute function the panels use, so
# building it on the bundled sample catches breaking changes in govhr before
# the app or the report hits them.

test_that("build_analytics_cache builds every panel from the sample data", {
  workforce_data <- govhr::bra_hrmis_personnel |>
    dplyr::filter(lubridate::year(.data[["ref_date"]]) <= 2017) |>
    dplyr::distinct(
      .data[["ref_date"]],
      .data[["personnel_id"]],
      .keep_all = TRUE
    ) |>
    dplyr::left_join(
      govhr::bra_hrmis_contract |>
        dplyr::distinct(.data[["personnel_id"]], .data[["ref_date"]], .keep_all = TRUE),
      by = c("ref_date", "personnel_id")
    )

  wagebill_data <- govhr::bra_hrmis_contract |>
    dplyr::filter(lubridate::year(.data[["ref_date"]]) <= 2017) |>
    dplyr::left_join(
      govhr::bra_hrmis_personnel,
      by = c("ref_date", "personnel_id")
    )

  # the sample holds contracts that share a personnel_id and ref_date, which
  # compute_transition() drops with a warning
  cache <- suppressWarnings(
    build_analytics_cache(workforce_data, wagebill_data)
  )

  expect_named(
    cache$workforce$movement_box,
    c("hire", "separation", "retirement", "replacement")
  )
  expect_true(all(
    c("ref_date", "headcount") %in% names(cache$workforce$workforce_overview)
  ))
  expect_true(all(
    c("ref_date", "wagebill", "wage") %in% names(cache$wagebill$wagebill_overview)
  ))
  expect_true(all(
    c("hires", "separations", "replacement_rate") %in%
      names(cache$workforce$workforce_movement)
  ))
  expect_true(all(
    c("retirements", "retirement_rate") %in%
      names(cache$workforce$workforce_retirement)
  ))
  expect_true(
    "projected_retirement_rate" %in%
      names(cache$workforce$workforce_retirement_expected)
  )
  expect_true(all(c("from", "to") %in% names(cache$workforce$workforce_transition)))
  expect_false("ref_date" %in% names(cache$wagebill$wagebill_equity_percentile))

  # the Overview tab and the key indicator box both count only active
  # personnel, so they report the same wage bill for the latest date
  wagebill_box <- cache[["wagebill"]][["total_box"]][["total_wagebill"]]
  overview_wagebill <- cache[["wagebill"]][["wagebill_overview"]] |>
    dplyr::filter(.data[["ref_date"]] == wagebill_box[["ref_date"]])

  expect_equal(overview_wagebill[["wagebill"]], wagebill_box[["total"]])

  latest_date <- max(workforce_data[["ref_date"]])
  overview_headcount <- cache[["workforce"]][["workforce_overview"]] |>
    dplyr::filter(.data[["ref_date"]] == latest_date)
  active_people <- workforce_data |>
    dplyr::filter(
      .data[["ref_date"]] == latest_date,
      .data[["employment_status"]] == "active"
    ) |>
    dplyr::pull(.data[["personnel_id"]]) |>
    dplyr::n_distinct(na.rm = TRUE)

  expect_equal(overview_headcount[["headcount"]], active_people)

  # the retirement panels' pension payments match the key indicator box, and
  # their pensioner headcount counts each pensioner once
  pension_box <- cache[["wagebill"]][["total_box"]][["total_pension_liabilities"]]
  latest_pensions <- cache[["wagebill"]][["wagebill_pensions"]] |>
    dplyr::filter(.data[["ref_date"]] == pension_box[["ref_date"]])

  expect_equal(latest_pensions[["wagebill"]], pension_box[["total"]])

  latest_pensioners <- cache[["workforce"]][["workforce_pensioners"]] |>
    dplyr::filter(.data[["ref_date"]] == latest_date)
  pensioner_people <- workforce_data |>
    dplyr::filter(
      .data[["ref_date"]] == latest_date,
      .data[["employment_status"]] == "pensioner"
    ) |>
    dplyr::pull(.data[["personnel_id"]]) |>
    dplyr::n_distinct(na.rm = TRUE)

  expect_equal(latest_pensioners[["headcount"]], pensioner_people)

  # pensions are not pay, so the equity summaries leave pensioners out
  expect_equal(
    cache[["wagebill"]][["wagebill_equity_compression"]],
    wagebill_data |>
      dplyr::filter(.data[["employment_status"]] == "active") |>
      govhr::compute_compression_ratio(measure_col = "gross_salary_lcu") |>
      dplyr::collect(),
    ignore_attr = TRUE
  )
})
