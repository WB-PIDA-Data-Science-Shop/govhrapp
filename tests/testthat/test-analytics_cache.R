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
})
