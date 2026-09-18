pkgload::load_all(".")

workforce_data <- govhr::bra_hrmis_personnel |>
  dplyr::filter(lubridate::year(.data[["ref_date"]]) <= 2017) |>
  dplyr::distinct(
    .data[["ref_date"]],
    .data[["personnel_id"]],
    .keep_all = TRUE
  ) |>
  dplyr::select(dplyr::all_of(c(
    "ref_date",
    "personnel_id",
    "gender",
    "educat7",
    "employment_status",
    "birth_date"
  ))) |>
  left_join(
    govhr::bra_hrmis_contract |> dplyr::distinct(personnel_id, ref_date, .keep_all = TRUE),
    by = c("ref_date", "personnel_id")
  ) |>
  left_join(
    govhr::bra_hrmis_est |> dplyr::distinct(est_id, ref_date, .keep_all = TRUE),
    by = c("ref_date", "est_id")
  )

wagebill_data <- govhr::bra_hrmis_contract |>
  dplyr::filter(lubridate::year(.data[["ref_date"]]) <= 2017) |>
  dplyr::left_join(
    govhr::bra_hrmis_personnel,
    by = c("ref_date", "personnel_id")
  ) |>
  mutate(
    country_code = "BRA"
  )

cache_analytics <- build_analytics_cache(workforce_data, wagebill_data)

run_govhrapp_analytics(workforce_data, wagebill_data, cache_analytics)
