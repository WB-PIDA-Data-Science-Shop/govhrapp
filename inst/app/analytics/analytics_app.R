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
  )))

wagebill_data <- govhr::bra_hrmis_contract |>
  dplyr::filter(lubridate::year(.data[["ref_date"]]) <= 2017) |>
  dplyr::left_join(
    workforce_data,
    by = c("ref_date", "personnel_id")
  ) |>
  mutate(
    country_code = "BRA"
  )

# bootstrap to increase size
workforce_data <- workforce_data |>
  dplyr::slice_sample(n = 1e6, replace = TRUE)

wagebill_data <- wagebill_data |>
  dplyr::slice_sample(n = 1e6, replace = TRUE)

cache_analytics <- build_analytics_cache(workforce_data, wagebill_data)

run_govhrapp_beta(workforce_data, wagebill_data, cache_analytics)
