#' Run the quality check app on sample data
#'
#' Launches [run_govhrapp_qcheck()] on the sample Brazilian establishment,
#' personnel and contract data shipped with govhr, validated against govhr's
#' standard rules.
#'
#' @returns A Shiny app object.
#'
#' @examples
#' \dontrun{
#' run_qcheck_demo()
#' }
#'
#' @export
run_qcheck_demo <- function(){
  est_data <- govhr::bra_hrmis_est

  personnel_data <- govhr::bra_hrmis_personnel

  contract_data <- govhr::bra_hrmis_contract

  contract_validation <- govhr::validate_data(
    govhr::bra_hrmis_contract,
    govhr::contract_rules
  )

  personnel_validation <- govhr::validate_data(
    govhr::bra_hrmis_personnel,
    govhr::personnel_rules
  )

  run_govhrapp_qcheck(
    est_data = est_data,
    personnel_data = personnel_data,
    contract_data = contract_data,
    contract_validation = contract_validation,
    personnel_validation = personnel_validation
  )
}

#' Run the analytics app on sample data
#'
#' Launches [run_govhrapp_analytics()] on the sample Brazilian personnel and
#' contract data shipped with govhr, up to 2017. The contract data takes
#' `employment_status`, `birth_date` and the other personnel columns from the
#' personnel data, which is how to prepare your own data for the app.
#'
#' @returns A Shiny app object.
#'
#' @examples
#' \dontrun{
#' run_analytics_demo()
#' }
#'
#' @export
run_analytics_demo <- function(){
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

  cache_analytics <- build_analytics_cache(workforce_data, wagebill_data)

  run_govhrapp_analytics(workforce_data, wagebill_data, cache_analytics)
}