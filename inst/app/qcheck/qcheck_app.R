pkgload::load_all(".")

qc_obj <- govhr::compute_qualitycontrol(
  contract_dt  = govhr::bra_hrmis_contract,
  personnel_dt = govhr::bra_hrmis_personnel,
  est_dt       = govhr::bra_hrmis_est
)

est_data <- dplyr::tbl(
  govhrapp_con,
  "est"
) |> 
  duckplyr::as_duckdb_tibble()

personnel_data <- dplyr::tbl(
  govhrapp_con,
  "personnel"
) |> 
  duckplyr::as_duckdb_tibble()

contract_data <- dplyr::tbl(
  govhrapp_con,
  "contract"
) |> 
  duckplyr::as_duckdb_tibble()

run_qcheckapp(qc_obj)