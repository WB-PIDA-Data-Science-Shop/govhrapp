pkgload::load_all(".")

# micro data
est_data <- govhr::bra_hrmis_est

personnel_data <- govhr::bra_hrmis_personnel

contract_data <- govhr::bra_hrmis_contract

# validation data
contract_validation <- govhr::validate_data(
  contract_data,
  govhr::contract_rules
)

personnel_validation <- govhr::validate_data(
  personnel_data,
  govhr::personnel_rules
)

cache <- list(
  box_consistency_est = render_consistency_box(
      est_data,
      id_col = "est_id",
      value_cols = c("est_name_native"),
      "Establishments",
      "building"
    ),
  box_consistency_personnel = render_consistency_box(
      personnel_data,
      id_col = "personnel_id",
      value_cols = c("birth_date"),
      "Personnel",
      "people-fill"
    ),
  box_consistency_contract = render_consistency_box(
      contract_data,
      id_col = "contract_id",
      value_cols = c("contract_type"),
      "Contracts",
      "file-text-fill"
    )
)

run_govhrapp_qcheck(
  est_data = est_data,
  personnel_data = personnel_data,
  contract_data = contract_data,
  contract_validation = contract_validation,
  personnel_validation = personnel_validation
)
