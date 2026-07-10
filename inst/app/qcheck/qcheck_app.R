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

run_govhrapp_qcheck(
  est_data = est_data,
  personnel_data = personnel_data,
  contract_data = contract_data,
  contract_validation = contract_validation,
  personnel_validation = personnel_validation
)
