pkgload::load_all(".")

# micro data
# boostrap to increase sample size and make load testing realistic
n_boot <- 100

est_data <- purrr::map_dfr(
  1:n_boot, 
  ~ slice_sample(govhr::bra_hrmis_est, n = nrow(govhr::bra_hrmis_est), replace = TRUE)
)

personnel_data <- purrr::map_dfr(
  1:n_boot, 
  ~ slice_sample(govhr::bra_hrmis_personnel, n = nrow(govhr::bra_hrmis_personnel), replace = TRUE)
)

contract_data <- purrr::map_dfr(
  1:n_boot, 
  ~ slice_sample(govhr::bra_hrmis_contract, n = nrow(govhr::bra_hrmis_contract), replace = TRUE)
)

# validation data
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
