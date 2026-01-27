# Quality Control Dashboard App
# This file is used for deploying the quality control dashboard to Posit Connect

# Load the package
pkgload::load_all(".")

# Prepare the quality control data
# Note: In production, you might want to load this from a saved file or database
qc_data <- compute_dynamicqc(
  contract_dt = govhr::bra_hrmis_contract,
  personnel_dt = govhr::bra_hrmis_personnel,
  est_dt = govhr::bra_hrmis_est
)

# Run the quality control app
run_qcheckapp(dynamicqc_obj = qc_data)
