# Quality Control Dashboard App
# This file is used for deploying the quality control dashboard to Posit Connect

# Load the package
pkgload::load_all(".")

# Compute quality control object directly
qc_obj <- govhr::compute_qualitycontrol(
  contract_dt  = govhr::bra_hrmis_contract,
  personnel_dt = govhr::bra_hrmis_personnel,
  est_dt       = govhr::bra_hrmis_est
)

# Run the quality control app
run_qcheckapp(qc_obj)
