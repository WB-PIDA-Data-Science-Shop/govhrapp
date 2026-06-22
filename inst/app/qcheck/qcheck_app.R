pkgload::load_all(".")

qc_obj <- govhr::compute_qualitycontrol(
  contract_dt  = govhr::bra_hrmis_contract,
  personnel_dt = govhr::bra_hrmis_personnel,
  est_dt       = govhr::bra_hrmis_est
)

run_qcheckapp(qc_obj)