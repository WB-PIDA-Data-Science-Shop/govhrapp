#############################################################################################
########## SOME FUNCTIONS THAT CUT ACROSS MODULES AND INTERFACES OF THE QCHECK APP ##########
#############################################################################################

#' Compute Dynamic Quality Control Object
#'
#' Prepares aggregated data and diagnostics for the interactive quality control dashboard.
#' This function performs heavy computations once, so the Shiny app can focus on 
#' visualization and filtering. Use this before launching \code{run_qcheckapp()}.
#'
#' @param contract_dt data.table. Contract module data.
#' @param personnel_dt data.table. Personnel module data (optional).
#' @param est_dt data.table. Establishment module data (optional).
#'
#' @return A list (class \code{dynamicqc}) containing:
#'   \describe{
#'     \item{qc_obj}{Quality control diagnostics from \code{govhr::compute_qualitycontrol()}}
#'     \item{temporal_coverage}{Aggregated observation counts by reference date and module}
#'     \item{metadata}{Original data dimensions and date ranges}
#'   }
#'
#' @export
#' @importFrom data.table data.table as.data.table rbindlist setDT copy
#'
#' @examples
#' \dontrun{
#' qc_data <- compute_dynamicqc(
#'   contract_dt = govhr::bra_hrmis_contract,
#'   personnel_dt = govhr::bra_hrmis_personnel,
#'   est_dt = govhr::bra_hrmis_est
#' )
#' run_qcheckapp(qc_data)
#' }
compute_dynamicqc <- function(contract_dt, personnel_dt = NULL, est_dt = NULL) {
  
  # Convert to data.table
  contract_dt <- data.table::as.data.table(contract_dt)
  if (!is.null(personnel_dt)) personnel_dt <- data.table::as.data.table(personnel_dt)
  if (!is.null(est_dt)) est_dt <- data.table::as.data.table(est_dt)
  
  # Compute static quality control diagnostics
  qc_obj <- govhr::compute_qualitycontrol(
    contract_dt = contract_dt,
    personnel_dt = personnel_dt,
    est_dt = est_dt
  )
  
  # Aggregate temporal coverage (heavy computation done once)
  temporal_coverage <- list()
  
  if ("ref_date" %in% names(contract_dt)) {
    temporal_coverage$contract <- contract_dt[, list(n_obs = .N), by = ref_date][order(ref_date)]
  }
  
  if (!is.null(personnel_dt) && "ref_date" %in% names(personnel_dt)) {
    temporal_coverage$personnel <- personnel_dt[, list(n_obs = .N), by = ref_date][order(ref_date)]
  }
  
  if (!is.null(est_dt) && "ref_date" %in% names(est_dt)) {
    temporal_coverage$establishment <- est_dt[, list(n_obs = .N), by = ref_date][order(ref_date)]
  }
  
  # Extract metadata
  all_dates <- contract_dt$ref_date
  if (!is.null(personnel_dt) && "ref_date" %in% names(personnel_dt)) {
    all_dates <- c(all_dates, personnel_dt$ref_date)
  }
  if (!is.null(est_dt) && "ref_date" %in% names(est_dt)) {
    all_dates <- c(all_dates, est_dt$ref_date)
  }
  
  metadata <- list(
    date_range = range(all_dates, na.rm = TRUE),
    n_obs = list(
      contract = nrow(contract_dt),
      personnel = if (!is.null(personnel_dt)) nrow(personnel_dt) else 0,
      establishment = if (!is.null(est_dt)) nrow(est_dt) else 0
    ),
    n_vars = list(
      contract = ncol(contract_dt),
      personnel = if (!is.null(personnel_dt)) ncol(personnel_dt) else 0,
      establishment = if (!is.null(est_dt)) ncol(est_dt) else 0
    )
  )
  
  structure(
    list(
      qc_obj = qc_obj,
      temporal_coverage = temporal_coverage,
      metadata = metadata
    ),
    class = "dynamicqc"
  )
}


