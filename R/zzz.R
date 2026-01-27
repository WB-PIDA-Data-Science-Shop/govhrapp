# Global variables to avoid R CMD check NOTEs
# These are column names used in data.table and tidyverse-style NSE

utils::globalVariables(c(
  # Column names used in databasics_server
  "Records",
  "Fields",
  "Module",
  "Status",
  "N",
  "Check",
  "Missing Fields",
  "Extra Fields",
  "Unmatched Records",
  "ref_date",
  "n_obs",
  "module",
  # Column names used in missingness_server
  "pct_missing",
  "variable",
  "occupation_native",
  "occupation_isconame",
  "est_id",
  "year",
  "mean_missing",
  "value",
  # Column names used in volatility_server
  "contract_id",
  "indicator",
  "rolling_cv",
  "pct_change",
  "."
))
