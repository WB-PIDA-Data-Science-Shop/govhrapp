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
  "Missing Variables",
  "Extra Variables",
  "Missing Count",
  "n_distinct",
  "ref_date",
  "n_obs",
  "module",
  "Date Range",
  # Column names used in missingness_server
  "pct_missing",
  "variable",
  "occupation_native",
  "occupation_isconame",
  "est_id",
  "year",
  "mean_missing",
  "value",
  "group_val",
  "target_var",
  "target_label",
  "group_label",
  # Column names used in validation_server
  "Total Records",
  "Passes",
  "Fails",
  "Pass Rate",
  "Errors",
  # Column names used in volatility_server
  "contract_id",
  "indicator",
  "indicator_label",
  "vol_stat",
  "max_vol",
  "n_missing",
  "stat_type",
  "group_var",
  "group_var_label",
  "rolling_cv",
  "pct_change",
  ".",
  # data.table and dplyr symbols
  ".N",
  ".data",
  # some functions
  "nav_menu"
))
