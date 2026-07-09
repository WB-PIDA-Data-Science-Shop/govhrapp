pkgload::load_all(".")
library(dbplyr)

govhrapp_con <- DBI::dbConnect(
  duckdb::duckdb(),
  dbdir = fs::path(
    system.file("db", package = "govhrapp"),
    "govhrapp.duckdb"
  ),
  read_only = TRUE
)

# micro data
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