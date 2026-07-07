pkgload::load_all(".")

govhrapp_con <- DBI::dbConnect(
  duckdb::duckdb(),
  dbdir = fs::path(
    system.file("db", package = "govhrapp"),
    "govhrapp.duckdb"
  ),
  read_only = TRUE
)

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

run_govhrapp_qcheck(
  est_data = est_data,
  personnel_data = personnel_data,
  contract_data = contract_data
)