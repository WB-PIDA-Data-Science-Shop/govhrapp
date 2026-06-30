pkgload::load_all(".")

govhrapp_con <- DBI::dbConnect(
  duckdb::duckdb(),
  dbdir = fs::path(
    system.file("db", package = "govhrapp"),
    "govhrapp.duckdb"
  ),
  read_only = TRUE
)

workforce_data <- tbl(
  govhrapp_con,
  "personnel"
) |> 
  duckplyr::as_duckdb_tibble()

wagebill_data <- tbl(
  govhrapp_con,
  "contract"
) |> 
  duckplyr::as_duckdb_tibble()

run_govhrapp(workforce_data, wagebill_data)