#' Build the govhrapp database
#'
#' Writes analytical tables derived into a duckdb file.
#'
#' @param force Logical. If \code{FALSE} (default), skips writing if the
#'   database file already exists. Set to \code{TRUE} to overwrite.
#'
#' @import dplyr DBI duckdb
#' @importFrom lubridate year
#' @importFrom purrr map
#'
#' @return Invisibly returns the path to the DuckDB file.
#' @export
build_govhrdb <- function(force = FALSE) {
  db_dir <- system.file("db", package = "govhrapp")

  # 0. create inst/db if absent
  if (!nzchar(db_dir)) {
    db_dir <- file.path(
      system.file(package = "govhrapp"),
      "db"
    )
    dir.create(db_dir, recursive = TRUE, showWarnings = FALSE)
  }

  db_path <- file.path(db_dir, "govhrapp.duckdb")

  if (file.exists(db_path) && !force) {
    message(
      "DB already exists at: ",
      db_path,
      "\nUse force = TRUE to overwrite."
    )
    return(invisible(db_path))
  }

  if (file.exists(db_path)) {
    file.remove(db_path)
  }

  message("Building govhrapp database at: ", db_path)

  # 1. prepare data
  educat7_levels <- c(
    "No formal education",
    "Some primary education",
    "Completed primary education",
    "Some secondary education",
    "Completed secondary education",
    "Some tertiary education",
    "Completed tertiary education"
  )

  educat7_map <- c(
    `1` = "No formal education",
    `2` = "Some primary education",
    `3` = "Completed primary education",
    `4` = "Some secondary education",
    `5` = "Completed secondary education",
    `6` = "Some tertiary education",
    `7` = "Completed tertiary education"
  )

  personnel_clean <- govhr::bra_hrmis_personnel |>
    dplyr::mutate(
      educat7 = educat7_map[as.character(as.numeric(.data[["educat7"]]))]
    ) |>
    dplyr::filter(lubridate::year(.data[["ref_date"]]) <= 2017) |>
    dplyr::distinct(
      .data[["ref_date"]],
      .data[["personnel_id"]],
      .keep_all = TRUE
    ) |>
    dplyr::select(dplyr::all_of(c(
      "ref_date",
      "personnel_id",
      "gender",
      "educat7",
      "status"
    )))

  workforce_data <- personnel_clean

  wagebill_data <- govhr::bra_hrmis_contract |>
    dplyr::filter(lubridate::year(.data[["ref_date"]]) <= 2017) |>
    dplyr::left_join(
      personnel_clean,
      by = c("ref_date", "personnel_id")
    )

  est_data <- govhr::bra_hrmis_est
  personnel_data <- govhr::bra_hrmis_personnel
  contract_data <- govhr::bra_hrmis_contract

  # 2. write to duckdb
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = FALSE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  tables <- list(
    workforce = workforce_data,
    wagebill = wagebill_data,
    est = est_data,
    personnel = personnel_data,
    contract = contract_data
  ) |> 
    # convert to UTF-8
    purrr::map(\(df)
      mutate(df, across(where(is.character), \(col) iconv(col, from = "", to = "UTF-8", sub = "byte")))
    )

  for (tbl_name in names(tables)) {
    DBI::dbWriteTable(con, tbl_name, tables[[tbl_name]], overwrite = TRUE)
    message(
      "  wrote table: ",
      tbl_name,
      " (",
      nrow(tables[[tbl_name]]),
      " rows)"
    )
  }

  message("Done.")
  invisible(db_path)
}
