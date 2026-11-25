#' Minimum columns of the analytics app
#'
#' The columns each input of the analytics app must contain. Each panel reads
#' some of them directly, so a missing column would otherwise surface as an
#' error from deep inside a govhr function.
#'
#' * `workforce`: personnel data. `employment_status` marks active personnel as
#'   `"active"` and retirees as `"pensioner"`; `birth_date` projects
#'   retirements; `gender` and `educat7` describe hires and separations in the
#'   movement profile table.
#' * `wagebill`: contract data for the wage bill panels. `gross_salary_lcu`
#'   is the pay measure shown before the user picks another.
#' * `transition`: contract data for the workforce transition panel, which
#'   counts moves between contract types.
#'
#' @format A named list of character vectors.
#'
#' @keywords internal
analytics_required_cols <- list(
  workforce = c(
    "ref_date",
    "personnel_id",
    "employment_status",
    "birth_date",
    "gender",
    "educat7"
  ),
  wagebill = c(
    "ref_date",
    "personnel_id",
    "employment_status",
    "birth_date",
    "gross_salary_lcu"
  ),
  transition = c(
    "ref_date",
    "personnel_id",
    "contract_type"
  )
)

#' Check that a table has the columns the analytics app needs
#'
#' Stops with an error naming the missing columns, so a user learns what to add
#' before the app starts rather than from an error inside a panel.
#'
#' @param data Data frame or database table (`tbl_dbi`) to check.
#' @param required_cols Character vector of the columns `data` must contain.
#' @param arg Character. Name of the argument holding `data`, used in the error
#'   message.
#'
#' @returns `data`, invisibly.
#'
#' @examples
#' \dontrun{
#' check_required_columns(
#'   workforce_data,
#'   analytics_required_cols[["workforce"]],
#'   arg = "workforce_data"
#' )
#' }
#'
#' @keywords internal
check_required_columns <- function(data, required_cols, arg) {
  # colnames() rather than names(), which does not list a tbl_dbi's columns
  missing_cols <- setdiff(required_cols, colnames(data))

  if (length(missing_cols) > 0) {
    stop(
      "`", arg, "` is missing required columns: ",
      paste0("`", missing_cols, "`", collapse = ", "), ".\n",
      "It must contain: ",
      paste0("`", required_cols, "`", collapse = ", "), ".",
      call. = FALSE
    )
  }

  invisible(data)
}

#' Check the inputs of the analytics app
#'
#' Checks personnel and contract data against [analytics_required_cols]. Either
#' input can be `NULL` to skip it, as the standalone wage bill app has no
#' personnel data.
#'
#' @param workforce_data Personnel data, or `NULL` to skip it.
#' @param wagebill_data Contract data, or `NULL` to skip it.
#' @param transition Logical. Also require the columns of the workforce
#'   transition panel in `wagebill_data`. Default `TRUE`.
#'
#' @returns `NULL`, invisibly.
#'
#' @keywords internal
check_analytics_data <- function(
  workforce_data = NULL,
  wagebill_data = NULL,
  transition = TRUE
) {
  if (!is.null(workforce_data)) {
    check_required_columns(
      workforce_data,
      analytics_required_cols[["workforce"]],
      arg = "workforce_data"
    )
  }

  if (!is.null(wagebill_data)) {
    wagebill_cols <- analytics_required_cols[["wagebill"]]

    if (transition) {
      wagebill_cols <- union(
        wagebill_cols,
        analytics_required_cols[["transition"]]
      )
    }

    check_required_columns(wagebill_data, wagebill_cols, arg = "wagebill_data")
  }

  invisible(NULL)
}
