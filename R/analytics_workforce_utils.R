#' Detect Career Transitions
#'
#' Collapses each entity's history into spells of consecutive periods in the
#' same group, then pairs each spell with the one that follows it. The
#' `ref_date` on a returned row is the date the `from` spell began.
#'
#' @param .data Data frame containing a `ref_date` column, the identifier and
#'   the grouping columns.
#' @param id_col Character. Column identifying the entity whose career is
#'   tracked. Default `"contract_id"`.
#' @param group_cols Character vector of columns defining the career position
#'   (e.g. paygrade, department). Multiple columns are pasted into one label.
#' @param return_all Logical. Keep terminal spells, whose `to` is `NA`. Default
#'   `FALSE`.
#'
#' @return A data table with the identifier, `from`, `to` and `ref_date`.
#'
#' @importFrom data.table as.data.table rleidv setnames setorderv shift
#' @importFrom stats complete.cases
#' @export
detect_career_transition <- function(
  .data, id_col = "contract_id", group_cols,
  return_all = FALSE
) {
  dt <- data.table::as.data.table(.data)

  dt <- dt[
    stats::complete.cases(dt[, c(id_col, group_cols), with = FALSE])
  ]

  # if necessary, combine group cols into a single column
  if (length(group_cols) > 1) {
    dt[, grouping := do.call(paste, c(.SD, sep = " | ")), .SDcols = group_cols]

    group_cols <- "grouping"
  }

  data.table::setorderv(dt, c(id_col, "ref_date"))

  # collapse to spell, i.e., when an entity stays in the same group for
  # consecutive periods
  dt[, ".spell_id" := data.table::rleidv(.SD), by = id_col, .SDcols = group_cols]

  spells <- unique(dt, by = c(id_col, ".spell_id"))[
    , c(id_col, group_cols, "ref_date"), with = FALSE
  ]

  data.table::setnames(spells, group_cols, "from")

  # create a column for the ref_date of transition
  spells[
    , 
    "to" := data.table::shift(from, type = "lead"), 
    by = id_col
  ]

  out <- spells[,  c(id_col, "from", "to", "ref_date"), with = FALSE]
  data.table::setorderv(out, c(id_col, "ref_date"))

  # if return_all is FALSE, remove non-transitions
  if (!return_all) {
    out <- out[!is.na(to)]
  }

  out[]
}