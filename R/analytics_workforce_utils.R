#' Detect Career Transitions
#' 
#' @param .data A data frame containing workforce data.
#' @param id_col A character string specifying the column name for the unique identifier (default is "contract_id").
#' @param group_cols A character vector specifying the column names for grouping (e.g., paygrade, department).
#' @param return_all A logical value indicating whether to return all records (including non-transitions) or only transitions (default is FALSE).
#' 
#' @importFrom data.table as.data.table setorderv rleidv shift setnames
#' @importFrom stats complete.cases
#' @return A data frame containing detected career transitions with columns for the unique identifier, from group, to group, and reference date.
#' 
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