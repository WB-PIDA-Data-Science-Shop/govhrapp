#' Detect Career Transitions
#'
#' Collapses each entity's history into spells of consecutive periods in the
#' same group, then pairs each spell with the one that follows it. A row is
#' dated by `ref_date`, the period in which the entity is first observed in the
#' destination group, so a transition is counted when it happens. `from_date`
#' records when the origin spell began.
#'
#' Records must be uniquely identified by `id_col` and `ref_date`. An entity
#' holding more than one record in the same period has no well-defined position,
#' so every record for that entity is dropped with a warning reporting how many.
#'
#' @param .data Data frame containing a `ref_date` column, the identifier and
#'   the grouping columns.
#' @param id_col Character. Column identifying the entity whose career is
#'   tracked. Default `"contract_id"`.
#' @param group_cols Character vector of columns defining the career position
#'   (e.g. paygrade, department). Multiple columns are pasted into one label.
#' @param return_all Logical. Keep terminal spells, which have no destination
#'   and so carry `NA` in both `to` and `ref_date`. Default `FALSE`.
#'
#' @return A data table with the identifier, `from`, `to`, `from_date` and
#'   `ref_date`.
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

  # spells assume one position per entity per period. an entity holding several
  # records in the same period has no well-defined position, so drop the entity
  # outright rather than pick one of its records arbitrarily. checked after the
  # complete.cases filter, so records already dropped for missingness are not
  # counted as violations
  duplicate_keys <- duplicated(dt[, c(id_col, "ref_date"), with = FALSE])

  if (any(duplicate_keys)) {
    violating_ids <- unique(dt[[id_col]][duplicate_keys])
    violating_rows <- dt[[id_col]] %in% violating_ids

    warning(
      sprintf(
        paste(
          "%d record(s) are not uniquely identified by `%s` and `ref_date`;",
          "dropping all %d record(s) for the %d affected `%s` value(s)."
        ),
        sum(duplicate_keys),
        id_col,
        sum(violating_rows),
        length(violating_ids),
        id_col
      ),
      call. = FALSE
    )

    dt <- dt[!violating_rows]
  }

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

  data.table::setnames(
    spells,
    c(group_cols, "ref_date"),
    c("from", "from_date")
  )

  # date the transition by the destination spell, not the origin one: dating it
  # by the start of the `from` spell backdates every move to the period the
  # entity entered its previous group, which piles all first moves onto the
  # earliest date in the panel and starves the latest one
  spells[
    ,
    c("to", "ref_date") := list(
      data.table::shift(from, type = "lead"),
      data.table::shift(from_date, type = "lead")
    ),
    by = id_col
  ]

  out <- spells[
    , c(id_col, "from", "to", "from_date", "ref_date"), with = FALSE
  ]
  # terminal spells have no `ref_date`, so order on the always-present date
  data.table::setorderv(out, c(id_col, "from_date"))

  # if return_all is FALSE, remove non-transitions
  if (!return_all) {
    out <- out[!is.na(to)]
  }

  out[]
}