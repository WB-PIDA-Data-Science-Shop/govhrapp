#' Update filter controls based on selected group variable
#'
#' @param .data A data frame containing the data to be filtered.
#' @param input A Shiny input object containing the selected group variable.
#' @param session A Shiny session object.
#'
#' @importFrom shiny observe
#' @importFrom shinyWidgets updatePickerInput
#' @return None. This function updates the filter controls in the Shiny UI.
update_group_filter_controls <- function(.data, input, session) {
  shiny::observe({
    variable <- input$group_filter

    if (is.null(variable) || variable == "none") {
      shinyWidgets::updatePickerInput(
        session,
        "subgroup_filter",
        choices = NULL,
        selected = character(0)
      )
    } else {
      filter_vals <- sort(
        as.character(
          unique(
            stats::na.omit(.data[[variable]])
          )
        )
      )

      shinyWidgets::updatePickerInput(
        session,
        "subgroup_filter",
        choices = filter_vals,
        selected = filter_vals
      )
    }
  })
}

#' Filter data based on group, subgroup, and date range inputs
#' 
#' @param data A data frame containing the data to be filtered.
#' @param group_filter A character string specifying the group variable to filter by.
#' @param subgroup_filter A character vector specifying the subgroups to filter by.
#' @param date_range A vector of two dates specifying the date range to filter by.
#' 
#' @importFrom dplyr filter
#' @return A filtered data frame based on the specified inputs.
filter_data <- function(data, group_filter, subgroup_filter, date_range) {
  if (group_filter != "ref_date") {
    data <- data |>
      dplyr::filter(.data[[group_filter]] %in% subgroup_filter)
  }

  if (!is.null(date_range)) {
    data <- data |>
      dplyr::filter(
        .data[["ref_date"]] >= date_range[1],
        .data[["ref_date"]] <= date_range[2]
      )
  }

  data
}

# meso table -------------------------------------------------------------
#' Nest Data into Meso-Level Cells
#'
#' Internal helper that reshapes a data frame into "meso" cells: one row per
#' combination of `ref_date`, grouping variable (`group_var`), and grouping
#' value (`subgroup`). The raw rows belonging to each cell are kept in a
#' list-column (`cell_data`), ready for functions to be mapped onto them.
#'
#' @param data A data frame containing at least `ref_date`.
#' @param group_vars Character vector of column names in `data` to build cells
#'   for. Use `"ref_date"` to represent the whole-population (ungrouped) cell.
#' @param measure_cols Optional character string naming a column in `data` to be used as the measure for the wage bill meso table.
#'
#' @return A data frame with columns `ref_date`, `group_var`, `subgroup`, and
#'   a list-column `cell_data`.
#'
#' @importFrom dplyr mutate filter
#' @importFrom tidyr nest
#' @importFrom purrr map_dfr
#' @keywords internal
nest_meso_cells <- function(data, group_vars, measure_cols = NULL) {
  purrr::map_dfr(group_vars, function(group_var) {
    if (group_var != "ref_date" && !group_var %in% names(data)) {
      return(NULL)
    }

    data |>
      dplyr::mutate(
        subgroup = if (group_var == "ref_date") {
          NA_character_
        } else {
          as.character(.data[[group_var]])
        },
        group_var = group_var,
        # keep a duplicate of `ref_date` as the nest key so that the original
        # `ref_date` column is retained inside `cell_data`; several downstream
        # helpers (e.g. `compute_decile()`) expect a `ref_date` column even
        # when the cell already represents a single reference date.
        .ref_date_key = .data[["ref_date"]]
      ) |>
      dplyr::filter(group_var == "ref_date" | !is.na(subgroup)) |>
      tidyr::nest(cell_data = -c(".ref_date_key", "group_var", "subgroup")) |>
      dplyr::rename(ref_date = ".ref_date_key")
  })
}

#' Look Up a Slice of a Meso Table
#'
#' Filters a meso table (as produced by [build_meso_table()],
#' [build_workforce_meso_table()], or [build_wagebill_meso_table()]) down to
#' the rows needed to drive a single plot: one grouping variable, optionally
#' a set of subgroups, and optionally a reference-date range. This is the
#' core building block that lets plot pipelines simply *look up*
#' pre-computed indicators (`headcount`, `wagebill`, `percentile_distribution`,
#' `decile_distribution`, ...) instead of recomputing them from raw
#' microdata every time a control changes.
#'
#' @param meso_table A meso table (or a slice of one).
#' @param group_var Character string naming the grouping variable to look up,
#'   e.g. `"ref_date"`, `"paygrade"`, or `"gender"`.
#' @param subgroup_filter Character vector of subgroup values to keep, or
#'   `NULL`/`character(0)` to keep every subgroup available for `group_var`.
#'   Ignored when `group_var == "ref_date"`.
#' @param date_range A length-2 vector of dates (or values coercible to
#'   `Date`) giving the inclusive range of `ref_date` to keep, or `NULL` to
#'   keep every reference date.
#'
#' @return The subset of `meso_table` matching `group_var` (and, if supplied,
#'   `subgroup_filter` and `date_range`).
#'
#' @importFrom dplyr filter
#' @export
lookup_meso_table <- function(
  meso_table,
  group_var,
  subgroup_filter = NULL,
  date_range = NULL
) {
  # NOTE: `.env[["group_var"]]` (rather than the bare `group_var`) is required
  # here because the meso table itself has a column literally named
  # "group_var"; inside `dplyr::filter()`, a bare `group_var` would resolve
  # to that data-variable (data-mask precedence), turning this into a
  # self-comparison that always returns `TRUE` instead of filtering.
  out <- meso_table |>
    dplyr::filter(.data[["group_var"]] == .env[["group_var"]])

  if (group_var != "ref_date" && length(subgroup_filter) > 0) {
    out <- out |>
      dplyr::filter(.data[["subgroup"]] %in% subgroup_filter)
  }

  if (!is.null(date_range)) {
    out <- out |>
      dplyr::filter(
        .data[["ref_date"]] >= date_range[1],
        .data[["ref_date"]] <= date_range[2]
      )
  }

  out
}

#' Label the Subgroup Column of a Meso Table Slice
#'
#' Renames the generic `subgroup` column produced by [nest_meso_cells()] /
#' [lookup_meso_table()] to the actual grouping variable name (e.g.
#' `"paygrade"`), so the result can be passed directly to plotting helpers
#' such as [plot_trend()], [plot_bar_total()], [plot_bar_growth()], and
#' [apply_baseline_index()], all of which expect group values to live in a
#' column named after the grouping variable. When `group_var` is
#' `"ref_date"` (no grouping), `subgroup` is always `NA` and is dropped
#' instead of renamed.
#'
#' @param meso_slice A data frame containing a `subgroup` column, as returned
#'   by [lookup_meso_table()].
#' @param group_var Character string naming the grouping variable.
#'
#' @return `meso_slice` with `subgroup` renamed to `group_var` (or dropped, if
#'   `group_var == "ref_date"`).
#'
#' @importFrom dplyr rename select
#' @export
label_subgroup <- function(meso_slice, group_var) {
  if (group_var == "ref_date") {
    meso_slice |>
      dplyr::select(-"subgroup")
  } else {
    meso_slice |>
      dplyr::rename(!!group_var := "subgroup")
  }
}

#' Compute a Growth Rate from a Meso Table Slice
#'
#' Computes the percentage change in `value_col` between the earliest and
#' latest reference dates present in `meso_slice`, separately for each
#' subgroup. Operates on already-aggregated meso-level indicators (e.g.
#' `headcount`, `wagebill`) rather than raw microdata, since a meso table
#' slice already has one row per `ref_date`/subgroup combination.
#'
#' @param meso_slice A data frame returned by [lookup_meso_table()], already
#'   labeled via [label_subgroup()] so that it contains a column named
#'   `group_var`.
#' @param group_var Character string naming the grouping column.
#' @param value_col Character string naming the indicator column to compare,
#'   e.g. `"headcount"` or `"wagebill"`.
#'
#' @return A data frame with `group_var` and a `growth_rate` column
#'   (percentage points, e.g. `12.5` for +12.5%).
#'
#' @importFrom dplyr filter arrange summarise all_of first last
#' @export
meso_growth_summary <- function(meso_slice, group_var, value_col) {
  meso_slice |>
    dplyr::filter(
      .data[["ref_date"]] %in%
        c(min(.data[["ref_date"]]), max(.data[["ref_date"]])),
      .by = dplyr::all_of(group_var)
    ) |>
    dplyr::arrange(.data[["ref_date"]]) |>
    dplyr::filter(!is.na(.data[[group_var]])) |>
    dplyr::summarise(
      growth_rate = round(
        dplyr::last(.data[[value_col]]) / dplyr::first(.data[[value_col]]) - 1,
        3
      ) *
        100,
      .by = dplyr::all_of(group_var)
    ) |>
    dplyr::filter(!is.na(.data[["growth_rate"]]))
}

#' Build a Workforce Meso Table
#'
#' Builds the workforce half of a meso table (see [build_meso_table()]):
#' one row per `ref_date`/`group_var`/`subgroup` cell, with one column per
#' entry in `scalars`. Standalone from [build_wagebill_meso_table()] since
#' workforce indicators (e.g. headcount) don't depend on a wagebill measure.
#'
#' @param workforce_data A data frame with workforce/personnel attributes,
#'   containing `ref_date`.
#' @param group_vars Character vector of grouping columns to build meso cells
#'   for. Defaults to `"ref_date"` plus every character grouping column
#'   identified by [identify_group_choices()].
#' @param indicators A named list of functions, each taking a single-cell data
#'   frame and returning a single scalar. Defaults to `headcount`, the count
#'   of distinct `personnel_id`.
#'
#' @return A tidy data frame with `ref_date`, `group_var`, `subgroup`, and one
#'   column per entry in `indicators`.
#'
#' @importFrom dplyr select distinct
#' @importFrom purrr map_dbl
#' @export
build_workforce_meso_table <- function(
  workforce_data,
  group_vars = NULL,
  indicators = NULL
) {
  if (is.null(group_vars)) {
    group_vars <- union(
      "ref_date",
      unlist(identify_group_choices(workforce_data), use.names = FALSE)
    )
  }

  if(is.null(indicators)) {
    indicators <- list(
      headcount = function(data) {
        data |>
          dplyr::distinct(.data[["personnel_id"]]) |>
          nrow()
      }
    )
  }

  workforce_meso <- nest_meso_cells(workforce_data, group_vars)

  for (indicator in names(indicators)) {
    workforce_meso[[indicator]] <- purrr::map_dbl(
      workforce_meso[["cell_data"]],
      indicators[[indicator]]
    )
  }

  workforce_meso |>
    dplyr::select(-"cell_data")
}

#' Build a Wagebill Meso Table
#'
#' Builds the wagebill half of a meso table (see [build_meso_table()]): one
#' row per `ref_date`/`group_var`/`subgroup` cell, with a `wagebill` scalar
#' plus one list-column per entry in `vectors`. Standalone from
#' [build_workforce_meso_table()] so that it can be rebuilt on its own
#' whenever `wagebill_measure` changes, without touching workforce data.
#'
#' @param wagebill_data A data frame with contract/salary attributes,
#'   containing `ref_date` and `wagebill_measure`.
#' @param group_vars Character vector of grouping columns to build meso cells
#'   for. Defaults to `"ref_date"` plus every character grouping column
#'   identified by [identify_group_choices()]
#'   `100`.
#' @param indicators A named list of functions, each taking a single-cell data
#'   frame and returning a vector. Defaults to `wagebill`, the sum of
#'   `wagebill_measure` columns.
#'
#' @return A tidy data frame with `ref_date`, `group_var`, `subgroup`,
#'   `wagebill`, and one list-column per entry in `vectors`.
#'
#' @importFrom dplyr select
#' @importFrom purrr map map_dbl
#' @export
build_wagebill_meso_table <- function(
  wagebill_data,
  group_vars = NULL,
  indicators = NULL
) {
  if (is.null(group_vars)) {
    group_vars <- union(
      "ref_date",
      unlist(identify_group_choices(wagebill_data), use.names = FALSE)
    )
  }

  indicators <- list(
    wagebill = function(data) {
      collapse::fsum(data[[wagebill_measure]], na.rm = TRUE)
    }
  )

  wagebill_measure_cols <- identify_wagebill_choices(wagebill_data) |>
      unlist(use.names = FALSE)

  wagebill_meso <- nest_meso_cells(
    wagebill_data, group_vars
  )

  for (wagebill_measure in wagebill_measure_cols) {
    for (indicator in names(indicators)) {
      wagebill_meso[[paste0(indicator, "_", wagebill_measure)]] <- purrr::map_dbl(
        wagebill_meso[["cell_data"]],
        function(cell) {
          indicators[[indicator]](cell)
        }
      )
    }
  }

  wagebill_meso |>
    dplyr::select(-"cell_data")
}

#' Build a Meso-Level Summary Table
#'
#' Consolidates the repeated "filter data -> compute summary -> feed a plot"
#' pipelines found throughout the govhr dashboard servers (see e.g.
#' [workforce_overview_server()], [wagebill_overview_server()],
#' [wagebill_equity_server()]) into a single tidy table. Each row of the
#' resulting table represents one meso-level slice of the data: a reference
#' date, a grouping variable, and a value of that grouping variable (a
#' "subgroup"). Each column is either a scalar *indicator* (e.g. `headcount`,
#' `wagebill`) or a nested *data object* (e.g. `percentile_distribution`,
#' `decile_distribution`) computed for that particular slice.
#'
#' Internally, this combines [build_workforce_meso_table()] and
#' [build_wagebill_meso_table()]; call those directly when only one side is
#' needed (e.g. to avoid rebuilding workforce indicators every time a
#' wagebill measure changes).
#'
#' @param workforce_data A data frame with workforce/personnel attributes
#'   (headcount), containing `ref_date`.
#' @param wagebill_data A data frame with contract/salary attributes (wage
#'   bill), containing `ref_date` and `wagebill_measure`.
#' @param group_vars Character vector of grouping columns to build meso cells
#'   for, e.g. `c("ref_date", "paygrade", "gender")`. Defaults to `"ref_date"`
#'   plus every character grouping column common to both `workforce_data` and
#'   `wagebill_data`, as identified by [identify_group_choices()].
#' @param wagebill_measure Character string naming the wage column to
#'   summarize in `wagebill_data`. Default `"gross_salary_lcu"`.
#' @param percentile_binwidth Numeric bin width passed to
#'   `compute_percentile()` when building `percentile_distribution`. Default
#'   `100`.
#' @param scalars A named list of functions, each taking a single-cell data
#'   frame (all rows for one `ref_date`/`group_var`/`subgroup` combination,
#'   from `workforce_data`) and returning a single scalar. Defaults to
#'   `headcount`, the row count.
#' @param vectors A named list of functions, each taking a single-cell data
#'   frame (from `wagebill_data`) and returning an arbitrary vector (typically
#'   a data frame) to store in a list-column. Defaults compute
#'   `percentile_distribution` via `compute_percentile()` and
#'   `decile_distribution` via `compute_decile()`.
#'
#' @return A tidy data frame (the "meso table") with one row per
#'   `ref_date` / `group_var` / `subgroup` combination, containing:
#'   \itemize{
#'     \item `ref_date`, `group_var`, `subgroup`: row identifiers.
#'     \item One column per entry in `indicators` (scalar), plus `wagebill`
#'       (scalar, sum of `wagebill_measure`).
#'     \item One list-column per entry in `objects`, each holding a nested
#'       data object for that particular slice.
#'   }
#'
#' @examples
#' \dontrun{
#' meso_table <- build_meso_table(workforce_data, wagebill_data)
#'
#' # a single slice's percentile distribution:
#' meso_table$percentile_distribution[[1]]
#'
#' # look up just the "paygrade" trend, without recomputing anything:
#' lookup_meso_table(meso_table, group_var = "paygrade")
#' }
#'
#' @importFrom dplyr full_join
#' @export
build_meso_table <- function(
  workforce_data,
  wagebill_data,
  group_vars = NULL
) {
  # identify available group_vars
  if (is.null(group_vars)) {
    workforce_vars <- unlist(identify_group_choices(workforce_data), use.names = FALSE)
    wagebill_vars <- unlist(identify_group_choices(wagebill_data), use.names = FALSE)
    group_vars <- union("ref_date", intersect(workforce_vars, wagebill_vars))
  }

  workforce_cells <- build_workforce_meso_table(workforce_data, group_vars)

  wagebill_cells <- build_wagebill_meso_table(
    wagebill_data,
    group_vars
  )

  dplyr::full_join(
    workforce_cells,
    wagebill_cells,
    by = c("ref_date", "group_var", "subgroup")
  )
}
