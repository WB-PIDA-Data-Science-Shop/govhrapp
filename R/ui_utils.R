#' Nest dictionary variables by module
#'
#' Collapses dictionary rows into the named list of named vectors that Shiny
#' select inputs use to render option groups.
#'
#' @param dictionary_rows Data frame of `govhr::dictionary` rows with
#'   `variable_id`, `variable_name` and `module` columns.
#'
#' @returns A named list keyed by module, each element a named character vector
#'   of variable IDs labeled by variable name.
#'
#' @importFrom dplyr pull summarise
#' @importFrom purrr set_names
#' @keywords internal
nest_choices_by_module <- function(dictionary_rows) {
  dictionary_rows |>
    dplyr::summarise(
      choices = list(
        purrr::set_names(.data[["variable_id"]], .data[["variable_name"]])
      ),
      .by = "module"
    ) |>
    dplyr::pull(.data[["choices"]], name = .data[["module"]])
}

#' Identify the available grouping choices
#'
#' Lists the categorical columns present in the data that can be grouped by,
#' nested by dictionary module. Always includes an "All" option, mapped to
#' `ref_date`, for no grouping.
#'
#' @param .data Data frame whose columns bound the available choices.
#'
#' @returns A named list of grouping choices, keyed by module.
#'
#' @importFrom dplyr filter
#' @keywords internal
identify_group_choices <- function(.data) {
  # colnames() rather than names(), which does not list a tbl_dbi's columns
  available_cols <- colnames(.data)

  c(
    list("All" = "ref_date"),
    govhr::dictionary |>
      dplyr::filter(
        .data[["variable_id"]] %in% available_cols,
        .data[["variable_class"]] == "character",
        # identifiers have one value per record, so grouping by them is useless
        !.data[["variable_id"]] %in%
          c("ref_date", "contract_id", "personnel_id"),
        # est_id is listed in both the contract and establishment modules, and
        # would otherwise appear twice
        !(.data[["variable_id"]] == "est_id" & .data[["module"]] == "Contract")
      ) |>
      nest_choices_by_module()
  )
}

#' Identify the choices of an optional column selection
#'
#' Lists the same categorical columns as [identify_group_choices()], with
#' "None" in place of "All", for selections that can be left unset, such as
#' the population filter and the "Shares by:" breakdown.
#'
#' @inheritParams identify_group_choices
#'
#' @returns A named list of column choices, keyed by module, with `"none"` for
#'   no selection.
#'
#' @keywords internal
identify_choices <- function(.data) {
  group_choices <- identify_group_choices(.data)

  c(
    list("None" = "none"),
    group_choices[names(group_choices) != "All"]
  )
}

#' Read the range of reference dates
#'
#' @param .data Data frame or database table (`tbl_dbi`) with a `ref_date`
#'   column.
#'
#' @returns A Date vector of the earliest and latest `ref_date`.
#'
#' @importFrom dplyr collect summarise
#' @keywords internal
read_date_range <- function(.data) {
  if (inherits(.data, "tbl_lazy")) {
    bounds <- .data |>
      dplyr::summarise(
        first_date = min(.data[["ref_date"]], na.rm = TRUE),
        last_date = max(.data[["ref_date"]], na.rm = TRUE)
      ) |>
      dplyr::collect()

    # some databases return dates as text or day counts rather than as Dates
    as.Date(
      c(bounds[["first_date"]], bounds[["last_date"]])
    )
  } else {
    return(
      range(.data[["ref_date"]], na.rm = TRUE)
    )
  }
}

#' Date range filter input
#'
#' @param id Character. Module namespace ID.
#' @param .data Data frame or database table (`tbl_dbi`) whose `ref_date`
#'   column bounds the selectable range.
#'
#' @returns A Shiny date range input.
#'
#' @importFrom shiny NS dateRangeInput
#' @keywords internal
date_ui <- function(id, .data) {
  date_range <- read_date_range(.data)

  shiny::dateRangeInput(
    shiny::NS(id, "date_range"),
    "Filter date",
    start = date_range[1],
    end = date_range[2],
    min = date_range[1],
    max = date_range[2]
  )
}

#' Group filter input
#'
#' @param id Character. Module namespace ID.
#' @param .data Data frame whose columns bound the available choices.
#' @param selected Character. Grouping column selected at first. Default
#'   `"ref_date"`, for no grouping.
#' @param group_choices Named list of grouping choices, or `NULL` (default) to
#'   derive them from `.data` with [identify_group_choices()].
#'
#' @returns A Shiny select input.
#'
#' @importFrom shiny NS selectInput
#' @keywords internal
group_filter_ui <- function(
  id,
  .data,
  selected = "ref_date",
  group_choices = NULL
) {
  if (is.null(group_choices)) {
    group_choices <- identify_group_choices(.data)
  }

  shiny::selectInput(
    shiny::NS(id, "group_filter"),
    "Group by:",
    selected = selected,
    choices = group_choices
  )
}

#' Value picker input
#'
#' Renders a picker of the values of the column selected in another input,
#' shown only once a column is selected. Its choices are filled in on the
#' server by [update_filter_controls()].
#'
#' @param id Character. Module namespace ID.
#' @param column_input Character. ID of the input selecting the column.
#' @param values_input Character. ID of the picker.
#' @param label Character. Picker label.
#' @param noun Character. Plural name of the values, for the picker's summary
#'   of how many are selected.
#'
#' @returns A Shiny conditional panel wrapping a picker input.
#'
#' @importFrom shiny NS conditionalPanel
#' @importFrom shinyWidgets pickerInput pickerOptions
#' @keywords internal
value_picker_ui <- function(id, column_input, values_input, label, noun) {
  shiny::conditionalPanel(
    condition = sprintf(
      "input['%1$s'] !== 'none' && input['%1$s'] !== 'ref_date'",
      shiny::NS(id, column_input)
    ),
    shinyWidgets::pickerInput(
      shiny::NS(id, values_input),
      label,
      choices = NULL,
      multiple = TRUE,
      options = shinyWidgets::pickerOptions(
        actionsBox = TRUE,
        liveSearch = TRUE,
        selectedTextFormat = "count > 3",
        countSelectedText = paste("{0}", noun, "selected"),
        noneSelectedText = paste("No", noun, "selected"),
        container = "body"
      )
    )
  )
}

#' Subgroup filter input
#'
#' Picks which subgroups of the "Group by" column are shown.
#'
#' @param id Character. Module namespace ID.
#'
#' @returns A Shiny conditional panel wrapping a picker input.
#'
#' @keywords internal
subgroup_filter_ui <- function(id) {
  value_picker_ui(
    id,
    column_input = "group_filter",
    values_input = "subgroup_filter",
    label = "Subgroups",
    noun = "subgroups"
  )
}

#' Population filter inputs
#'
#' Restricts the panel to records with chosen values of any categorical
#' column, separately from how the panel is grouped.
#'
#' @param id Character. Module namespace ID.
#' @param .data Data frame whose columns bound the available choices.
#'
#' @returns A list of a Shiny select input and a conditional picker.
#'
#' @importFrom shiny NS selectInput
#' @keywords internal
population_filter_ui <- function(id, .data) {
  list(
    shiny::selectInput(
      shiny::NS(id, "filter_col"),
      "Filter by:",
      choices = identify_choices(.data),
      selected = "none"
    ),
    value_picker_ui(
      id,
      column_input = "filter_col",
      values_input = "filter_values",
      label = "Values",
      noun = "values"
    )
  )
}

#' Default sidebar filter controls
#'
#' Assembles the date, population filter, group and subgroup controls shared by
#' every analytics and quality check panel sidebar.
#'
#' @param id Character. Module namespace ID.
#' @param .data Data frame to be filtered.
#'
#' @returns A list of Shiny UI elements.
#'
#' @keywords internal
default_ui_controls <- function(id, .data) {
  c(
    list(date_ui(id, .data)),
    population_filter_ui(id, .data),
    list(
      shiny::hr(),
      group_filter_ui(id, .data),
      subgroup_filter_ui(id)
    )
  )
}
