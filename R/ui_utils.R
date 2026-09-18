#' Nest Dictionary Variables by Module
#'
#' Collapses dictionary rows into the named list of named vectors that Shiny
#' select inputs use to render optgroups.
#'
#' @param dictionary_rows Data frame of `govhr::dictionary` rows with
#'   `variable_id`, `variable_name` and `module` columns.
#'
#' @return A named list keyed by module, each element a named character vector
#'   of variable IDs labelled by variable name.
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

#' Identify Available Grouping Choices
#'
#' Lists the categorical variables present in the data that can be used as a
#' grouping dimension, nested by dictionary module. Always includes an "All"
#' option mapped to `ref_date`.
#'
#' @param .data Data frame whose columns bound the available choices.
#'
#' @return A named list of grouping choices, keyed by module.
#'
#' @importFrom dplyr filter
#' @keywords internal
identify_group_choices <- function(.data) {
  available_cols <- names(.data)

  c(
    list("All" = "ref_date"),
    govhr::dictionary |>
      dplyr::filter(
        .data[["variable_id"]] %in% available_cols,
        .data[["variable_class"]] == "character",
        # exclude id columns that are not suitable for grouping
        !.data[["variable_id"]] %in%
          c("ref_date", "contract_id", "personnel_id"),
        # exclude est_id in the contract module to avoid est_id duplication with
        # the estabilshment module
        !(.data[["variable_id"]] == "est_id" & .data[["module"]] == "Contract")
      ) |>
      nest_choices_by_module()
  )
}

#' Date Range Filter Input
#'
#' @param id Character. Module namespace ID.
#' @param .data Data frame whose `ref_date` column bounds the selectable range.
#'
#' @return A Shiny date range input.
#'
#' @importFrom shiny NS dateRangeInput
#' @keywords internal
date_ui <- function(id, .data) {
  date_range <- range(.data[["ref_date"]], na.rm = TRUE)

  shiny::dateRangeInput(
    shiny::NS(id, "date_range"),
    "Select date range:",
    start = date_range[1],
    end = date_range[2],
    min = date_range[1],
    max = date_range[2]
  )
}

#' Group Filter Input
#'
#' @param id Character. Module namespace ID.
#' @param .data Data frame whose columns bound the available choices.
#' @param selected Character. Grouping column selected by default. Default
#'   `"ref_date"`.
#' @param group_choices Named list of grouping choices. Default `NULL`, which
#'   derives them from `.data` via [identify_group_choices()].
#'
#' @return A Shiny select input.
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
    "Select group:",
    selected = selected,
    choices = group_choices
  )
}

#' Subgroup Filter Input
#'
#' Renders the subgroup picker, shown only once a grouping column other than
#' `ref_date` is selected. Choices are populated server-side by
#' [update_group_filter_controls()].
#'
#' @param id Character. Module namespace ID.
#'
#' @return A Shiny conditional panel wrapping a picker input.
#'
#' @importFrom shiny NS conditionalPanel
#' @importFrom shinyWidgets pickerInput pickerOptions
#' @keywords internal
subgroup_filter_ui <- function(id) {
  shiny::conditionalPanel(
    condition = sprintf(
      "input['%s'] !== 'none' && input['%s'] !== 'ref_date'",
      shiny::NS(id, "group_filter"),
      shiny::NS(id, "group_filter")
    ),
    shinyWidgets::pickerInput(
      shiny::NS(id, "subgroup_filter"),
      "Select subgroups:",
      choices = NULL,
      multiple = TRUE,
      options = shinyWidgets::pickerOptions(
        actionsBox = TRUE,
        liveSearch = TRUE,
        selectedTextFormat = "count > 3",
        countSelectedText = "{0} subgroups selected",
        noneSelectedText = "No subgroups selected",
        container = "body"
      )
    )
  )
}

#' Default Sidebar Filter Controls
#'
#' Assembles the date, group and subgroup controls shared by every analytics
#' panel sidebar.
#'
#' @param .data Data frame to be filtered.
#' @param id Character. Module namespace ID.
#'
#' @return A list of Shiny UI elements.
#'
#' @keywords internal
default_ui_controls <- function(.data, id) {
  list(
    date_ui(id, .data),
    group_filter_ui(id, .data),
    subgroup_filter_ui(id)
  )
}
