#' Identify available grouping choices based on the columns present in the data.
#' @param .data A data frame.
#' 
#' @importFrom dplyr filter summarise pull
#' @importFrom purrr set_names
#' @return A named list of grouping choices, where each element corresponds to a module and contains a named vector of variable IDs and their corresponding variable names.
identify_group_choices <- function(.data){
  available_cols <- names(.data)

  group_choices <- c(
      list("All" = "ref_date"),
      govhr::dictionary |>
        dplyr::filter(
          .data[["variable_id"]] %in%
            available_cols &
            .data[["variable_class"]] == "character" &
            !.data[["variable_id"]] %in%
              c("ref_date", "contract_id", "personnel_id")
        ) |>
        dplyr::summarise(
          choices = list(
            purrr::set_names(.data[["variable_id"]], .data[["variable_name"]])
          ),
          .by = .data[["module"]]
        ) |>
        dplyr::pull(.data[["choices"]], name = .data[["module"]])
    )

  group_choices
}



#' Generate UI controls to filter data.
#' 
#' @param .data Data frame. The data to be filtered.
#' @param id Character string. The module namespace ID.
#'
#' @import shiny
#' @importFrom shinyWidgets pickerInput pickerOptions
#'  
#' @return A list of Shiny UI elements for filtering the data.
default_ui_controls <- function(.data, id) {
  group_choices <- identify_group_choices(.data)

  list(
    shiny::dateRangeInput(
      shiny::NS(id, "date_range"),
      "Select date range:",
      start = min(.data[["ref_date"]], na.rm = TRUE),
      end = max(.data[["ref_date"]], na.rm = TRUE),
      min = min(.data[["ref_date"]], na.rm = TRUE),
      max = max(.data[["ref_date"]], na.rm = TRUE)
    ),
    shiny::selectInput(
      shiny::NS(id, "group_filter"),
      "Select group:",
      choices = group_choices
    ),
    # only show the subgroup filter if a group is selected and it's not "ref_date"
    shiny::conditionalPanel(
      condition = sprintf(
        "input['%s'] !== 'none' && input['%s'] !== 'ref_date'",
        shiny::NS(id, "group_filter"),
        shiny::NS(id, "group_filter")
      ),
      # subgroup filter: dynamically populated based on the selected group
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
  )
}

#' UI for the date range input.
#'
#' @param id Character string. The module namespace ID.
#' @param .data Data frame. The data to be used for determining the date range.
#' 
#' @importFrom shiny dateRangeInput NS
#' 
#' @return A Shiny UI element for selecting a date range.
date_ui <- function(id, .data) {
  shiny::dateRangeInput(
    shiny::NS(id, "date_range"),
    "Select date range:",
    start = min(.data[["ref_date"]], na.rm = TRUE),
    end = max(.data[["ref_date"]], na.rm = TRUE),
    min = min(.data[["ref_date"]], na.rm = TRUE),
    max = max(.data[["ref_date"]], na.rm = TRUE)
  )
}

#' UI for the group filter input.
#' 
#' @param id Character string. The module namespace ID.
#' @param .data Data frame. The data to be used for determining the available grouping choices.
#' @param selected Character string. The default selected group. Defaults to "ref_date".
#' @param group_choices Optional named vector of grouping choices. If NULL, the function will identify available grouping choices from the data.
#' 
#' @importFrom shiny selectInput NS
#' 
#' @return A Shiny UI element for selecting a group filter.
group_filter_ui <- function(id, .data, selected = "ref_date", group_choices = NULL) {
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

#' UI for the subgroup filter input.
#' 
#' @param id Character string. The module namespace ID.
#' @param .data Data frame. The data to be used for determining the available subgroup choices.
#' 
#' @importFrom shiny conditionalPanel NS
#' @importFrom shinyWidgets pickerInput pickerOptions
#' 
#' @return A Shiny UI element for selecting a subgroup filter, conditionally displayed based on the selected group.
subgroup_filter_ui <- function(id, .data) {
  # only show the subgroup filter if a group is selected and it's not "ref_date"
  shiny::conditionalPanel(
    condition = sprintf(
      "input['%s'] !== 'none' && input['%s'] !== 'ref_date'",
      shiny::NS(id, "group_filter"),
      shiny::NS(id, "group_filter")
    ),
    # subgroup filter: dynamically populated based on the selected group
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