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
