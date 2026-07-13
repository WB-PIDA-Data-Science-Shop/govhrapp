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
