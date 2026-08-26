#' Workforce Transition UI
#'
#' @param id A character string specifying the module ID.
#' @param .data A data frame containing workforce data.
#'
#' @import shiny
#' @import bslib
#' @importFrom plotly plotlyOutput
#'
#' @return A Shiny UI function for the workforce transition module.
workforce_transition_ui <- function(id, .data) {
  bslib::layout_sidebar(
    fillable = FALSE,
    theme = bslib::bs_theme(bootswatch = "litera"),
    sidebar = bslib::sidebar(
      title = "Controls",
      width = "300px",
      !!!default_ui_controls(.data, id),
      shiny::actionButton(
        shiny::NS(id, "apply_btn"),
        "Apply selection",
        icon = shiny::icon("play")
      )
    ),
    bslib::card(
      full_screen = TRUE,
      bslib::card_header(
        "Transitions over time",
        bslib::popover(
          bsicons::bs_icon("info-circle-fill"),
          "The number of promotions and rate (promotions / total workforce) over time. The rate is computed as the number of promotions divided by the total workforce at the beginning of each period.",
          title = "Transitions over time",
          placement = "left"
        ),
        class = "d-flex justify-content-between"
      ),
      plotly::plotlyOutput(shiny::NS(id, "progression_plot"))
    )
  )
}
