#' Workforce Transfer UI Module
#'
#' @param id A character string specifying the module ID.
#' @param .data A data frame containing wagebill data.
#'
#' @importFrom bslib layout_sidebar sidebar card card_header popover
#' @importFrom shiny dateRangeInput selectInput actionButton NS
#' @importFrom purrr map discard
#' @importFrom plotly plotlyOutput
#'
#' @return A Shiny module UI function for workforce transfer analytics.
workforce_transfer_ui <- function(id, .data) {
  group_choices <- identify_group_choices(.data) |>
    purrr::map(
      # remove ref_date from choice set
      \(variable_choices) {
        variable_choices |>
          purrr::discard(
            \(x) x == "ref_date"
          )
      }
    )

  bslib::layout_sidebar(
    fillable = FALSE,
    sidebar = bslib::sidebar(
      title = "Controls",
      width = "300px",
      date_ui(id, .data),
      group_filter_ui(id, .data, selected = "paygrade", group_choices),
      subgroup_filter_ui(id, .data),
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
          "The number of internal transfers over time. This is computed as the number of internal transfers over the entire time period, considering as a transfer a movement of personnel across groups between each reference date.",
          title = "Transitions over time",
          placement = "left"
        ),
        class = "d-flex justify-content-between"
      ),
      plotly::plotlyOutput(shiny::NS(id, "transfer_plot"))
    )
  )
}

#' Workforce Transfer Server Module
#'
#' @param id A character string specifying the module ID.
#' @param .data A data frame containing wagebill data.
#' @param cache A list containing pre-computed data for caching.
#'
#' @importFrom shiny moduleServer reactive req bindEvent
#' @importFrom plotly renderPlotly
#' @importFrom dplyr filter between
#' @importFrom tidyr complete
#' @importFrom data.table as.data.table
#' @importFrom govhr detect_career_transitions fastcount
#'
#' @export
#'
#' @return A Shiny server module for workforce transfer analytics.
workforce_transfer_server <- function(id, .data, cache) {
  moduleServer(id, function(input, output, session) {
    update_group_filter_controls(.data, input, session)

    # ignore initial values for group_filter, subgroup_filter, and date_range
    workforce_filtered <- reactive({
       req(input$group_filter != "ref_date")

        .data |>
          filter_data(
            group_filter = input$group_filter,
            subgroup_filter = input$subgroup_filter,
            date_range = input$date_range
          )
    })

    transfer_data <- reactive({
      if (input$group_filter == "paygrade") {
        cache[["transfer_default"]]
      } else {
        workforce_filtered() |>
          as.data.table() |>
          govhr::detect_career_transitions(
            vars = input$group_filter,
            decision_var = "base_salary_lcu"
          ) |>
          govhr::fastcount(
            dplyr::across(
              all_of(
                c("from", "to")
              )
            ),
            name = "transfer"
          ) |>
          tidyr::complete(
            .data[["from"]],
            .data[["to"]],
            fill = list(transfer = 0)
          )
      }
    })

    output$transfer_plot <- plotly::renderPlotly({
      transfer_data() |>
        plot_transfer_heatmap()
    }) |>
      shiny::bindEvent(input$apply_btn, ignoreNULL = FALSE)
  })
}

workforce_transfer_app <- function(.data) {
  shiny::shinyApp(
    ui = workforce_transfer_ui("transfer", .data),
    server = function(input, output, session) {
      workforce_transfer_server("transfer", .data)
    }
  )
}
