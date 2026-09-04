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
      shiny::selectInput(
        shiny::NS(id, "id_col"),
        "Identifier",
        choices = c("Personnel" = "personnel_id", "Contract" = "contract_id"),
        selected = "personnel_id"
      ),
      shiny::actionButton(
        shiny::NS(id, "apply_btn"),
        "Apply selection",
        icon = shiny::icon("play")
      )
    ),
    # plot 1. transfers over time
    bslib::card(
      full_screen = TRUE,
      bslib::card_header(
        "Transfers over time",
        bslib::popover(
          bsicons::bs_icon("info-circle-fill"),
          "The number of internal transfers over time. This is computed as the number of internal transfers over the entire time period, considering as a transfer a movement of personnel across groups between each reference date.",
          title = "Transfers over time",
          placement = "left"
        ),
        class = "d-flex justify-content-between"
      ),
      plotly::plotlyOutput(shiny::NS(id, "transfer_trend_plot"))
    ),

    # plot 2. heatmap
    bslib::card(
      full_screen = TRUE,
      bslib::card_header(
        "Transfers over time",
        bslib::popover(
          bsicons::bs_icon("info-circle-fill"),
          "The number of internal transfers over time. This is computed as the number of internal transfers over the entire time period, considering as a transfer a movement of personnel across groups between each reference date.",
          title = "Transfers over time",
          placement = "left"
        ),
        class = "d-flex justify-content-between"
      ),
      plotly::plotlyOutput(shiny::NS(id, "transfer_network_plot"))
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
#' @importFrom dplyr filter between across all_of
#' @importFrom tidyr complete
#' @importFrom data.table as.data.table
#' @importFrom govhr detect_career_transitions fastcount
#' @importFrom purrr pluck
#'
#' @export
#'
#' @return A Shiny server module for workforce transfer analytics.
workforce_transfer_server <- function(id, .data, cache) {
  moduleServer(id, function(input, output, session) {
    update_group_filter_controls(.data, input, session)

    # ignore initial values for group_filter, subgroup_filter, and date_range
    workforce_filtered <- reactive({
      req(input$apply_btn)

      .data |>
        filter_data(
          group_filter = input$group_filter,
          subgroup_filter = input$subgroup_filter,
          date_range = input$date_range
        )
    })

    transfer_data <- reactive({
      # only use cache if no apply button has been clicked
      if (input$apply_btn == 0) {
        cache |>
          purrr::pluck("workforce", "workforce_transfer")
      } else {
        workforce_filtered() |>
          detect_career_transition(
            id_col = input$id_col,
            group_cols = input$group_filter
          )
      }
    })

    # plot 1. transfers over time
    output$transfer_trend_plot <- plotly::renderPlotly({
      # use cache if default group is selected
      if (input$apply_btn == 0) {
        cache |>
          purrr::pluck("workforce", "workforce_transfer") |>
          govhr::fastcount(
            ref_date,
            name = "transfer"
          ) |>
          plot_trend(
            group = "ref_date",
            y_col = "transfer",
            y_label = "Number of Transfers"
          )
      } else {
        transfer_data() |>
          govhr::fastcount(
            ref_date,
            name = "transfer"
          ) |>
          plot_trend(
            group = "ref_date",
            y_col = "transfer",
            y_label = "Number of Transfers"
          )
      }
    }) |>
      shiny::bindEvent(input$apply_btn, ignoreNULL = FALSE)

    # plot 2. transfer network
    output$transfer_network_plot <- plotly::renderPlotly({
      if (input$apply_btn == 0) {
        cache |>
          purrr::pluck("workforce", "workforce_transfer") |>
          plotly_transfer_network()
      } else {
        transfer_data() |>
          plotly_transfer_network()
      }
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
