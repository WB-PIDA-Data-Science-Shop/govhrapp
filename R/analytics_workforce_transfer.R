workforce_transfer_ui <- function(id, .data) {
  bslib::layout_sidebar(
    fillable = FALSE,
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
          "The number of internal transfers over time. The rate is computed as the number of internal transfers divided by the total workforce at the beginning of each period.",
          title = "Transitions over time",
          placement = "left"
        ),
        class = "d-flex justify-content-between"
      ),
      plotly::plotlyOutput(shiny::NS(id, "transfer_plot"))
    )
  )
}

workforce_transfer_server <- function(id, .data) {
  moduleServer(id, function(input, output, session) {
    update_group_filter_controls(.data, input, session)

    workforce_filtered <- reactive({
      filter_data(
        .data,
        group_filter = input$group_filter,
        subgroup_filter = input$subgroup_filter,
        date_range = input$date_range
      )
    })

    transfer_data <- reactive({
      req(input$apply_btn)

      workforce_filtered() |>
      as.data.table() |>
      govhr:::detect_career_transitions(
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
        )
    })

    output$transfer_plot <- plotly::renderPlotly({
      transfer_data() |>
        plot_transfer_heatmap()
    })
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