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
      plotly::plotlyOutput(shiny::NS(id, "progression_plot"))
    )
  )
}

workforce_transfer_server <- function(id, .data) {
  moduleServer(id, function(input, output, session) {
    workforce_filtered <- reactive({
      req(input$apply_btn)
      
      
    })

    transfer_data <- reactive({
      req(filtered_data())
      govhr:::estimate_movement_rates(
        filtered_data(),
        group_cols = input$group_filter
      )
    })

    output$progression_plot <- plotly::renderPlotly({
      req(filtered_data())
      plot_transfer_over_time(filtered_data())
    })
  })
}