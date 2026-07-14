flows_panel_ui <- function(id, .data) {
  bslib::layout_sidebar(
    fillable = FALSE,
    theme = bslib::bs_theme(bootswatch = "litera"),
    sidebar = bslib::sidebar(
      title = span("Controls", bsicons::bs_icon("sliders")),
      width = "300px",
      !!!ui_filter_controls(.data, id),
      shiny::selectInput(
        shiny::NS(id, "measurement_type"),
        label = "Select type of measurement:",
        choices = c("Count" = "count", "Rate" = "rate")
      ),
      shiny::actionButton(
        shiny::NS(id, "apply_btn"),
        "Apply selection",
        icon = shiny::icon("play")
      )
    ),

    # plot 1. hiring counts/rates over time
    bslib::card(
      bslib::card_header(
        "Hiring over time",
        bslib::popover(
          bsicons::bs_icon("info-circle-fill"),
          "The number of new hires and the hiring rate (new hires / total workforce) over time.",
          title = "Hiring over time",
          placement = "left"
        ),
        class = "d-flex justify-content-between"
      ),
      plotly::plotlyOutput(shiny::NS(id, "hiring_plot"))
    ),

    # table 2. demographic characteristics of hires vs. general pop.
    bslib::card(
      bslib::card_header(
        "Profile of new hires",
        bslib::popover(
          bsicons::bs_icon("info-circle-fill"),
          "Compare the characteristics of new hires against the general population, selecting which attributes to compare them with.",
          title = "Profile of new hires",
          placement = "left"
        )
      ),
      gt::gt_output(shiny::NS(id, "hiring_profile"))
    )
  )
}
  
flows_panel_server <- function(id, .data, movement_type) {
  moduleServer(id, function(input, output, session) {
    update_group_filter_controls(.data, input, session)

    data_filtered <- shiny::reactive({
      data <- .data

      if (input$group_filter != "ref_date") {
        data <- data |>
          dplyr::filter(.data[[input$group_filter]] %in% input$subgroup_filter)
      }

      data |>
        dplyr::filter(
          .data[["ref_date"]] >= input$date_range[1],
          .data[["ref_date"]] <= input$date_range[2]
        )
    }) 

    # plot 1. hiring counts/rates over time
    output$hiring_plot <- renderPlotly({
      movement_data <- generate_movement_data(
        .data = data_filtered(),
        movement_type = movement_type,
        measurement_type = input$measurement_type,
        group_cols = input$group_filter
      )

      plot_movement(
        movement_data,
        movement_type = movement_type,
        group_cols = input$group_filter
      )
    }) |>
      bindEvent(input$apply_btn, ignoreNULL = FALSE)

    # plot 2. demographic characteristics of hires vs. general pop.
    output$hiring_profile <- gt::render_gt({
      profile_data <- classify_personnel_event(
        .data = data_filtered(),
        id_col = "personnel_id",
        event_type = movement_type,
        start_date = min(data_filtered()[["ref_date"]]),
        end_date = max(data_filtered()[["ref_date"]]),
        status_col = "employment_status",
        freq = guess_date_frequency(data_filtered())
      )

      profile_data |> 
        gtsummary::tbl_summary(
          by = "type_event",
          include = -c("personnel_id", "ref_date"),
          label = list(
            "gender" = "Gender",
            "educat7" = "Education Level",
            "employment_status" = "Employment Status",
            "age" = "Age"
          ),
          missing = "no"
        ) |>
        gtsummary::modify_header(
          label = "**Characteristics**",
          stat_1 = paste0("**", stringr::str_to_title(movement_type), "**"),
          stat_2 = "**General Population**"
        ) |>
        gtsummary::as_gt() |>
        gt::tab_options(
          table.font.size = "medium",
          table.font.names = "Lato",
          heading.title.font.size = "large"
        )
    }) |>
      bindEvent(input$apply_btn, ignoreNULL = FALSE)
  })
}

run_flows_app <- function(
  workforce_data,
  ...
) {
  theme = bslib::bs_theme(
    bootswatch = "litera"
  )

  ui <- flows_panel_ui("test", workforce_data)

  server <- function(input, output, session) {
    flows_panel_server("test", workforce_data, movement_type = "hire")
  }

  shiny::shinyApp(ui, server, ...)
}
