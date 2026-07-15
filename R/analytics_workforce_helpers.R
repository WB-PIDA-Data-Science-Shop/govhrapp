workforce_movement_ui <- function(
  id,
  .data,
  type_movement = c("hire", "fire")
) {
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

    # plot 1. counts and rates over time
    bslib::card(
      bslib::card_header(
        sprintf("%ss over time", stringr::str_to_title(type_movement)),
        bslib::popover(
          bsicons::bs_icon("info-circle-fill"),
          sprintf(
            "The number of new %ss and rate (new %ss / total workforce) over time. The rate is computed as the number of new %ss divided by the total workforce at the beginning of each period.",
            type_movement,
            type_movement,
            type_movement
          ),
          title = sprintf("New %ss over time", type_movement),
          placement = "left"
        ),
        class = "d-flex justify-content-between"
      ),
      plotly::plotlyOutput(shiny::NS(id, sprintf("%s_plot", type_movement)))
    ),

    # table 2. demographic characteristics of hires vs. general pop.
    bslib::card(
      bslib::card_header(
        sprintf("Profile of new %ss", type_movement),
        bslib::popover(
          bsicons::bs_icon("info-circle-fill"),
          sprintf(
            "Compare the characteristics of new %ss against the general population, selecting which attributes to compare them with.",
            type_movement
          ),
          title = sprintf("Profile of new %ss", type_movement),
          placement = "left"
        )
      ),
      gt::gt_output(shiny::NS(id, sprintf("%s_profile", type_movement)))
    )
  )
}

#' Workforce Movement Server
#' 
#' @param id A character string specifying the module ID.
#' @param .data A data frame containing personnel data.
#' @param movement_type A character string specifying the type of movement: "hire", "fire", or "turnover".
#' 
#' @import shiny
#' @import bslib
#' @importFrom plotly renderPlotly
#' @importFrom gt render_gt
#' @importFrom gtsummary tbl_summary modify_spanning_header modify_header as_gt
#' @importFrom dplyr filter
#' 
#' @return A Shiny server function for the workforce movement module.
workforce_movement_server <- function(
  id,
  .data,
  movement_type = c("hire", "fire", "turnover")
) {
  shiny::moduleServer(id, function(input, output, session) {
    movement_type <- match.arg(movement_type, choices = c("hire", "fire", "turnover"))

    data_filtered <- shiny::reactive({
      data <- .data

      if (input$group_filter != "ref_date") {
        data <- data |>
          dplyr::filter(
            .data[[input$group_filter]] %in% input$subgroup_filter
          )
      }

      data |>
        dplyr::filter(
          .data[["ref_date"]] >= input$date_range[1],
          .data[["ref_date"]] <= input$date_range[2]
        )
    })

    # plot 1. hiring counts/rates over time
    output[[sprintf("%s_plot", movement_type)]] <- plotly::renderPlotly({
      plot_data <- generate_movement_data(
        .data = data_filtered(),
        movement_type = movement_type,
        measurement_type = input$measurement_type,
        group_cols = input$group_filter
      )

      plot_movement(
        plot_data,
        movement_type = movement_type,
        measurement_type = input$measurement_type,
        group_cols = input$group_filter
      )
    }) |>
      bindEvent(input$apply_btn, ignoreNULL = FALSE)

    if (movement_type %in% c("hire", "fire")) {
      # table 2. demographic characteristics of hires vs. general pop.
      output[[sprintf("%s_profile", movement_type)]] <- gt::render_gt({
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
            )
          ) |>
          gtsummary::modify_spanning_header(
            update = list(
              label ~ "**Population**"
            )
          ) |>
          # label hires and fires as "New Hires" and "New Fires" and
          # stayed as "General Population"
          gtsummary::modify_header(
            update = list(
              label = "**Variable**",
              stat_1 = sprintf("**New %ss**", movement_type),
              stat_2 = "**General Population**"
            )
          ) |>
          gtsummary::as_gt()
      }) |>
        bindEvent(input$apply_btn, ignoreNULL = FALSE)
    }
  })
}
