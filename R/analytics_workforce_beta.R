#' Workforce Analytics Module (Beta)
#'
#' @param id A character string specifying the module ID.
#' @param .data A data frame containing personnel data.
#'
#' @import shiny
#' @import bslib
#' @importFrom plotly renderPlotly
#'
#' @return A Shiny app object for workforce analytics.
workforce_ui_beta <- function(id, .data) {
  # value boxes for workforce movement metrics
  value_boxes <- list(
    uiOutput(
      NS(id, "movement_hire")
    ),
    uiOutput(
      NS(id, "movement_fire")
    ),
    uiOutput(
      NS(id, "movement_retirement")
    ),
    uiOutput(
      NS(id, "movement_turnover")
    )
  )

  bslib::layout_columns(
    fillable = FALSE,
    col_widths = 12,

    # 1. value boxes for coverage metrics
    bslib::card(
      bslib::card_header(
        "Workforce: Overview",
        bslib::popover(
          bsicons::bs_icon("info-circle-fill"),
          "Computed as the most recent count or share of hires and fires. For turnover, it is the ratio of hires to fires in the most recent reference period.",
          title = "Workforce Overview",
          placement = "left"
        ),
        class = "d-flex justify-content-between"
      ),
      bslib::card_body(
        layout_column_wrap(
          width = 1/2,
          fill = FALSE,
          !!!value_boxes
        )
      )
    ),

    # 2. panel for workforce movement
    bslib::navset_underline(
      # sub-panel 1: overview
      bslib::nav_panel(
        title = "Overview",
        workforce_overview_ui(NS(id, "overview"), .data)
      ),
      # sub-panel 2: hiring
      bslib::nav_panel(
        title = "Hire",
        workforce_movement_ui(NS(id, "hire"), .data, type_movement = "hire")
      ),
      # sub-panel 3: firing
      bslib::nav_panel(
        title = "Fire",
        workforce_movement_ui(NS(id, "fire"), .data, type_movement = "fire")
      ),
      # sub-panel 4: retirement
      bslib::nav_panel(
        title = "Retirement",
        workforce_retirement_ui(NS(id, "retirement"), .data)
      ),
      # sub-panel 5: turnover
      bslib::nav_panel(
        title = "Turnover",
        workforce_movement_ui(
          NS(id, "turnover"),
          .data,
          type_movement = "turnover"
        )
      )
    )
  )
}

workforce_server_beta <- function(id, .data) {
  moduleServer(id, function(input, output, session) {
    update_group_filter_controls(.data, input, session)

    # 1. value boxes for workforce movement metrics
    output$movement_hire <- render_movement_box(.data, type_movement = "hire")
    output$movement_fire <- render_movement_box(.data, type_movement = "fire")
    output$movement_retirement <- render_movement_box(
      .data,
      type_movement = "retirement"
    )
    output$movement_turnover <- render_movement_box(
      .data,
      type_movement = "turnover"
    )

    # 2. panel servers
    workforce_overview_server("overview", .data)
    workforce_movement_server("hire", .data, movement_type = "hire")
    workforce_movement_server("fire", .data, movement_type = "fire")
    workforce_retirement_server("retirement", .data)
    workforce_movement_server("turnover", .data, movement_type = "turnover")
  })
}

run_workforce_app_beta <- function(
  workforce_data,
  ...
) {
  theme <- bslib::bs_theme(
    bootswatch = "litera"
  )

  ui <- workforce_ui_beta("test", workforce_data)

  server <- function(input, output, session) {
    workforce_server_beta("test", workforce_data)
  }

  shiny::shinyApp(ui, server, ...)
}
