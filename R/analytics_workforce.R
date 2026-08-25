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
workforce_ui <- function(id, .data) {
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

    bslib::card(
      bslib::card_header("Workforce: Overview"),
      bslib::card_body(
        shiny::markdown(
          readLines(system.file("markdown/analytics_workforce.md", package = "govhrapp"))
        )
      )
    ),
    bslib::accordion(
      bslib::accordion_panel(
        title = "Guidance Questions",
        icon = shiny::icon("question-circle"),
        shiny::markdown(
          readLines(system.file(
            "markdown/analytics_workforce_questions.md",
            package = "govhrapp"
          ))
        )
      ),
      open = FALSE
    ),

    # 1. value boxes for coverage metrics
    bslib::card(
      bslib::card_header(
        "Workforce: Key Metrics",
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
      # sub-panel 2: movement
      bslib::nav_panel(
        title = "Movement",
        workforce_movement_ui(NS(id, "movement"), .data)
      ),
      # sub-panel 3: retirement
      bslib::nav_panel(
        title = "Retirement",
        workforce_retirement_ui(NS(id, "retirement"), .data)
      )
    )
  )
}

workforce_server <- function(id, .data) {
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
    workforce_movement_server("movement", .data)
    workforce_retirement_server("retirement", .data)
  })
}

run_workforce_app <- function(
  workforce_data,
  ...
) {
  theme <- bslib::bs_theme(
    bootswatch = "litera"
  )

  ui <- workforce_ui("test", workforce_data)

  server <- function(input, output, session) {
    workforce_server("test", workforce_data)
  }

  shiny::shinyApp(ui, server, ...)
}
