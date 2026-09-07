#' Workforce UI
#'
#' Top-level workforce tab: guidance, key indicator boxes, and the overview,
#' movement, transition and retirement panels.
#'
#' @param id Character. Module namespace ID.
#' @param workforce_data Data frame containing personnel data.
#' @param wagebill_data Data frame containing wage bill data, used by the
#'   overview and transition panels.
#'
#' @return A Shiny UI definition.
#'
#' @importFrom bslib accordion accordion_panel card card_body card_header layout_column_wrap layout_columns nav_panel navset_underline popover
#' @importFrom bsicons bs_icon
#' @importFrom shiny NS icon markdown uiOutput
workforce_ui <- function(id, workforce_data, wagebill_data) {
  # value boxes for workforce movement metrics
  value_boxes <- list(
    shiny::uiOutput(shiny::NS(id, "movement_hire")),
    shiny::uiOutput(shiny::NS(id, "movement_fire")),
    shiny::uiOutput(shiny::NS(id, "movement_retirement")),
    shiny::uiOutput(shiny::NS(id, "movement_turnover"))
  )

  bslib::layout_columns(
    fillable = FALSE,
    col_widths = 12,

    bslib::card(
      bslib::card_header("Workforce: Overview"),
      bslib::card_body(
        shiny::markdown(
          readLines(system.file(
            "markdown/analytics_workforce.md",
            package = "govhrapp"
          ))
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

    # 1. value boxes for coverage metrics in the same row
    bslib::card(
      bslib::card_header(
        "Workforce: Key Indicators",
        bslib::popover(
          bsicons::bs_icon("info-circle-fill"),
          "Computed as the most recent count or share of hires and fires. For turnover, it is the ratio of hires to fires in the most recent reference period.",
          title = "Workforce Overview",
          placement = "left"
        ),
        class = "d-flex justify-content-between"
      ),
      bslib::card_body(
        bslib::layout_column_wrap(
          fill = FALSE,
          !!!value_boxes
        )
      )
    ),

    # 2. panels
    bslib::navset_underline(
      bslib::nav_panel(
        title = "Overview",
        workforce_overview_ui(shiny::NS(id, "overview"), workforce_data)
      ),
      bslib::nav_panel(
        title = "Movement",
        workforce_movement_ui(shiny::NS(id, "movement"), workforce_data)
      ),
      bslib::nav_panel(
        title = "Transitions",
        workforce_transition_ui(shiny::NS(id, "transition"), wagebill_data)
      ),
      bslib::nav_panel(
        title = "Retirement",
        workforce_retirement_ui(shiny::NS(id, "retirement"), workforce_data)
      )
    )
  )
}

#' Workforce Server
#'
#' Renders the key indicator boxes and delegates to the overview, movement,
#' transition and retirement panel servers.
#'
#' @param id Character. Module namespace ID.
#' @param workforce_data Data frame containing personnel data.
#' @param wagebill_data Data frame containing wage bill data, used by the
#'   overview and transition panels.
#' @param cache List of pre-computed summaries from [build_analytics_cache()].
#'
#' @return A Shiny module server function.
#'
#' @importFrom shiny moduleServer
#' @export
workforce_server <- function(id, workforce_data, wagebill_data, cache) {
  shiny::moduleServer(id, function(input, output, session) {
    # 1. value boxes for workforce movement metrics
    output$movement_hire <- render_movement_box(
      workforce_data, "hire", cache = cache
    )
    output$movement_fire <- render_movement_box(
      workforce_data, "fire", cache = cache
    )
    output$movement_retirement <- render_movement_box(
      workforce_data, "retirement", cache = cache
    )
    output$movement_turnover <- render_movement_box(
      workforce_data, "turnover", cache = cache
    )

    # 2. panel servers
    # NOTE: the overview panel's UI is built from wagebill_data while its
    # server filters workforce_data. Wiring preserved as-is; see the audit note
    # on the headcount source mismatch.
    workforce_overview_server("overview", workforce_data, cache = cache)
    workforce_movement_server("movement", workforce_data, cache = cache)
    workforce_transition_server("transition", wagebill_data, cache = cache)
    workforce_retirement_server("retirement", workforce_data, cache = cache)
  })
}

#' Run the Workforce Shiny Application
#'
#' Launches the workforce analytics app on its own, outside the full govhr
#' dashboard.
#'
#' @param workforce_data Data frame containing personnel data.
#' @param wagebill_data Data frame containing wage bill data.
#' @param cache List of pre-computed summaries from [build_analytics_cache()].
#'   Defaults to building one from the supplied data.
#' @param ... Additional arguments passed to [shiny::shinyApp()].
#'
#' @return A Shiny app object.
#'
#' @importFrom shiny shinyApp
#' @keywords internal
run_workforce_app <- function(
  workforce_data,
  wagebill_data,
  cache = build_analytics_cache(workforce_data, wagebill_data),
  ...
) {
  ui <- workforce_ui("workforce", workforce_data, wagebill_data)

  server <- function(input, output, session) {
    workforce_server("workforce", workforce_data, wagebill_data, cache = cache)
  }

  shiny::shinyApp(ui, server, ...)
}
