#' Workforce tab UI
#'
#' Guidance, key indicator boxes, and the overview, movement, transition and
#' retirement panels of the workforce tab.
#'
#' @param id Character. Module namespace ID.
#' @inheritParams run_govhrapp_analytics
#' @param wagebill_data Contract data, as in [run_govhrapp_analytics()]. The
#'   workforce tab uses it only in the transition panel, which counts moves
#'   between contract-level groups such as `contract_type`.
#'
#' @returns A Shiny UI definition.
#'
#' @importFrom bslib accordion accordion_panel card card_body card_header layout_column_wrap layout_columns nav_panel navset_underline popover
#' @importFrom bsicons bs_icon
#' @importFrom shiny NS icon markdown uiOutput
#' @export
workforce_ui <- function(id, workforce_data, wagebill_data) {
  # value boxes for workforce movement metrics
  value_boxes <- list(
    shiny::uiOutput(shiny::NS(id, "movement_hire")),
    shiny::uiOutput(shiny::NS(id, "movement_separation")),
    shiny::uiOutput(shiny::NS(id, "movement_retirement")),
    shiny::uiOutput(shiny::NS(id, "movement_replacement"))
  )

  bslib::layout_columns(
    fillable = FALSE,
    col_widths = 12,

    bslib::card(
      bslib::card_header("Workforce analytics"),
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
        title = "Guiding questions",
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
        "Key indicators",
        bslib::popover(
          bsicons::bs_icon("info-circle-fill"),
          "Hires, separations and retirements on the latest reference date, as counts and as shares of the previous reference date's active headcount. Separations exclude retirements. The replacement rate is the ratio of hires to separation.",
          title = "Key indicators",
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

#' Workforce tab server
#'
#' Renders the key indicator boxes and runs the overview, movement, transition
#' and retirement panel servers.
#'
#' @param id Character. Module namespace ID.
#' @inheritParams workforce_ui
#' @param cache List of precomputed summaries from [build_analytics_cache()].
#'
#' @returns Called for its side effect of rendering the module's outputs.
#'
#' @importFrom shiny moduleServer
#' @export
workforce_server <- function(id, workforce_data, wagebill_data, cache) {
  shiny::moduleServer(id, function(input, output, session) {
    # 1. value boxes for workforce movement metrics
    output$movement_hire <- render_movement_box(
      workforce_data, "hire", cache = cache
    )
    output$movement_separation <- render_movement_box(
      workforce_data, "separation", cache = cache
    )
    output$movement_retirement <- render_movement_box(
      workforce_data, "retirement", cache = cache
    )
    output$movement_replacement <- render_movement_box(
      workforce_data, "replacement", cache = cache
    )

    # 2. panel servers
    workforce_overview_server("overview", workforce_data, cache = cache)
    workforce_movement_server("movement", workforce_data, cache = cache)
    workforce_transition_server("transition", wagebill_data, cache = cache)
    workforce_retirement_server("retirement", workforce_data, cache = cache)
  })
}

#' Run the workforce app
#'
#' Launches the workforce tab of the analytics app on its own.
#'
#' @inheritParams workforce_ui
#' @param cache List of precomputed summaries from [build_analytics_cache()].
#'   Default builds one from `workforce_data` and `wagebill_data`.
#' @param ... Additional arguments passed to [shiny::shinyApp()].
#'
#' @returns A Shiny app object.
#'
#' @seealso [run_govhrapp_analytics()] for the full analytics app.
#'
#' @examples
#' \dontrun{
#' run_workforce_app(workforce_data, wagebill_data)
#' }
#'
#' @importFrom shiny shinyApp
#' @export
run_workforce_app <- function(
  workforce_data,
  wagebill_data,
  cache = build_analytics_cache(workforce_data, wagebill_data),
  ...
) {
  check_analytics_data(workforce_data, wagebill_data)

  ui <- workforce_ui("workforce", workforce_data, wagebill_data)

  server <- function(input, output, session) {
    workforce_server("workforce", workforce_data, wagebill_data, cache = cache)
  }

  shiny::shinyApp(ui, server, ...)
}
