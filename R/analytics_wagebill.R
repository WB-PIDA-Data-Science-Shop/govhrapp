#' Wage Bill UI
#'
#' Top-level wage bill tab: guidance, key indicator boxes, and the overview,
#' equity, movement and retirement panels.
#'
#' @param id Character. Module namespace ID.
#' @param wagebill_data Data frame containing wage bill data.
#'
#' @return A Shiny UI definition.
#'
#' @importFrom bslib accordion accordion_panel card card_body card_header layout_column_wrap layout_columns nav_panel navset_underline popover
#' @importFrom bsicons bs_icon
#' @importFrom shiny NS icon markdown uiOutput
#' @export
wagebill_ui <- function(id, wagebill_data) {
  # value boxes for total wage bill and pension liabilities
  value_boxes <- list(
    shiny::uiOutput(shiny::NS(id, "total_wagebill")),
    shiny::uiOutput(shiny::NS(id, "total_pension_liabilities"))
  )

  bslib::layout_columns(
    fillable = FALSE,
    col_widths = 12,

    bslib::card(
      bslib::card_header("Wage Bill Analytics"),
      bslib::card_body(
        shiny::markdown(
          readLines(system.file(
            "markdown/analytics_wagebill.md",
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
            "markdown/analytics_wagebill_questions.md",
            package = "govhrapp"
          ))
        )
      ),
      open = FALSE
    ),

    # value boxes
    bslib::card(
      bslib::card_header(
        "Wagebill: Key Indicators",
        bslib::popover(
          bsicons::bs_icon("info-circle-fill"),
          "Computed as the most recent wagebill (active workers) and pension liability (pensioners).",
          title = "Wagebill Overview",
          placement = "left"
        ),
        class = "d-flex justify-content-between"
      ),
      bslib::card_body(
        bslib::layout_column_wrap(
          width = 1 / 2,
          fill = FALSE,
          !!!value_boxes
        )
      )
    ),

    # panels
    bslib::navset_underline(
      bslib::nav_panel(
        title = "Overview",
        wagebill_overview_ui(shiny::NS(id, "overview"), wagebill_data)
      ),
      bslib::nav_panel(
        title = "Equity",
        wagebill_equity_ui(shiny::NS(id, "equity"), wagebill_data)
      ),
      bslib::nav_panel(
        title = "Movement",
        wagebill_movement_ui(shiny::NS(id, "movement"), wagebill_data)
      ),
      bslib::nav_panel(
        title = "Retirement",
        wagebill_retirement_ui(shiny::NS(id, "retirement"), wagebill_data)
      )
    )
  )
}

#' Wage Bill Server
#'
#' Renders the key indicator boxes and delegates to the overview, equity,
#' movement and retirement panel servers.
#'
#' @param id Character. Module namespace ID.
#' @param wagebill_data Data frame containing wage bill data.
#' @param cache List of pre-computed summaries from [build_analytics_cache()].
#'
#' @return A Shiny module server function.
#'
#' @importFrom shiny moduleServer
#' @export
wagebill_server <- function(id, wagebill_data, cache) {
  shiny::moduleServer(id, function(input, output, session) {
    # 1. value boxes for wage bill key metrics
    output$total_wagebill <- render_wagebill_box(
      wagebill_data,
      measure_type = "total_wagebill",
      cache = cache
    )
    output$total_pension_liabilities <- render_wagebill_box(
      wagebill_data,
      measure_type = "total_pension_liabilities",
      cache = cache
    )

    # 2. panels for wage bill server
    wagebill_overview_server("overview", wagebill_data, cache = cache)
    wagebill_equity_server("equity", wagebill_data, cache = cache)
    wagebill_movement_server("movement", wagebill_data, cache = cache)
    wagebill_retirement_server("retirement", wagebill_data, cache = cache)
  })
}

#' Run the Wage Bill Shiny Application
#'
#' Launches the wage bill analytics app on its own, outside the full govhr
#' dashboard. Panels cover overview trends, pay equity, movement costs and
#' retirement costs.
#'
#' @param wagebill_data Data frame containing wage bill data. Requires
#'   `ref_date`, `personnel_id`, `employment_status`, `birth_date` and at least
#'   one salary column (`gross_salary_lcu` is used for the key indicators);
#'   categorical columns present in `govhr::dictionary` become grouping options.
#' @param cache List of pre-computed summaries from [build_wagebill_cache()].
#'   Defaults to building one from `wagebill_data`.
#' @param ... Additional arguments passed to [shiny::shinyApp()].
#'
#' @return A Shiny app object.
#'
#' @examples
#' \dontrun{
#' run_wagebillapp(wagebill_data = govhr::bra_hrmis_contract)
#' }
#'
#' @importFrom shiny shinyApp
#' @export
run_wagebillapp <- function(
  wagebill_data,
  cache = list(wagebill = build_wagebill_cache(wagebill_data)),
  ...
) {
  ui <- wagebill_ui("wagebill", wagebill_data)

  server <- function(input, output, session) {
    wagebill_server("wagebill", wagebill_data, cache = cache)
  }

  shiny::shinyApp(ui, server, ...)
}
