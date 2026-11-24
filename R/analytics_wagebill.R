#' Wage bill tab UI
#'
#' Guidance, key indicator boxes, and the overview, equity, movement and
#' retirement panels of the wage bill tab.
#'
#' @param id Character. Module namespace ID.
#' @param wagebill_data Contract data, one row per contract and reference date:
#'   a data frame. Must contain `ref_date`, `personnel_id`, `employment_status`
#'   (`"active"` or `"pensioner"`), `birth_date` and `gross_salary_lcu`. Other pay columns listed in
#'   `govhr::dictionary` become pay measure options, and its categorical
#'   columns become grouping options.
#'
#' @returns A Shiny UI definition.
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

#' Wage bill tab server
#'
#' Renders the key indicator boxes and runs the overview, equity, movement and
#' retirement panel servers.
#'
#' @param id Character. Module namespace ID.
#' @inheritParams wagebill_ui
#' @param cache List of precomputed summaries from [build_analytics_cache()].
#'
#' @returns Called for its side effect of rendering the module's outputs.
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

#' Run the wage bill app
#'
#' Launches the wage bill tab of the analytics app on its own. Its panels cover
#' wage bill trends, pay equity, the cost of hires and separations, and
#' retirement costs.
#'
#' @inheritParams wagebill_ui
#' @param cache List of precomputed summaries, with the wage bill summaries in
#'   its `wagebill` element as in [build_analytics_cache()]. Default builds
#'   them from `wagebill_data`.
#' @param ... Additional arguments passed to [shiny::shinyApp()].
#'
#' @returns A Shiny app object.
#'
#' @seealso [run_govhrapp_analytics()] for the full analytics app.
#'
#' @examples
#' \dontrun{
#' run_wagebill_app(wagebill_data)
#' }
#'
#' @importFrom shiny shinyApp
#' @export
run_wagebill_app <- function(
  wagebill_data,
  cache = list(wagebill = build_wagebill_cache(wagebill_data)),
  ...
) {
  # the standalone app has no transition panel, so `contract_type` is optional
  check_analytics_data(wagebill_data = wagebill_data, transition = FALSE)

  ui <- wagebill_ui("wagebill", wagebill_data)

  server <- function(input, output, session) {
    wagebill_server("wagebill", wagebill_data, cache = cache)
  }

  shiny::shinyApp(ui, server, ...)
}

#' Run the wage bill app (deprecated)
#'
#' `run_wagebillapp()` was renamed to [run_wagebill_app()], to match the names
#' of the other app launchers.
#'
#' @inheritParams run_wagebill_app
#'
#' @returns A Shiny app object.
#'
#' @keywords internal
#' @export
run_wagebillapp <- function(
  wagebill_data,
  cache = list(wagebill = build_wagebill_cache(wagebill_data)),
  ...
) {
  .Deprecated("run_wagebill_app", package = "govhrapp")

  run_wagebill_app(wagebill_data, cache = cache, ...)
}
