#' Wage Bill UI Module
#'
#' UI for wage bill analytics, including overview, controls, and plots.
#'
#' @param id Module id.
#' @param wagebill_data Data frame with wage bill data.
#'
#' @importFrom bslib layout_columns card card_header card_body accordion accordion_panel layout_sidebar sidebar tooltip navset_tab nav_panel
#' @importFrom bsicons bs_icon
#' @importFrom shiny markdown icon NS selectInput downloadButton actionButton uiOutput
#' @importFrom shinyWidgets numericRangeInput materialSwitch pickerInput
#' @importFrom plotly plotlyOutput
#' @importFrom stringr str_wrap
#' @importFrom lubridate year
#' @importFrom purrr keep map set_names
#' @importFrom stats na.omit
#' @export
wagebill_ui <- function(id, wagebill_data) {
  macroindicator_choices <- c(
    "GDP" = "gdp_lcu",
    "Public expenditure" = "pexpenditure_lcu",
    "Public revenue" = "prevenue_lcu"
  )

  # value boxes for total wage bill and pension liabilities
  value_boxes <- list(
    uiOutput(
      NS(id, "total_wagebill")
    ),
    uiOutput(
      NS(id, "total_pension_liabilities")
    )
  )

  accordion_controls <- bslib::accordion(
    accordion_panel(
      "Filters",
      icon = bsicons::bs_icon("sliders"),
      !!!default_ui_controls(wagebill_data, id)
    ),
    accordion_panel(
      "Measures",
      icon = bsicons::bs_icon("bar-chart"),
      !!!wagebill_overview_ui(id, wagebill_data)
    )
  )

  bslib::layout_columns(
    fillable = FALSE,
    bslib::card(
      bslib::card_header("Wage Bill Analytics"),
      bslib::card_body(
        shiny::markdown(
          readLines(system.file("markdown/analytics_wagebill.md", package = "govhrapp"))
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
        "Wagebill: Key Metrics",
        bslib::popover(
          bsicons::bs_icon("info-circle-fill"),
          "Computed as the most recent wagebill (active workers) and pension liability (pensioners).",
          title = "Wagebill Overview",
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


    # panels
    bslib::navset_underline(
      # sub-panel 1: overview
      bslib::nav_panel(
        title = "Overview",
        wagebill_overview_ui(NS(id, "overview"), wagebill_data)
      ),
      # sub-panel 2: equity
      bslib::nav_panel(
        title = "Equity",
        wagebill_equity_ui(NS(id, "equity"), wagebill_data)
      ),
      # sub-panel 3: movement
      bslib::nav_panel(
        title = "Movement",
        wagebill_movement_ui(NS(id, "movement"), wagebill_data)
      ),
      # sub-panel 4: retirement
      bslib::nav_panel(
        title = "Retirement",
        wagebill_retirement_ui(NS(id, "retirement"), wagebill_data)
      )
    ),
    col_widths = c(12, 12, 12)
  )
}

#' Wage Bill Server Module
#'
#' Server logic for wage bill analytics server
#'
#' @param id Module id.
#' @param wagebill_data Data frame with wage bill data.
#' @param cache A list containing pre-computed trend summaries for workforce and wagebill data.
#'
#' @importFrom shiny moduleServer reactive validate need bindEvent downloadHandler withProgress incProgress renderUI uiOutput
#' @importFrom shinyWidgets pickerInput
#' @importFrom plotly renderPlotly ggplotly plot_ly layout animation_opts animation_slider
#' @importFrom dplyr filter mutate arrange ungroup across all_of first last pull left_join summarise n_distinct
#' @importFrom lubridate year years
#' @importFrom govhr compute_fastsummary complete_dates convert_constant_ppp
#' @importFrom ggplot2 ggplot aes geom_point geom_line geom_col geom_hline geom_vline scale_y_continuous scale_x_continuous scale_y_discrete scale_color_manual guide_axis labs xlab ylab
#' @importFrom grDevices colorRampPalette
#' @importFrom stats reorder
#' @importFrom scales label_number cut_short_scale comma
#' @importFrom stringr str_wrap
#' @importFrom rmarkdown render
#' @importFrom stats na.omit
#' @export
wagebill_server <- function(id, wagebill_data, cache) {
  shiny::moduleServer(id, function(input, output, session) {
    # 1. value boxes for wage bill key metrics
    output$total_wagebill <- render_wagebill_box(wagebill_data, type_measure = "total_wagebill")
    output$total_pension_liabilities <- render_wagebill_box(wagebill_data, type_measure = "total_pension_liabilities")

    # 2. panels for wage bill server
    wagebill_overview_server("overview", wagebill_data, cache = cache)
    wagebill_equity_server("equity", wagebill_data)
    wagebill_movement_server("movement", wagebill_data)
    wagebill_retirement_server("retirement", wagebill_data)
  })
}

#' Run the Wage Bill Shiny Application
#'
#' Launches an interactive Shiny application for analyzing wage bill data,
#' including time trends, cross-sectional comparisons, growth rate analysis,
#' and animated visualizations.
#'
#' @param wagebill_data A data frame containing wage bill information with the
#'   following required columns:
#'   \itemize{
#'     \item \code{ref_date}: Reference date (Date class)
#'     \item \code{personnel_id}: Personnel identifier (for animation)
#'     \item \code{base_salary_lcu}: Base salary in local currency units
#'     \item \code{gross_salary_lcu}: Gross salary in local currency units
#'     \item \code{net_salary_lcu}: Net salary in local currency units
#'     \item \code{est_id}: Establishment identifier
#'     \item \code{contract_type_native}: Contract type
#'     \item \code{paygrade}: Paygrade classification
#'     \item \code{occupation_native}: Occupation classification
#'     \item \code{gender}: Gender
#'     \item \code{educat7}: Education level
#'     \item \code{status}: Employment status
#'   }
#' @param ... Additional arguments passed to \code{\link[shiny]{shinyApp}}.
#'
#' @return A Shiny app object.
#'
#' @details
#' The application is organized into two main tabs:
#'
#' \strong{Overview Tab:}
#' \itemize{
#'   \item Time trend analysis with optional baseline indexing
#'   \item Cross-sectional wage bill totals by group
#'   \item Year-over-year growth rates by group
#'   \item Wage bill dispersion and variation analysis
#'   \item Download Word report functionality
#' }
#'
#' \strong{Animation Tab:}
#' \itemize{
#'   \item Animated scatter plot showing the evolution of wage bill vs. headcount over time
#'   \item Log-scale axes for better visualization of different magnitudes
#'   \item Frame-by-frame animation through time periods
#' }
#'
#' All visualizations support interactive filtering by time period, wage type
#' (base/gross/net salary), and grouping variable (establishment, contract type,
#' personnel characteristics).
#'
#' @examples
#' \dontrun{
#' # Run with default data
#' run_wagebillapp(wagebill_data = govhr::wagebill)
#'
#' # Run with filtered data
#' my_data <- govhr::wagebill |>
#'   dplyr::filter(lubridate::year(ref_date) >= 2015)
#' run_wagebillapp(wagebill_data = my_data)
#' }
#'
#' @importFrom shiny shinyApp
#' @importFrom bslib page_sidebar sidebar
#' @importFrom plotly plotlyOutput renderPlotly ggplotly
#' @export
run_wagebillapp <- function(
  wagebill_data,
  ...
) {
  ui <- wagebill_ui("test", wagebill_data)

  server <- function(input, output, session) {
    wagebill_server("test", wagebill_data)
  }

  shiny::shinyApp(ui, server, ...)
}
