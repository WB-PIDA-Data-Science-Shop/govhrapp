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
#' @import dplyr
#' @export
wagebill_ui_beta <- function(id, wagebill_data) {
  macroindicator_choices <- c(
    "GDP" = "gdp_lcu",
    "Public expenditure" = "pexpenditure_lcu",
    "Public revenue" = "prevenue_lcu"
  )

  accordion_controls <- bslib::accordion(
    accordion_panel(
      "Filters",
      icon = bsicons::bs_icon("sliders"),
      !!!ui_filter_controls(wagebill_data, id)
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
          readLines(system.file("markdown/wagebill.md", package = "govhrapp"))
        )
      )
    ),
    bslib::accordion(
      bslib::accordion_panel(
        title = "Guidance Questions",
        icon = shiny::icon("question-circle"),
        shiny::markdown(
          readLines(system.file(
            "markdown/wagebill_questions.md",
            package = "govhrapp"
          ))
        )
      ),
      open = FALSE
    ),
    bslib::navset_underline(
      bslib::nav_panel(
        title = "Overview",
        wagebill_overview_ui(NS(id, "overview"), wagebill_data),
      # ),
      # bslib::nav_panel(
      #   title = "Fiscal Sustainability",
      #   bslib::layout_sidebar(
      #     fillable = FALSE,
      #     sidebar = bslib::sidebar(
      #       title = "Controls",
      #       width = "300px",
      #       # Reuse shared filter controls — same input IDs, so state is shared
      #       shared_filter_controls,
      #       shiny::selectInput(
      #         shiny::NS(id, "wagebill_measure"),
      #         "Type of Wage:",
      #         choices = identify_wagebill_choices(wagebill_data)
      #       ),
      #       shiny::selectInput(
      #         shiny::NS(id, "macroindicator_measure"),
      #         "Macro indicator:",
      #         choices = macroindicator_choices
      #       ),
      #       shiny::actionButton(
      #         shiny::NS(id, "apply_btn"),
      #         "Apply selection",
      #         icon = shiny::icon("play"),
      #         class = "btn-primary w-100 mt-2"
      #       )
      #     ),
      #     bslib::card(
      #       full_screen = TRUE,
      #       bslib::card_header(
      #         "Fiscal Sustainability",
      #         bslib::tooltip(
      #           bsicons::bs_icon("info-circle"),
      #           "Evolution of wage bill, normalized by macroeconomic indicators."
      #         )
      #       ),
      #       plotly::plotlyOutput(shiny::NS(id, "wagebill_fiscal")),
      #       min_height = "450px"
      #     )
        # )
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
wagebill_server_beta <- function(id, wagebill_data) {
  shiny::moduleServer(id, function(input, output, session) {
    wagebill_overview_server("overview", wagebill_data)
    # # choice of cols
    # wagebill_group_choices <- identify_group_choices(wagebill_data)

    # update_group_filter_controls(wagebill_data, input, session)

    # wagebill_filtered <- shiny::reactive({
    #   data <- wagebill_data

    #   if (
    #     !is.null(input$wagebill_filter_variable) &&
    #       input$wagebill_filter_variable != "none" &&
    #       length(input$wagebill_filter_values) > 0
    #   ) {
    #     data <- data |>
    #       dplyr::filter(
    #         .data[[input$wagebill_filter_variable]] %in%
    #           input$wagebill_filter_values
    #       )
    #   }

    #   data |>
    #     dplyr::mutate(year = lubridate::year(.data[["ref_date"]])) |>
    #     dplyr::filter(
    #       .data[["year"]] >= input$date_range[1],
    #       .data[["year"]] <= input$date_range[2]
    #     )
    # })

    # wagebill_summary <- shiny::reactive({
    #   out <- compute_trend_summary(
    #     wagebill_filtered(),
    #     group = input$wagebill_group,
    #     measure_col = input$wagebill_measure
    #   )

    #   if (input$toggle_growth) {
    #     out <- apply_baseline_index(out, group = input$wagebill_group)
    #   }

    #   out
    # })

    # wagebill_annual <- shiny::reactive({
    #   wagebill_filtered() |>
    #     govhr::compute_fastsummary(
    #       cols = input$wagebill_measure,
    #       fns = "sum",
    #       groups = c("ref_date", "country_code")
    #     )
    # })

    # # plot 1. panel
    # output$wagebill_panel <- plotly::renderPlotly({
    #   plotly::ggplotly(
    #     plot_trend(
    #       wagebill_summary(),
    #       group = input$wagebill_group,
    #       toggle_growth = input$toggle_growth,
    #       y_label = "Wage Bill"
    #     )
    #   )
    # }) |>
    #   shiny::bindEvent(input$apply_btn, ignoreNULL = FALSE)

    # # plot 2. macro panel
    # output$wagebill_fiscal <- renderPlotly({
    #   wagebill_fiscal <- wagebill_annual() |>
    #     mutate(
    #       year = lubridate::year(ref_date)
    #     ) |>
    #     left_join(
    #       govhr::macro_indicators,
    #       by = c("country_code", "year")
    #     ) |>
    #     mutate(
    #       ratio = .data[["value"]] / .data[[input$macroindicator_measure]] * 100
    #     )

    #   plot <- wagebill_fiscal |>
    #     ggplot2::ggplot(
    #       ggplot2::aes(x = .data[["ref_date"]], y = .data[["ratio"]])
    #     ) +
    #     ggplot2::geom_point() +
    #     ggplot2::geom_line() +
    #     ggplot2::xlab("Time") +
    #     ggplot2::ylab("Ratio") +
    #     ggplot2::scale_y_continuous(
    #       labels = scales::percent_format(scale = 1)
    #     )

    #   plotly::ggplotly(plot)
    # }) |>
    #   shiny::bindEvent(input$apply_btn, ignoreNULL = FALSE)

    # # plot 2. total by group
    # output$wagebill_cross_section <- plotly::renderPlotly({
    #   shiny::validate(
    #     shiny::need(
    #       input$wagebill_group != "ref_date",
    #       "Please select a group."
    #     )
    #   )

    #   cross_section_data <- compute_cross_section_summary(
    #     wagebill_filtered(),
    #     group = input$wagebill_group,
    #     measure_col = input$wagebill_measure
    #   )

    #   n_groups <- nrow(cross_section_data)
    #   plot_height <- max(350, n_groups * 35 + 100)

    #   plotly::ggplotly(
    #     plot_bar_total(
    #       cross_section_data,
    #       group = input$wagebill_group,
    #       x_label = "Wage bill"
    #     ),
    #     height = plot_height
    #   )
    # }) |>
    #   shiny::bindEvent(input$apply_btn, ignoreNULL = FALSE)

    # # plot 3. growth rate by group
    # output$wagebill_change <- plotly::renderPlotly({
    #   shiny::validate(
    #     shiny::need(
    #       input$wagebill_group != "ref_date",
    #       "Please select a group."
    #     )
    #   )

    #   change_data <- compute_growth_summary(
    #     wagebill_filtered(),
    #     group = input$wagebill_group,
    #     measure_col = input$wagebill_measure
    #   )

    #   n_groups <- nrow(change_data)
    #   plot_height <- max(350, n_groups * 35 + 100)

    #   plotly::ggplotly(
    #     plot_bar_growth(change_data, group = input$wagebill_group),
    #     height = plot_height
    #   )
    # }) |>
    #   shiny::bindEvent(input$apply_btn, ignoreNULL = FALSE)

    # # plot 4. variation
    # output$wagebill_variation <- plotly::renderPlotly({
    #   shiny::validate(
    #     shiny::need(
    #       input$wagebill_group != "ref_date",
    #       "Please select a group."
    #     )
    #   )

    #   dispersion_data <- wagebill_filtered() |>
    #     # only present latest reference date
    #     dplyr::filter(
    #       ref_date == max(ref_date),
    #       .by = all_of(input$wagebill_group)
    #     )

    #   # dynamic height
    #   n_groups <- dispersion_data |>
    #     dplyr::filter(!is.na(.data[[input$wagebill_group]])) |>
    #     dplyr::pull(input$wagebill_group) |>
    #     unique() |>
    #     length()

    #   plot_height <- max(350, n_groups * 25 + 100)

    #   plot <- dispersion_data |>
    #     plot_segment(
    #       col = input$wagebill_measure,
    #       group = input$wagebill_group
    #     ) +
    #     scale_y_discrete(
    #       guide = guide_axis(n.dodge = 2)
    #     ) +
    #     ggplot2::labs(
    #       x = "Wage bill",
    #       y = ""
    #     )

    #   plotly::ggplotly(plot, height = plot_height)
    # }) |>
    #   shiny::bindEvent(input$apply_btn, ignoreNULL = FALSE)

    # # report
    # output$download_report <- shiny::downloadHandler(
    #   filename = function() {
    #     paste0("wagebill_report_", format(Sys.Date(), "%Y%m%d"), ".docx")
    #   },
    #   content = function(file) {
    #     # Show progress
    #     shiny::withProgress(message = 'Generating report...', value = 0, {
    #       # Increment progress
    #       shiny::incProgress(0.3, detail = "Creating plots...")

    #       # Generate report using helper function
    #       output_path <- generate_wagebill_report(
    #         wagebill_summary_data = wagebill_summary(),
    #         wagebill_filtered_data = wagebill_filtered(),
    #         date_range = input$date_range,
    #         wagebill_measure = input$wagebill_measure,
    #         wagebill_group = input$wagebill_group,
    #         toggle_growth = input$toggle_growth
    #       )

    #       shiny::incProgress(0.9, detail = "Finalizing...")

    #       # Copy generated file to download location
    #       file.copy(output_path, file, overwrite = TRUE)

    #       shiny::incProgress(1, detail = "Complete!")
    #     })
    #   }
    # )
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
run_wagebillapp_beta <- function(
  wagebill_data,
  ...
) {
  ui <- wagebill_ui_beta("test", wagebill_data)

  server <- function(input, output, session) {
    wagebill_server_beta("test", wagebill_data)
  }

  shiny::shinyApp(ui, server, ...)
}
