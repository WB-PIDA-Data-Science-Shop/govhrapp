#' Wage Bill UI Module
#'
#' UI for wage bill analytics, including overview, controls, and plots.
#'
#' @param id Module id.
#' @param wagebill_data Data frame with wage bill data.
#'
#' @importFrom bslib layout_columns card card_header card_body accordion accordion_panel layout_sidebar sidebar tooltip
#' @importFrom bsicons bs_icon
#' @importFrom shiny markdown icon NS selectInput downloadButton
#' @importFrom shinyWidgets numericRangeInput materialSwitch
#' @importFrom plotly plotlyOutput
#' @importFrom stringr str_wrap
#' @importFrom lubridate year
#' @importFrom purrr keep map set_names
#' @importFrom stats na.omit
#' @import dplyr
#' @export
wagebill_ui <- function(id, wagebill_data) {
  available_cols <- names(wagebill_data)

  wagebill_measure_choices <- list(
    "Base Salary"  = "base_salary_lcu",
    "Gross Salary" = "gross_salary_lcu",
    "Net Salary"   = "net_salary_lcu",
    "Allowance"    = "allowance_lcu"
  ) |>
    purrr::keep(\(x) x %in% available_cols)

  wagebill_group_choices <- c(
    list("All" = "ref_date"),
    govhr::dictionary |>
      dplyr::filter(
        .data[["variable_id"]] %in% available_cols &
          .data[["variable_class"]] == "character" &
          !.data[["variable_id"]] %in% c("ref_date", "contract_id", "personnel_id")
      ) |>
      dplyr::group_by(.data[["module"]]) |>
      dplyr::summarise(
        choices = list(
          purrr::set_names(.data[["variable_id"]], .data[["variable_name"]])
        ),
        .groups = "drop"
      ) |>
      dplyr::pull(.data[["choices"]],
                  name = .data[["module"]])
  )

  bslib::layout_columns(
    fillable = FALSE,
    bslib::card(
      bslib::card_header(
        "Wage Bill Analytics"
      ),
      bslib::card_body(
        shiny::markdown(
          readLines("inst/markdown/wagebill.md")
        )
      )
    ),
    bslib::accordion(
      bslib::accordion_panel(
        title = "Guidance Questions",
        icon = shiny::icon("question-circle"),
        shiny::markdown(
          readLines("inst/markdown/wagebill_questions.md")
        )
      ),
      open = FALSE
    ),
    bslib::page_navbar(
      bslib::nav_panel(
        title = "Overview",
        bslib::layout_sidebar(
          fillable = FALSE,
          title = "Wagebill: Overview",
          sidebar = bslib::sidebar(
            title = "Controls",
            width = "300px",
            shinyWidgets::numericRangeInput(
              shiny::NS(id, "date_range"),
              "Time frame:",
              value = c(
                min(lubridate::year(wagebill_data$ref_date), na.rm = TRUE),
                max(lubridate::year(wagebill_data$ref_date), na.rm = TRUE)
              ),
              min = min(lubridate::year(wagebill_data$ref_date), na.rm = TRUE),
              max = max(lubridate::year(wagebill_data$ref_date), na.rm = TRUE)
            ),
            shiny::selectInput(
              shiny::NS(id, "wagebill_measure"),
              "Type of Wage:",
              choices = wagebill_measure_choices
            ),
            shiny::selectInput(
              shiny::NS(id, "wagebill_group"),
              "Group:",
              choices = wagebill_group_choices
            ),
            shiny::uiOutput(shiny::NS(id, "group_filter_ui")),
            shiny::downloadButton(
                shiny::NS(id, "download_report"),
                "Generate report",
                icon = shiny::icon("file-word")
            )
          ),
          bslib::card(
            bslib::card_header(
              "Time trends",
              bslib::tooltip(
                bsicons::bs_icon("info-circle"),
                "Wage bill total, by year. Choosing a group will add new totals, by group."
              )
            ),
            shinyWidgets::materialSwitch(
              shiny::NS(id, "toggle_growth"),
              label = "Switch to baseline index",
              value = FALSE
            ),
            plotly::plotlyOutput(shiny::NS(id, "wagebill_panel")),
            min_height = "350px"
          ),
          bslib::layout_columns(
            bslib::card(
              full_screen = TRUE,
              fillable = FALSE,
              bslib::card_header(
                "Total by group",
                bslib::tooltip(
                  bsicons::bs_icon("info-circle"),
                  "Wage bill total, by group. Total refers to the latest available year in the selected time frame."
                )
              ),
              plotly::plotlyOutput(shiny::NS(id, "wagebill_cross_section")),
              min_height = "450px"
            ),
            bslib::card(
              full_screen = TRUE,
              fillable = FALSE,
              bslib::card_header(
                "Growth rate by group",
                bslib::tooltip(
                  bsicons::bs_icon("info-circle"),
                  "Growth rate with respect to first reference date, by group."
                )
              ),
              plotly::plotlyOutput(shiny::NS(id, "wagebill_change")),
              min_height = "450px"
            )
          ),
          bslib::card(
            full_screen = TRUE,
            fillable = FALSE,
            bslib::card_header(
              "Variation",
              bslib::tooltip(
                bsicons::bs_icon("info-circle"),
                "Variation in wages by group, for the latest year in the selected time frame."
              )
            ),
            plotly::plotlyOutput(shiny::NS(id, "wagebill_variation")),
            min_height = "450px"
          )
        )
      ),
      bslib::nav_panel(
        title = "Evolution",
        bslib::layout_sidebar(
          title = "Wagebill: Animation",
          sidebar = bslib::sidebar(
            title = "Controls",
            width = "300px",
            shinyWidgets::numericRangeInput(
              shiny::NS(id, "date_range"),
              "Time frame:",
              value = c(
                min(lubridate::year(wagebill_data$ref_date), na.rm = TRUE),
                max(lubridate::year(wagebill_data$ref_date), na.rm = TRUE)
              ),
              min = min(lubridate::year(wagebill_data$ref_date), na.rm = TRUE),
              max = max(lubridate::year(wagebill_data$ref_date), na.rm = TRUE)
            ),
            shiny::selectInput(
              shiny::NS(id, "wagebill_measure"),
              "Type of Wage:",
              choices = wagebill_measure_choices
            ),
            shiny::selectInput(
              shiny::NS(id, "wagebill_group"),
              "Group:",
              choices = wagebill_group_choices
            ),
            shiny::uiOutput(shiny::NS(id, "group_filter_ui_evolution"))
          ),
          bslib::card(
            full_screen = TRUE,
            bslib::card_header(
              "Animation",
              bslib::tooltip(
                bsicons::bs_icon("info-circle"),
                "Evolution of wage bill and headcount, by group."
              )
            ),
            plotly::plotlyOutput(shiny::NS(id, "wagebill_animation")),
            min_height = "450px"
          )
        )
      )
    ),
    col_widths = c(12, 12)
  )
}

#' Wage Bill Server Module
#'
#' Server logic for wage bill analytics UI.
#'
#' @param id Module id.
#' @param wagebill_data Data frame with wage bill data.
#'
#' @importFrom shiny moduleServer reactive validate need bindEvent downloadHandler withProgress incProgress renderUI uiOutput selectizeInput debounce
#' @importFrom plotly renderPlotly ggplotly plot_ly layout animation_opts animation_slider
#' @importFrom dplyr filter mutate arrange group_by ungroup across all_of first last pull left_join summarise n_distinct
#' @importFrom lubridate year years
#' @importFrom govhr compute_fastsummary convert_constant_ppp
#' @importFrom ggplot2 ggplot aes geom_point geom_line geom_col geom_hline geom_vline scale_y_continuous scale_x_continuous scale_y_discrete scale_color_manual guide_axis labs xlab ylab
#' @importFrom grDevices colorRampPalette
#' @importFrom stats reorder
#' @importFrom scales label_number cut_short_scale comma
#' @importFrom stringr str_wrap
#' @importFrom rmarkdown render
#' @export
wagebill_server <- function(id, wagebill_data) {
  shiny::moduleServer(id, function(input, output, session) {
    wagebill_filtered_date <- shiny::reactive({
      wagebill_data |>
        dplyr::mutate(
          year = lubridate::year(.data[["ref_date"]])
        ) |>
        dplyr::filter(
          .data[["year"]] >= input$date_range[1],
          .data[["year"]] <= input$date_range[2]
        )
    })

    # Conditional UI: group filter when > 8 unique values
    output$group_filter_ui <- shiny::renderUI({
      shiny::req(input$wagebill_group != "ref_date")
      group_vals <- sort(as.character(unique(na.omit(
        wagebill_data[[input$wagebill_group]]
      ))))
      if (length(group_vals) > 8) {
        shiny::selectizeInput(
          session$ns("group_filter"),
          "Groups to display:",
          choices = group_vals,
          selected = group_vals,
          multiple = TRUE,
          options = list(plugins = list("remove_button"))
        )
      }
    })
    shiny::outputOptions(output, "group_filter_ui", suspendWhenHidden = FALSE)

    output$group_filter_ui_evolution <- shiny::renderUI({
      shiny::req(input$wagebill_group != "ref_date")
      group_vals <- sort(as.character(unique(na.omit(
        wagebill_data[[input$wagebill_group]]
      ))))
      if (length(group_vals) > 8) {
        shiny::selectizeInput(
          session$ns("group_filter"),
          "Display groups:",
          choices = group_vals,
          selected = group_vals,
          multiple = TRUE,
          options = list(plugins = list("remove_button"))
        )
      }
    })
    shiny::outputOptions(output, "group_filter_ui_evolution", suspendWhenHidden = FALSE)

    # Debounced group filter to avoid over-triggering computation
    input_group_filter_debounced <- shiny::debounce(shiny::reactive(input$group_filter), 1000)

    # Data filtered by both date and selected groups
    wagebill_group_filtered <- shiny::reactive({
      data <- wagebill_filtered_date()
      if (input$wagebill_group == "ref_date") return(data)
      group_vals <- unique(na.omit(wagebill_data[[input$wagebill_group]]))
      if (length(group_vals) > 8 && !is.null(input_group_filter_debounced())) {
        data <- data |>
          dplyr::filter(.data[[input$wagebill_group]] %in% input_group_filter_debounced())
      }
      data
    })

    wagebill_summary <- shiny::reactive({
      if (input$wagebill_group == "ref_date") {
        wagebill_out <- wagebill_group_filtered() |>
          govhr::compute_fastsummary(
            cols = input$wagebill_measure,
            fns = "sum",
            groups = c(input$wagebill_group)
          )
      } else {
        wagebill_out <- wagebill_group_filtered() |>
          govhr::compute_fastsummary(
            cols = input$wagebill_measure,
            fns = "sum",
            groups = c("ref_date", input$wagebill_group)
          )
      }

      # if growth rate toggle is on
      if (input$toggle_growth) {
        if (input$wagebill_group == "ref_date") {
          wagebill_out <- wagebill_out |>
            dplyr::arrange(.data[["ref_date"]]) |>
            dplyr::mutate(
              value = .data[["value"]] / dplyr::first(.data[["value"]]) * 100
            )
        } else {
          wagebill_out <- wagebill_out |>
            dplyr::group_by(dplyr::across(dplyr::all_of(input$wagebill_group))) |>
            dplyr::arrange(.data[["ref_date"]]) |>
            dplyr::mutate(
              value = .data[["value"]] / dplyr::first(.data[["value"]]) * 100
            ) |>
            dplyr::ungroup()
        }
      }

      wagebill_out
    })

    # plot 1. panel
    output$wagebill_panel <- plotly::renderPlotly({
      plot <- wagebill_summary() |>
        ggplot2::ggplot(
          ggplot2::aes(
            x = .data[["ref_date"]],
            y = .data[["value"]]
          )
        ) +
        ggplot2::geom_point() +
        ggplot2::geom_line() +
        ggplot2::scale_y_continuous(
          labels = scales::label_number(scale_cut = scales::cut_short_scale())
        ) +
        ggplot2::xlab("Time")

      if (input$wagebill_group != "ref_date") {
        n_groups <- dplyr::n_distinct(wagebill_summary()[[input$wagebill_group]], na.rm = TRUE)
        orange_palette <- colorRampPalette(c("#C34729", "#F5C6A0"))(n_groups)
        plot <- plot +
          ggplot2::aes(
            color = .data[[input$wagebill_group]],
            group = .data[[input$wagebill_group]]
          ) +
          ggplot2::scale_color_manual(values = orange_palette)
      }

      if (input$toggle_growth) {
        plot <- plot +
          ggplot2::ylab("Growth Rate (Base = 100%)") +
          ggplot2::geom_hline(
            yintercept = 100,
            linetype = "dashed",
            color = "red3"
          )
      } else {
        plot <- plot +
          ggplot2::ylab("Wage Bill")
      }

      plotly::ggplotly(plot)
    })

    # plot 2. total by group
    output$wagebill_cross_section <- plotly::renderPlotly({
      shiny::validate(
        shiny::need(input$wagebill_group != "ref_date", "Please select a group.")
      )

      cross_section_data <- wagebill_group_filtered() |>
        group_by(
          across(
            all_of(input$wagebill_group)
          )
        ) |>
        # only present latest reference date
        dplyr::filter(
          ref_date == max(ref_date)
        ) |>
        govhr::compute_fastsummary(
          cols = input$wagebill_measure,
          fns = "sum",
          groups = input$wagebill_group
        ) |>
        # drop missing values and groups
        dplyr::filter(
          !is.na(.data[["value"]]) &
            !is.na(.data[[input$wagebill_group]])
        )

      # dynamic height
      n_groups <- nrow(cross_section_data)
      plot_height <- max(350, n_groups * 35 + 100)

      plot <- cross_section_data |>
        ggplot2::ggplot(
          ggplot2::aes(
            x = .data[["value"]],
            y = stats::reorder(
              stringr::str_wrap(.data[[input$wagebill_group]], width = 30),
              .data[["value"]]
            )
          )
        ) +
        ggplot2::geom_col() +
        ggplot2::scale_x_continuous(
          labels = scales::label_number(scale_cut = scales::cut_short_scale())
        ) +
        scale_y_discrete(
          guide = guide_axis(n.dodge = 2)
        ) +
        ggplot2::labs(
          x = "Wage bill",
          y = ""
        )

      plotly::ggplotly(plot, height = plot_height)
    }) |>
      shiny::bindEvent(input$wagebill_group, input$wagebill_measure, input$date_range, input_group_filter_debounced())

    # plot 3. growth rate by group
    output$wagebill_change <- plotly::renderPlotly({
      shiny::validate(
        shiny::need(input$wagebill_group != "ref_date", "Please select a group.")
      )

      change_data <- wagebill_group_filtered() |>
        group_by(
          across(
            all_of(input$wagebill_group)
          )
        ) |> 
        dplyr::filter(
          ref_date %in% c(max(ref_date), min(ref_date))
        ) |>
        govhr::compute_fastsummary(
          cols = input$wagebill_measure,
          fns = "sum",
          groups = c("ref_date", input$wagebill_group)
        ) |>
        dplyr::filter(!is.na(.data[[input$wagebill_group]])) |>
        dplyr::group_by(dplyr::across(dplyr::all_of(input$wagebill_group))) |>
        dplyr::summarise(
          growth_rate = round(
            dplyr::last(.data[["value"]]) / dplyr::first(.data[["value"]]) - 1,
            3
          ) * 100,
          .groups = "drop"
        ) |>
        dplyr::filter(!is.na(.data[["growth_rate"]]))

      # dynamic height
      n_groups <- nrow(change_data)
      plot_height <- max(350, n_groups * 35 + 100)

      plot <- change_data |>
        ggplot2::ggplot(
          ggplot2::aes(
            x = .data[["growth_rate"]],
            y = stats::reorder(
              stringr::str_wrap(.data[[input$wagebill_group]], width = 30),
              .data[["growth_rate"]]
            )
          )
        ) +
        ggplot2::geom_col() +
        geom_vline(
          xintercept = 0,
          linewidth = 1.25,
          linetype = "dashed",
          color = "#2958c3"
        ) +
        ggplot2::scale_x_continuous(
          labels = scales::label_number(scale_cut = scales::cut_short_scale())
        ) +
        scale_y_discrete(
          guide = guide_axis(n.dodge = 2)
        ) +
        ggplot2::labs(
          x = "Growth rate",
          y = ""
        )

      plotly::ggplotly(plot, height = plot_height)
    }) |>
      shiny::bindEvent(input$wagebill_group, input$wagebill_measure, input$date_range, input_group_filter_debounced())

    # plot 4. variation
    output$wagebill_variation <- plotly::renderPlotly({
      shiny::validate(
        shiny::need(input$wagebill_group != "ref_date", "Please select a group.")
      )

      dispersion_data <- wagebill_group_filtered() |>
        govhr::convert_constant_ppp(
          cols = input$wagebill_measure,
          macro_indicators = govhr::macro_indicators
        ) |>
        group_by(
          across(
            all_of(input$wagebill_group)
          )
        ) |> 
        # only present latest reference date
        dplyr::filter(
          ref_date == max(ref_date)
        )

      # dynamic height
      n_groups <- dispersion_data |>
        dplyr::filter(!is.na(.data[[input$wagebill_group]])) |>
        dplyr::pull(input$wagebill_group) |>
        unique() |>
        length()

      plot_height <- max(350, n_groups * 25 + 100)

      plot <- dispersion_data |>
        plot_segment(
          col = input$wagebill_measure,
          group = input$wagebill_group
        ) +
        scale_y_discrete(
          guide = guide_axis(n.dodge = 2)
        ) +
        ggplot2::labs(
          x = "Wage bill",
          y = ""
        )

      plotly::ggplotly(plot, height = plot_height)
    }) |>
      shiny::bindEvent(input$wagebill_group, input$wagebill_measure, input$date_range, input_group_filter_debounced())

    # plot 5. wagebill animation
    output$wagebill_animation <- plotly::renderPlotly({
      shiny::validate(
        shiny::need(input$wagebill_group != "ref_date", "Please select a group.")
      )

      animation_data <- {
        wagebill <- wagebill_group_filtered() |>
          govhr::compute_fastsummary(
            cols = input$wagebill_measure,
            fns = "sum",
            groups = c("ref_date", input$wagebill_group)
          ) |>
          dplyr::filter(
            !is.na(.data[["value"]]) &
              !is.na(.data[[input$wagebill_group]])
          )

        personnel <- wagebill_group_filtered() |> 
          group_by(
            across(
              all_of(
                c("ref_date", input$wagebill_group)
              )
            )
          ) |> 
          summarise(
            headcount = n_distinct(.data[["personnel_id"]])
          )

        wagebill |> 
          left_join(
            personnel,
            by = c("ref_date", input$wagebill_group)
          )
      }

      plotly::plot_ly(
        data = animation_data,
        x = ~headcount,
        y = ~value,
        frame = ~ref_date,
        text = ~paste0(
          input$wagebill_group, ": ", get(input$wagebill_group), "<br>",
          "Headcount: ", scales::comma(headcount), "<br>",
          "Wage bill: ", scales::comma(value)
        ),
        hoverinfo = "text",
        type = "scatter",
        mode = "markers",
        marker = list(size = 10, opacity = 0.7)
      ) |>
        plotly::layout(
          xaxis = list(
            title = "Headcount (log scale)",
            type = "log",
            dtick = 1
          ),
          yaxis = list(
            title = "Wage bill (log scale)",
            type = "log",
            dtick = 1
          )
        ) |>
        plotly::animation_opts(
          frame = 500,
          transition = 300,
          redraw = FALSE
        ) |>
        plotly::animation_slider(
          currentvalue = list(prefix = "Date: ")
        )
    }) |>
      shiny::bindEvent(input$wagebill_group, input$wagebill_measure, input$date_range, input_group_filter_debounced())
    
    # report
    output$download_report <- shiny::downloadHandler(
      filename = function() {
        paste0("wagebill_report_", format(Sys.Date(), "%Y%m%d"), ".docx")
      },
      content = function(file) {
        # Show progress
        shiny::withProgress(message = 'Generating report...', value = 0, {
          
          # Increment progress
          shiny::incProgress(0.3, detail = "Creating plots...")
          
          # Generate report using helper function
          output_path <- generate_wagebill_report(
            wagebill_summary_data = wagebill_summary(),
            wagebill_filtered_data = wagebill_filtered_date(),
            date_range = input$date_range,
            wagebill_measure = input$wagebill_measure,
            wagebill_group = input$wagebill_group,
            toggle_growth = input$toggle_growth
          )
          
          shiny::incProgress(0.9, detail = "Finalizing...")
          
          # Copy generated file to download location
          file.copy(output_path, file, overwrite = TRUE)
          
          shiny::incProgress(1, detail = "Complete!")
        })
      }
    )
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
#' @import shiny
#' @import bslib
#' @import ggplot2
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
