#' Wage Bill UI Module
#'
#' UI for wage bill analytics, including overview, controls, and plots.
#'
#' @param id Module id.
#' @param wagebill_data Data frame with wage bill data.
#'
#' @importFrom bslib layout_columns card card_header card_body accordion accordion_panel layout_sidebar sidebar tooltip
#' @importFrom bsicons bs_icon
#' @importFrom shiny markdown icon NS selectInput
#' @importFrom shinyWidgets numericRangeInput materialSwitch
#' @importFrom plotly plotlyOutput
#' @importFrom stringr str_wrap
#' @importFrom lubridate year
#' @export
wagebill_ui <- function(id, wagebill_data) {
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
          choices = list(
            "Base Salary" = "base_salary_lcu",
            "Gross Salary" = "gross_salary_lcu",
            "Net Salary" = "net_salary_lcu"
          )
        ),
        shiny::selectInput(
          shiny::NS(id, "wagebill_group"),
          "Group:",
          choices = list(
            "All" = "ref_date",
            "Establishment" = "est_id",
            "Contract" = c(
              "Contract type (native)" = "contract_type_native",
              "Paygrade" = "paygrade",
              "Occupation" = "occupation_native"
            ),
            "Personnel" = c(
              "Gender" = "gender",
              "Education" = "educat7",
              "Employment Status" = "status"
            )
          )
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
              "Growth rate with respect to the previous year, by group. Growth rate refers to the latest available year in the selected time frame."
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
#' @importFrom shiny moduleServer reactive validate need bindEvent
#' @importFrom plotly renderPlotly
#' @importFrom dplyr filter mutate arrange group_by ungroup across all_of first lag pull
#' @importFrom lubridate year years
#' @importFrom govhr compute_fastsummary complete_dates convert_constant_ppp
#' @importFrom ggplot2 ggplot aes geom_point geom_line geom_col geom_hline geom_vline scale_y_continuous scale_x_continuous labs xlab ylab
#' @importFrom plotly ggplotly
#' @importFrom stats reorder
#' @importFrom scales label_number cut_short_scale
#' @importFrom stringr str_wrap
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

    wagebill_summary <- shiny::reactive({
      if (input$wagebill_group == "ref_date") {
        wagebill_out <- wagebill_filtered_date() |>
          govhr::compute_fastsummary(
            cols = input$wagebill_measure,
            fns = "sum",
            groups = c(input$wagebill_group)
          )
      } else {
        wagebill_out <- wagebill_filtered_date() |>
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
        plot <- plot +
          ggplot2::aes(group = .data[[input$wagebill_group]])
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

      cross_section_data <- shiny::reactive({
        wagebill_filtered_date() |>
          # only present latest year
          dplyr::filter(
            year == max(year)
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
      })
      
      # dynamic height
      n_groups <- nrow(cross_section_data())
      plot_height <- max(350, n_groups * 35 + 100)

      plot <- cross_section_data() |>
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
      shiny::bindEvent(input$wagebill_group, input$date_range)

    # plot 3. growth rate by group
    output$wagebill_change <- plotly::renderPlotly({
      shiny::validate(
        shiny::need(input$wagebill_group != "ref_date", "Please select a group.")
      )

      change_data <- shiny::reactive({
        wagebill_annual <- wagebill_filtered_date() |>
          # only present latest year
          dplyr::filter(
            year %in% c(max(year), max(year) - 1)
          ) |>
          govhr::compute_fastsummary(
            cols = input$wagebill_measure,
            fns = "sum",
            groups = c("ref_date", input$wagebill_group)
          )

        max_filtered_date <- max(wagebill_filtered_date()$ref_date, na.rm = TRUE)

        wagebill_annual |>
          dplyr::group_by(
            dplyr::across(dplyr::all_of(input$wagebill_group))
          ) |>
          govhr::complete_dates(
            id_col = input$wagebill_group,
            start_date = max_filtered_date - lubridate::years(1),
            end_date = max_filtered_date
          ) |>
          dplyr::mutate(
            growth_rate = round(
              .data[["value"]] / dplyr::lag(.data[["value"]]) - 1,
              3
            ) *
              100
          ) |>
          dplyr::filter(
            # filter only latest value
            .data[["ref_date"]] == max_filtered_date &
              # drop missing values and groups
              !is.na(.data[["growth_rate"]]) &
              !is.na(.data[[input$wagebill_group]])
          )
      })

      # dynamic height
      n_groups <- nrow(change_data())
      plot_height <- max(350, n_groups * 35 + 100)

      plot <- change_data() |>
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
          linewidth = 1.5,
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
      shiny::bindEvent(input$wagebill_group, input$date_range)

    # plot 4. variation
    output$wagebill_variation <- plotly::renderPlotly({
      shiny::validate(
        shiny::need(input$wagebill_group != "ref_date", "Please select a group.")
      )

      dispersion_data <- shiny::reactive({
        wagebill_deflated <- wagebill_filtered_date() |>
          govhr::convert_constant_ppp(
            cols = input$wagebill_measure,
            macro_indicators = govhr::macro_indicators
          ) |>
          # only present latest year
          dplyr::filter(
            year == max(year)
          )
      })

      # dynamic height
      n_groups <- dispersion_data() |>
        dplyr::filter(!is.na(.data[[input$wagebill_group]])) |>
        dplyr::pull(input$wagebill_group) |>
        unique() |> 
        length()

      plot_height <- max(350, n_groups * 25 + 100)

      plot <- dispersion_data() |>
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
      shiny::bindEvent(input$wagebill_group, input$date_range)
  })
}

#' Run the Wage Bill Shiny Application
#'
#' Launches an interactive Shiny application for analyzing wage bill data,
#' including time trends, cross-sectional comparisons, and growth rate analysis.
#'
#' @param wagebill_data A data frame containing wage bill information with the
#'   following required columns:
#'   \itemize{
#'     \item \code{ref_date}: Reference date (Date class)
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
#' The application provides four main analytical views:
#' \itemize{
#'   \item Time trend analysis with optional baseline indexing
#'   \item Cross-sectional wage bill totals by group
#'   \item Year-over-year growth rates by group
#'   \item Wage bill dispersion and variation analysis
#'   \item Interactive filtering by time period, wage type, and grouping variable
#' }
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
