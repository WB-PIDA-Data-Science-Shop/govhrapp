#' Workforce UI Module
#'
#' UI for workforce analytics, including overview, controls, and plots.
#'
#' @param id Module id.
#' @param workforce_data Data frame with workforce data.
#'
#' @importFrom bslib layout_columns card card_header card_body accordion accordion_panel layout_sidebar sidebar tooltip
#' @importFrom bsicons bs_icon
#' @importFrom shiny markdown icon NS selectInput
#' @importFrom shinyWidgets numericRangeInput materialSwitch
#' @importFrom plotly plotlyOutput
#' @importFrom stringr str_wrap
#' @importFrom lubridate year
#' @export
workforce_ui <- function(id, workforce_data) {
  sidebar_default <- bslib::sidebar(
    title = "Controls",
    width = "300px",
    shinyWidgets::numericRangeInput(
      shiny::NS(id, "date_range"),
      "Time frame:",
      value = c(
        min(lubridate::year(workforce_data$ref_date), na.rm = TRUE),
        max(lubridate::year(workforce_data$ref_date), na.rm = TRUE)
      ),
      min = min(lubridate::year(workforce_data$ref_date), na.rm = TRUE),
      max = max(lubridate::year(workforce_data$ref_date), na.rm = TRUE)
    ),
    shiny::selectInput(
      shiny::NS(id, "workforce_group"),
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
  )

  sidebar_movement <- bslib::sidebar(
    title = "Controls",
    width = "300px",
    shinyWidgets::numericRangeInput(
      shiny::NS(id, "date_range"),
      "Time frame:",
      value = c(
        min(lubridate::year(workforce_data$ref_date), na.rm = TRUE),
        max(lubridate::year(workforce_data$ref_date), na.rm = TRUE)
      ),
      min = min(lubridate::year(workforce_data$ref_date), na.rm = TRUE),
      max = max(lubridate::year(workforce_data$ref_date), na.rm = TRUE)
    ),
    shiny::selectInput(
      shiny::NS(id, "workforce_group"),
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
    ),
    shiny::selectInput(
      shiny::NS(id, "movement_type"),
      "Movement type:",
      choices = list(
        "Hires" = "hire",
        "Separations" = "fire"
      )
    )
  )

  bslib::layout_columns(
    bslib::card(
      bslib::card_header(
        "Workforce: Overview"
      ),
      bslib::card_body(
        shiny::markdown(
          readLines("inst/markdown/workforce.md")
        )
      )
    ),
    bslib::accordion(
      bslib::accordion_panel(
        title = "Guidance Questions",
        icon = shiny::icon("question-circle"),
        shiny::markdown(
          readLines("inst/markdown/workforce_questions.md")
        )
      ),
      open = FALSE
    ),
    bslib::page_navbar(
      bslib::nav_panel(
        title = "Headcount",
        bslib::layout_sidebar(
          title = "Workforce: Headcount",
          sidebar = sidebar_default,
          bslib::card(
            full_screen = TRUE,
            bslib::card_header(
              "Headcount",
              bslib::tooltip(
                bsicons::bs_icon("info-circle"),
                "Headcount trends over time. Choosing a group will add new trend lines, by group."
              )
            ),
            shinyWidgets::materialSwitch(
              shiny::NS(id, "toggle_growth"),
              label = "Switch to baseline index",
              value = FALSE
            ),
            plotly::plotlyOutput(NS(id, "workforce_panel")),
            min_height = "350px"
          ),
          layout_columns(
            col_widths = c(6, 6),
            bslib::card(
              full_screen = TRUE,
              fillable = FALSE,
              bslib::card_header(
                "Total by group",
                bslib::tooltip(
                  bsicons::bs_icon("info-circle"),
                  "Headcount total, by group. Total refers to the latest available year in the selected time frame."
                )
              ),
              plotly::plotlyOutput(NS(id, "workforce_cross_section")),
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
              plotly::plotlyOutput(NS(id, "workforce_growth")),
              min_height = "450px"
            )
          )
        )
      ),
      bslib::nav_panel(
        title = "Movements",
        bslib::layout_sidebar(
          title = "Workforce: Movements",
          sidebar = sidebar_movement,
          bslib::card(
            full_screen = TRUE,
            bslib::card_header(
              "Movements",
              bslib::tooltip(
                bsicons::bs_icon("info-circle"),
                "Personnel movements (hires or separations) over time, showing the share of employees affected."
              )
            ),
            plotly::plotlyOutput(NS(id, "workforce_movements")),
            min_height = "350px"
          )
        )
      )
    ),
    col_widths = c(12, 12)
  )
}

#' Workforce Server Module
#'
#' Server logic for workforce analytics UI.
#'
#' @param id Module id.
#' @param workforce_data Data frame with workforce data.
#'
#' @importFrom shiny moduleServer reactive validate need bindEvent
#' @importFrom plotly renderPlotly
#' @importFrom dplyr filter mutate arrange group_by ungroup summarise across first lag right_join
#' @importFrom lubridate year years ymd
#' @importFrom govhr fastcount complete_dates detect_personnel_event
#' @importFrom ggplot2 ggplot aes geom_point geom_line geom_col geom_hline geom_vline scale_y_continuous scale_x_continuous labs xlab ylab
#' @importFrom plotly ggplotly
#' @importFrom stats reorder
#' @importFrom scales label_number cut_short_scale pretty_breaks
#' @importFrom data.table fifelse shift setorderv as.data.table copy
#' @importFrom dplyr across
#' @importFrom scales percent_format
#' @export
workforce_server <- function(id, workforce_data) {
  shiny::moduleServer(id, function(input, output, session) {
    workforce_filtered_date <- reactive({
      workforce_data |>
        mutate(
          year = lubridate::year(.data[["ref_date"]])
        ) |>
        dplyr::filter(
          .data[["year"]] >= input$date_range[1],
          .data[["year"]] <= input$date_range[2]
        )
    })

    workforce_summary <- reactive({
      if (input$workforce_group == "ref_date") {
        workforce_out <- workforce_filtered_date() |>
          govhr::fastcount(.data[["ref_date"]], name = "value")
      } else {
        workforce_out <- workforce_filtered_date() |>
          govhr::fastcount(
            .data[["ref_date"]],
            .data[[input$workforce_group]],
            name = "value"
          )
      }

      # if growth rate toggle is on
      if (input$toggle_growth) {
        if (input$workforce_group == "ref_date") {
          workforce_out <- workforce_out |>
            dplyr::arrange(.data[["ref_date"]]) |>
            dplyr::mutate(
              value = .data[["value"]] / dplyr::first(.data[["value"]]) * 100
            )
        } else {
          workforce_out <- workforce_out |>
            dplyr::group_by(across(all_of(input$workforce_group))) |>
            dplyr::arrange(.data[["ref_date"]]) |>
            dplyr::mutate(
              value = .data[["value"]] / dplyr::first(.data[["value"]]) * 100
            ) |>
            dplyr::ungroup()
        }
      }

      workforce_out
    })

    # plot 1. panel
    output$workforce_panel <- plotly::renderPlotly({
      plot <- workforce_summary() |>
        ggplot(
          aes(
            x = .data[["ref_date"]],
            y = .data[["value"]]
          )
        ) +
        geom_point() +
        geom_line() +
        scale_y_continuous(
          labels = scales::label_number(scale_cut = scales::cut_short_scale())
        ) +
        xlab("Time")

      if (input$workforce_group != "ref_date") {
        plot <- plot +
          aes(group = .data[[input$workforce_group]])
      }

      if (input$toggle_growth) {
        plot <- plot +
          ylab("Growth Rate (Base = 100%)") +
          geom_hline(
            yintercept = 100,
            linetype = "dashed",
            color = "red3"
          )
      } else {
        plot <- plot +
          ylab("Headcount")
      }

      plotly::ggplotly(plot)
    })

    # plot 2. total by group
    output$workforce_cross_section <- plotly::renderPlotly({
      validate(
        need(input$workforce_group != "ref_date", "Please select a group.")
      )

      cross_section_data <- reactive({
        workforce_filtered_date() |>
          filter(year == max(year)) |>
          govhr::fastcount(.data[[input$workforce_group]], name = "value") |>
          filter(
            !is.na(.data[["value"]]) &
              !is.na(.data[[input$workforce_group]])
          )
      })

      # dynamic height
      n_groups <- nrow(cross_section_data())
      plot_height <- max(350, n_groups * 35 + 100)

      plot <- cross_section_data() |>
        ggplot(
          aes(
            x = .data[["value"]],
            y = stats::reorder(
              stringr::str_wrap(.data[[input$workforce_group]], width = 30),
              .data[["value"]]
            )
          )
        ) +
        geom_col() +
        scale_x_continuous(
          labels = scales::label_number(scale_cut = scales::cut_short_scale())
        ) +
        scale_y_discrete(
          guide = guide_axis(n.dodge = 2)
        ) +
        labs(
          x = "Headcount",
          y = ""
        )

      plotly::ggplotly(plot, height = plot_height)
    }) |>
      bindEvent(input$workforce_group, input$date_range)

    # plot 3. growth rate by group
    output$workforce_growth <- plotly::renderPlotly({
      validate(
        need(input$workforce_group != "ref_date", "Please select a group.")
      )

      change_data <- reactive({
        workforce_annual <- workforce_filtered_date() |>
          dplyr::filter(
            year %in% c(max(year), max(year) - 1)
          ) |>
          govhr::fastcount(
            .data[["ref_date"]],
            .data[[input$workforce_group]],
            name = "value"
          )

        workforce_annual |>
          dplyr::group_by(
            across(all_of(input$workforce_group))
          ) |>
          govhr::complete_dates(
            id_col = input$workforce_group,
            start_date = max(workforce_data$ref_date) - lubridate::years(1),
            end_date = max(workforce_data$ref_date)
          ) |>
          dplyr::mutate(
            growth_rate = round(
              .data[["value"]] / dplyr::lag(.data[["value"]]) - 1,
              3
            ) *
              100
          ) |>
          filter(
            .data[["ref_date"]] == max(.data[["ref_date"]]) &
              !is.na(.data[["growth_rate"]]) &
              !is.na(.data[[input$workforce_group]])
          )
      })

       # dynamic height
      n_groups <- nrow(change_data())
      plot_height <- max(350, n_groups * 35 + 100)

      plot_growth <- change_data() |>
        ggplot(
          aes(
            x = .data[["growth_rate"]],
            y = stats::reorder(
              stringr::str_wrap(.data[[input$workforce_group]], width = 30),
              .data[["growth_rate"]]
            )
          )
        ) +
        geom_col() +
        geom_vline(
          xintercept = 0,
          linewidth = 1.5,
          linetype = "dashed",
          color = "#2958c3"
        ) +
        scale_x_continuous(
          labels = scales::label_number(scale_cut = scales::cut_short_scale())
        ) +
        scale_y_discrete(
          guide = guide_axis(n.dodge = 2)
        ) +
        labs(
          x = "Growth rate",
          y = ""
        )

      plotly::ggplotly(plot_growth, height = plot_height)
    }) |>
      bindEvent(input$workforce_group, input$date_range)

    # plot 4. movements
    output$workforce_movements <- renderPlotly({
      movement_data <- reactive({
        min_date <- min(workforce_filtered_date()[["ref_date"]]) |>
          as.character()
        max_date <- max(workforce_filtered_date()[["ref_date"]]) |>
          as.character()

        workforce_filtered_date() |>
          govhr::detect_personnel_event(
            event_type = input$movement_type,
            id_col = "personnel_id",
            start_date = min_date,
            end_date = max_date,
            freq = "year"
          ) |>
          dplyr::right_join(
            workforce_filtered_date(),
            by = c("personnel_id", "ref_date")
          ) |>
          dplyr::group_by(
            across(
              all_of(
                unique(c("ref_date", input$workforce_group))
              )
            )
          ) |>
          summarise(
            share = mean(!is.na(.data[["type_event"]]))
          )
      })

      plot_movement <- movement_data() |>
        ggplot(
          aes(.data[["ref_date"]], .data[["share"]])
        ) +
        geom_point() +
        geom_line() +
        labs(
          x = "Time",
          y = "Share"
        )

      if (input$workforce_group != "ref_date") {
        plot_movement <- plot_movement +
          aes(group = .data[[input$workforce_group]])
      }

      plot_movement <- plot_movement +
        scale_y_continuous(
          labels = scales::percent_format()
        )

      plotly::ggplotly(plot_movement)
    }) |>
      bindEvent(input$workforce_group, input$date_range)
  })
}

#' Run the Workforce Shiny Application
#'
#' Launches an interactive Shiny application for analyzing workforce headcount data,
#' including time trends, cross-sectional comparisons, and growth rate analysis.
#'
#' @param workforce_data A data frame containing workforce information with the
#'   following required columns:
#'   \itemize{
#'     \item \code{ref_date}: Reference date (Date class)
#'     \item \code{personnel_id}: Personnel identifier
#'     \item \code{est_id}: Establishment identifier
#'     \item \code{contract_type_native}: Contract type
#'     \item \code{paygrade}: Paygrade classification
#'     \item \code{occupation_native}: Occupation classification
#'     \item \code{gender}: Gender
#'     \item \code{educat7}: Education level
#'     \item \code{status}: Employment status
#'   }
#'   Defaults to filtered Brazilian HRMIS contract data (years <= 2017).
#' @param ... Additional arguments passed to \code{\link[shiny]{shinyApp}}.
#'
#' @return A Shiny app object.
#'
#' @details
#' The application provides three main analytical views:
#' \itemize{
#'   \item Time trend analysis with optional baseline indexing
#'   \item Cross-sectional headcount totals by group
#'   \item Year-over-year growth rates by group
#'   \item Interactive filtering by time period and grouping variable
#' }
#'
#' @examples
#' \dontrun{
#' # Run with default data
#' run_workforceapp()
#'
#' # Run with custom data
#' my_data <- govhr::bra_hrmis_contract |>
#'   dplyr::filter(lubridate::year(ref_date) >= 2015)
#' run_workforceapp(workforce_data = my_data)
#' }
#'
#' @importFrom shiny shinyApp
#' @importFrom bslib page_sidebar sidebar
#' @importFrom plotly plotlyOutput renderPlotly ggplotly
#' @export
run_workforceapp <- function(
  workforce_data,
  ...
) {
  ui <- workforce_ui("test", workforce_data)

  server <- function(input, output, session) {
    workforce_server("test", workforce_data)
  }

  shiny::shinyApp(ui, server)
}
