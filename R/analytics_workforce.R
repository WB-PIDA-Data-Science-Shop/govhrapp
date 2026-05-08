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
#' @importFrom purrr keep map set_names
#' @import dplyr
#' @export
workforce_ui <- function(id, workforce_data) {
  available_cols <- names(workforce_data)

  workforce_group_choices <- c(
    list("All" = "ref_date"),
    govhr::dictionary |>
      dplyr::filter(
        .data[["variable_id"]] %in% available_cols,
        .data[["variable_class"]] == "character",
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
      choices = workforce_group_choices
    ),
    shiny::uiOutput(shiny::NS(id, "group_filter_ui")),
    shiny::downloadButton(
      shiny::NS(id, "download_report"),
      "Generate report",
      icon = shiny::icon("file-word")
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
      choices = workforce_group_choices
    ),
    shiny::uiOutput(shiny::NS(id, "group_filter_ui_movement")),
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
        title = "Overview",
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
                  "Growth rate with respect to first reference date, by group."
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
                "Personnel movements (hires or separations) over time, showing the share of workforce affected."
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
#' @importFrom shiny moduleServer reactive validate need bindEvent downloadHandler withProgress incProgress renderUI outputOptions selectizeInput debounce
#' @importFrom plotly renderPlotly
#' @importFrom dplyr filter mutate arrange group_by ungroup summarise across all_of first last n_distinct right_join
#' @importFrom lubridate year years ymd
#' @importFrom rlang :=
#' @importFrom govhr fastcount detect_personnel_event
#' @importFrom ggplot2 ggplot aes geom_point geom_line geom_col geom_hline geom_vline scale_y_continuous scale_x_continuous scale_y_discrete scale_color_manual guide_axis labs xlab ylab
#' @importFrom grDevices colorRampPalette
#' @importFrom plotly ggplotly
#' @importFrom stats reorder
#' @importFrom scales label_number cut_short_scale pretty_breaks percent_format
#' @importFrom data.table fifelse shift setorderv as.data.table copy
#' @importFrom rmarkdown render
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

    # Conditional UI: group filter when > 8 unique values
    group_filter_widget <- function(input_id) {
      shiny::req(input$workforce_group != "ref_date")
      group_vals <- sort(as.character(unique(na.omit(
        workforce_data[[input$workforce_group]]
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
    }

    output$group_filter_ui <- shiny::renderUI(group_filter_widget())
    shiny::outputOptions(output, "group_filter_ui", suspendWhenHidden = FALSE)

    output$group_filter_ui_movement <- shiny::renderUI(group_filter_widget())
    shiny::outputOptions(output, "group_filter_ui_movement", suspendWhenHidden = FALSE)

    # Debounced group filter to avoid over-triggering computation
    input_group_filter_debounced <- shiny::debounce(shiny::reactive(input$group_filter), 1000)

    # Data filtered by both date and selected groups
    workforce_group_filtered <- shiny::reactive({
      data <- workforce_filtered_date()
      if (input$workforce_group == "ref_date") return(data)
      
      group_vals <- unique(na.omit(workforce_data[[input$workforce_group]]))
      
      if (length(group_vals) > 8 && !is.null(input_group_filter_debounced())) {
        data <- data |>
          dplyr::filter(.data[[input$workforce_group]] %in% input_group_filter_debounced())
      }
      data
    })

    workforce_summary <- reactive({
      if (input$workforce_group == "ref_date") {
        workforce_out <- workforce_group_filtered() |>
          govhr::fastcount(.data[["ref_date"]], name = "value")
      } else {
        workforce_out <- workforce_group_filtered() |>
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
        n_groups <- dplyr::n_distinct(workforce_summary()[[input$workforce_group]], na.rm = TRUE)
        orange_palette <- colorRampPalette(c("#C34729", "#F5C6A0"))(n_groups)
        plot <- plot +
          aes(
            color = .data[[input$workforce_group]],
            group = .data[[input$workforce_group]]
          ) +
          ggplot2::scale_color_manual(values = orange_palette)
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

      cross_section_data <- workforce_group_filtered() |>
        group_by(
          across(
            all_of(input$workforce_group)
          )
        ) |> 
        filter(year == max(year)) |>
        govhr::fastcount(.data[[input$workforce_group]], name = "value") |>
        filter(
          !is.na(.data[["value"]]) &
            !is.na(.data[[input$workforce_group]])
        )

      # dynamic height
      n_groups <- nrow(cross_section_data)
      plot_height <- max(350, n_groups * 35 + 100)

      plot <- cross_section_data |>
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
      bindEvent(input$workforce_group, input$date_range, input_group_filter_debounced())

    # plot 3. growth rate by group
    output$workforce_growth <- plotly::renderPlotly({
      validate(
        need(input$workforce_group != "ref_date", "Please select a group.")
      )

      change_data <- workforce_group_filtered() |>
        group_by(
          across(
            all_of(input$workforce_group)
          )
        ) |> 
        dplyr::filter(
          ref_date %in% c(max(ref_date), min(ref_date))
        ) |>
        govhr::fastcount(
          .data[["ref_date"]],
          .data[[input$workforce_group]],
          name = "value"
        ) |>
        dplyr::filter(!is.na(.data[[input$workforce_group]])) |>
        dplyr::group_by(dplyr::across(dplyr::all_of(input$workforce_group))) |>
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

      plot_growth <- change_data |>
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
          linewidth = 1.25,
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
      bindEvent(input$workforce_group, input$date_range, input_group_filter_debounced())

    # plot 4. movements
    output$workforce_movements <- renderPlotly({
      min_date <- min(workforce_group_filtered()[["ref_date"]]) |>
        as.character()
      max_date <- max(workforce_group_filtered()[["ref_date"]]) |>
        as.character()

      movement_data <- workforce_group_filtered() |>
        govhr::detect_personnel_event(
          event_type = input$movement_type,
          id_col = "personnel_id",
          start_date = min_date,
          end_date = max_date,
          freq = "year"
        ) |>
        dplyr::right_join(
          workforce_group_filtered(),
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

      plot_movement <- movement_data |>
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
      bindEvent(input$movement_type, input$workforce_group, input$date_range, input_group_filter_debounced())

    # report
    output$download_report <- shiny::downloadHandler(
      filename = function() {
        paste0("workforce_report_", format(Sys.Date(), "%Y%m%d"), ".docx")
      },
      content = function(file) {
        # Show progress
        shiny::withProgress(message = 'Generating report...', value = 0, {
          
          # Increment progress
          shiny::incProgress(0.3, detail = "Creating plots...")
          
          # Generate report using helper function
          output_path <- generate_workforce_report(
            workforce_summary_data = workforce_summary(),
            workforce_filtered_data = workforce_filtered_date(),
            date_range = input$date_range,
            workforce_group = input$workforce_group,
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
#' @import shiny
#' @import bslib
#' @import ggplot2
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
