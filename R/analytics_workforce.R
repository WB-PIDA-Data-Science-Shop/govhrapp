#' Workforce UI Module
#'
#' UI for workforce analytics, including overview, controls, and plots.
#'
#' @param id Module id.
#' @param workforce_data Data frame with workforce data.
#'
#' @importFrom bslib layout_columns card card_header card_body accordion accordion_panel layout_sidebar sidebar tooltip navset_tab nav_panel
#' @importFrom bsicons bs_icon
#' @importFrom shiny markdown icon NS selectInput actionButton uiOutput
#' @importFrom shinyWidgets numericRangeInput materialSwitch pickerInput
#' @importFrom plotly plotlyOutput
#' @importFrom stringr str_wrap
#' @importFrom lubridate year
#' @importFrom purrr keep map set_names
#' @importFrom stats na.omit
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
        !.data[["variable_id"]] %in%
          c("ref_date", "contract_id", "personnel_id")
      ) |>
      dplyr::group_by(.data[["module"]]) |>
      dplyr::summarise(
        choices = list(
          purrr::set_names(.data[["variable_id"]], .data[["variable_name"]])
        ),
        .groups = "drop"
      ) |>
      dplyr::pull(.data[["choices"]], name = .data[["module"]])
  )

  filter_choices <- c(list("None" = "none"), workforce_group_choices[-1])

  year_min <- min(lubridate::year(workforce_data$ref_date), na.rm = TRUE)
  year_max <- max(lubridate::year(workforce_data$ref_date), na.rm = TRUE)

  # single shared block, used in both tabs
  shared_filter_controls <- list(
    shinyWidgets::numericRangeInput(
      shiny::NS(id, "date_range"),
      "Time frame:",
      value = c(year_min, year_max),
      min = year_min,
      max = year_max
    ),
    shiny::selectInput(
      shiny::NS(id, "workforce_filter_variable"),
      "Filter by:",
      choices = filter_choices
    ),
    shinyWidgets::pickerInput(
      shiny::NS(id, "workforce_filter_values"),
      "Select subgroups:",
      choices = NULL,
      multiple = TRUE,
      options = shinyWidgets::pickerOptions(
        actionsBox = TRUE,
        liveSearch = TRUE,
        selectedTextFormat = "count > 3",
        countSelectedText = "{0} subgroups selected",
        noneSelectedText = "No subgroups selected",
        container = "body"
      )
    )
  )

  # the namespaced id of the navset, for the conditional panel
  navset_id <- shiny::NS(id, "workforce_tabs")

  sidebar_combined <- bslib::sidebar(
    title = "Controls",
    width = "300px",
    bslib::accordion(
      bslib::accordion_panel(
        "Filters",
        icon = bsicons::bs_icon("sliders"),
        !!!shared_filter_controls
      ),
      bslib::accordion_panel(
        "Measures",
        icon = bsicons::bs_icon("bar-chart"),
        shiny::selectInput(
          shiny::NS(id, "workforce_group"),
          "Group:",
          choices = workforce_group_choices
        ),
        shiny::uiOutput(shiny::NS(id, "group_filter_ui")),
        shiny::conditionalPanel(
          condition = sprintf("input['%s'] == 'Overview'", navset_id),
          shinyWidgets::materialSwitch(
            shiny::NS(id, "toggle_growth"),
            label = "Switch to baseline index",
            value = FALSE
          )
        ),
        shiny::conditionalPanel(
          condition = sprintf("input['%s'] == 'Movements'", navset_id),
          shiny::selectInput(
            shiny::NS(id, "movement_type"),
            "Movement type:",
            choices = list(
              "Hires" = "hire",
              "Separations" = "fire",
              "Replacement rate" = "replacement_rate"
            )
          )
        )
      )
    ),

    shiny::actionButton(
      shiny::NS(id, "apply_btn"),
      "Apply selection",
      icon = shiny::icon("play")
    ),
    shiny::conditionalPanel(
      condition = sprintf("input['%s'] == 'Overview'", navset_id),
      class = "d-grid mt-2",
      shiny::downloadButton(
        shiny::NS(id, "download_report"),
        "Generate report",
        icon = shiny::icon("file-word")
      )
    )
  )

  bslib::layout_columns(
    bslib::card(
      bslib::card_header("Workforce: Overview"),
      bslib::card_body(
        shiny::markdown(
          readLines(system.file("markdown/workforce.md", package = "govhrapp"))
        )
      )
    ),
    bslib::accordion(
      bslib::accordion_panel(
        title = "Guidance Questions",
        icon = shiny::icon("question-circle"),
        shiny::markdown(
          readLines(system.file(
            "markdown/workforce_questions.md",
            package = "govhrapp"
          ))
        )
      ),
      open = FALSE
    ),

    bslib::layout_sidebar(
      fillable = FALSE,
      sidebar = sidebar_combined,
      bslib::navset_underline(
        id = navset_id,
        bslib::nav_panel(
          title = "Overview",
          bslib::card(
            full_screen = TRUE,
            bslib::card_header(
              "Headcount",
              bslib::tooltip(
                bsicons::bs_icon("info-circle"),
                "Headcount trends over time. Choosing a group will add new trend lines, by group."
              )
            ),
            plotly::plotlyOutput(
              shiny::NS(id, "workforce_panel"),
              height = "350px"
            )
          ),
          bslib::layout_columns(
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
              plotly::plotlyOutput(shiny::NS(id, "workforce_cross_section")),
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
              plotly::plotlyOutput(shiny::NS(id, "workforce_growth")),
              min_height = "450px"
            )
          )
        ),
        bslib::nav_panel(
          title = "Movements",
          bslib::card(
            full_screen = TRUE,
            bslib::card_header(
              "Movements",
              bslib::tooltip(
                bsicons::bs_icon("info-circle"),
                "Personnel movements (hires or separations) over time, showing the share of workforce affected."
              )
            ),
            plotly::plotlyOutput(
              shiny::NS(id, "workforce_movements"),
              height = "350px"
            )
          )
        )
      )
    ),
    col_widths = c(12, 12, 12)
  )
}

#' Workforce Server Module
#'
#' Server logic for workforce analytics UI.
#'
#' @param id Module id.
#' @param workforce_data Data frame with workforce data.
#'
#' @importFrom shiny moduleServer reactive validate need bindEvent downloadHandler withProgress incProgress renderUI selectizeInput actionButton
#' @importFrom shinyWidgets pickerInput pickerOptions
#' @importFrom plotly renderPlotly ggplotly
#' @importFrom dplyr filter mutate group_by ungroup summarise across all_of n_distinct right_join
#' @importFrom lubridate year years ymd
#' @importFrom govhr detect_personnel_event
#' @importFrom grDevices colorRampPalette
#' @importFrom scales label_number percent_format
#' @importFrom rmarkdown render
#' @import ggplot2
#' @export
workforce_server <- function(id, workforce_data) {
  shiny::moduleServer(id, function(input, output, session) {
    # choice of cols
    available_cols <- names(workforce_data)

    workforce_group_choices <- identify_group_choices(workforce_data)
    
    # update filter values
    shiny::observe({
      variable <- input$workforce_filter_variable

      if (is.null(variable) || variable == "none") {
        shinyWidgets::updatePickerInput(
          session,
          "workforce_filter_values",
          choices = NULL,
          selected = character(0)
        )
      } else {
        filter_vals <- sort(
          as.character(
            unique(
              stats::na.omit(
                workforce_data[[variable]]
              )
            )
          )
        )

        shinyWidgets::updatePickerInput(
          session,
          "workforce_filter_values",
          choices = filter_vals,
          selected = filter_vals
        )
      }
    })

    workforce_filtered <- shiny::reactive({
      data <- workforce_data

      if (
        !is.null(input$workforce_filter_variable) &&
          input$workforce_filter_variable != "none" &&
          length(input$workforce_filter_values) > 0
      ) {
        data <- data |>
          dplyr::filter(
            .data[[input$workforce_filter_variable]] %in%
              input$workforce_filter_values
          )
      }

      data |>
        dplyr::mutate(year = lubridate::year(.data[["ref_date"]])) |>
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
        shinyWidgets::pickerInput(
          session$ns("group_filter"),
          "Display groups:",
          choices = group_vals,
          selected = group_vals,
          multiple = TRUE,
          options = shinyWidgets::pickerOptions(
            actionsBox = TRUE,
            liveSearch = TRUE,
            selectedTextFormat = "count > 3",
            countSelectedText = "{0} groups selected",
            noneSelectedText = "No groups selected",
            container = "body"
          )
        )
      }
    }

    output$group_filter_ui <- shiny::renderUI(group_filter_widget())

    output$group_filter_ui_movement <- shiny::renderUI(group_filter_widget())

    # Data filtered by both date and selected groups
    workforce_group_filtered <- shiny::reactive({
      data <- workforce_filtered()
      if (input$workforce_group == "ref_date") {
        return(data)
      }

      group_vals <- unique(na.omit(workforce_data[[input$workforce_group]]))

      if (length(group_vals) > 8 && !is.null(input$group_filter)) {
        data <- data |>
          dplyr::filter(.data[[input$workforce_group]] %in% input$group_filter)
      }
      data
    })

    workforce_summary <- reactive({
      out <- compute_trend_summary(
        workforce_group_filtered(),
        group = input$workforce_group
      )

      if (input$toggle_growth) {
        out <- apply_baseline_index(out, group = input$workforce_group)
      }

      out
    })

    # plot 1. panel
    output$workforce_panel <- plotly::renderPlotly({
      plotly::ggplotly(
        plot_trend(
          workforce_summary(),
          group = input$workforce_group,
          toggle_growth = input$toggle_growth,
          y_label = "Headcount"
        )
      )
    }) |>
      bindEvent(input$apply_btn, ignoreNULL = FALSE)

    # plot 2. total by group
    output$workforce_cross_section <- plotly::renderPlotly({
      validate(
        need(input$workforce_group != "ref_date", "Please select a group.")
      )

      cross_section_data <- compute_cross_section_summary(
        workforce_group_filtered(),
        group = input$workforce_group
      )

      n_groups <- nrow(cross_section_data)
      plot_height <- max(350, n_groups * 35 + 100)

      plotly::ggplotly(
        plot_bar_total(
          cross_section_data,
          group = input$workforce_group,
          x_label = "Headcount"
        ),
        height = plot_height
      )
    }) |>
      bindEvent(input$apply_btn, ignoreNULL = FALSE)

    # plot 3. growth rate by group
    output$workforce_growth <- plotly::renderPlotly({
      validate(
        need(input$workforce_group != "ref_date", "Please select a group.")
      )

      change_data <- compute_growth_summary(
        workforce_group_filtered(),
        group = input$workforce_group
      )

      n_groups <- nrow(change_data)
      plot_height <- max(350, n_groups * 35 + 100)

      plotly::ggplotly(
        plot_bar_growth(change_data, group = input$workforce_group),
        height = plot_height
      )
    }) |>
      bindEvent(input$apply_btn, ignoreNULL = FALSE)

    # plot 4. movements
    output$workforce_movements <- renderPlotly({
      min_date <- min(workforce_group_filtered()[["ref_date"]]) |>
        as.character()
      max_date <- max(workforce_group_filtered()[["ref_date"]]) |>
        as.character()

      # extract frequency of reference dates for plot 4. movements
      freq_ref_date <- workforce_group_filtered() |>
        guess_date_frequency()

      if (input$movement_type %in% c("hire", "fire")) {
        movement_data <- workforce_group_filtered() |>
          govhr::detect_personnel_event(
            event_type = input$movement_type,
            id_col = "personnel_id",
            start_date = min_date,
            end_date = max_date,
            freq = freq_ref_date
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
            indicator = mean(!is.na(.data[["type_event"]]))
          )
      } else {
        hire_data <- workforce_group_filtered() |>
          govhr::detect_personnel_event(
            event_type = "hire",
            id_col = "personnel_id",
            start_date = min_date,
            end_date = max_date,
            freq = freq_ref_date
          ) |>
          dplyr::left_join(
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
            hires = n()
          )

        fire_data <- workforce_group_filtered() |>
          govhr::detect_personnel_event(
            event_type = "fire",
            id_col = "personnel_id",
            start_date = min_date,
            end_date = max_date,
            freq = freq_ref_date
          ) |>
          dplyr::left_join(
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
            fires = n()
          )

        movement_data <- hire_data |>
          left_join(
            fire_data,
            by = unique(c("ref_date", input$workforce_group))
          ) |>
          mutate(
            indicator = .data[["hires"]] / .data[["fires"]]
          )
      }

      plot_movement <- movement_data |>
        ggplot(
          aes(.data[["ref_date"]], .data[["indicator"]])
        ) +
        geom_point() +
        geom_line() +
        labs(
          x = "Time",
          y = "Share"
        )

      if (input$workforce_group != "ref_date") {
        n_groups <- dplyr::n_distinct(
          movement_data[[input$workforce_group]],
          na.rm = TRUE
        )
        orange_palette <- colorRampPalette(c("#C34729", "#F5C6A0"))(n_groups)
        plot_movement <- plot_movement +
          aes(
            color = .data[[input$workforce_group]],
            group = .data[[input$workforce_group]]
          ) +
          ggplot2::scale_color_manual(values = orange_palette)
      }

      if (input$movement_type %in% c("hire", "fire")) {
        plot_movement <- plot_movement +
          scale_y_continuous(
            labels = scales::percent_format()
          )
      } else if (input$movement_type == "replacement_rate") {
        plot_movement <- plot_movement +
          scale_y_continuous(
            labels = scales::label_number(accuracy = 0.1)
          ) +
          geom_hline(
            yintercept = 1,
            linetype = "dashed",
            color = "#004181"
          ) +
          ggplot2::annotate(
            "text",
            x = as.Date(max_date) -
              (as.Date(max_date) - as.Date(min_date)) * 0.05,
            y = 1.15,
            label = "Replacement rate = 1",
            color = "#004181"
          ) +
          labs(
            y = "Replacement rate"
          )
      }

      plotly::ggplotly(plot_movement)
    }) |>
      bindEvent(input$apply_btn, ignoreNULL = FALSE)

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
            workforce_filtered_data = workforce_filtered(),
            date_range = input$date_range,
            workforce_group = input$workforce_group,
            toggle_growth = input$toggle_growth,
            movement_type = input$movement_type
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
