#' Overview UI Module
#'
#' Summary dashboard tab combining headline wage bill and headcount indicators.
#'
#' @param id Module id.
#' @param workforce_data Data frame with workforce/personnel data (headcount).
#' @param wagebill_data Data frame with contract/salary data (wage bill).
#'
#' @importFrom bslib layout_columns layout_sidebar sidebar card card_header card_body value_box value_box_theme tooltip
#' @importFrom bsicons bs_icon
#' @importFrom shiny NS markdown icon uiOutput radioButtons
#' @importFrom plotly plotlyOutput
#' @importFrom lubridate year
#' @import dplyr
#' @export
overview_ui <- function(id, workforce_data, wagebill_data) {
  ns <- shiny::NS(id)

  bslib::layout_columns(
    fillable = FALSE,
    col_widths = 12,

    # ── Value boxes ──────────────────────────────────────────────────────────
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::value_box(
        title = shiny::uiOutput(ns("vb_date_label")),
        value = shiny::uiOutput(ns("vb_headcount")),
        showcase = bsicons::bs_icon("people-fill"),
        theme = bslib::value_box_theme(bg = "#29A3C3", fg = "#FFFFFF")
      ),
      bslib::value_box(
        title = shiny::uiOutput(ns("vb_wagebill_label")),
        value = shiny::uiOutput(ns("vb_wagebill")),
        showcase = bsicons::bs_icon("cash-stack"),
        theme = bslib::value_box_theme(bg = "#C34729", fg = "#FFFFFF")
      )
    ),

    # ── Trend charts ──────────────────────────────────────────────────────────
    bslib::layout_sidebar(
      fillable = FALSE,
      sidebar = bslib::sidebar(
        title = "Controls",
        position = "left",
        width = "200px",
        shiny::radioButtons(
          ns("display_mode"),
          label = NULL,
          choices = list(
            "Workforce"  = "headcount",
            "Wage Bill"  = "wagebill",
            "Integrated" = "integrated"
          ),
          selected = "headcount"
        )
      ),
      shiny::uiOutput(ns("chart_area"))
    )
  )
}

#' Overview Server Module
#'
#' @param id Module id.
#' @param id Module id.
#' @param workforce_data Data frame with workforce/personnel data (headcount).
#' @param wagebill_data Data frame with contract/salary data (wage bill).
#'
#' @importFrom shiny moduleServer reactive renderUI renderText tags radioButtons
#' @importFrom plotly renderPlotly ggplotly
#' @importFrom dplyr filter mutate arrange group_by ungroup summarise bind_rows n_distinct first
#' @importFrom lubridate year
#' @importFrom ggplot2 ggplot aes geom_point geom_line scale_y_continuous scale_color_manual geom_hline labs xlab ylab
#' @importFrom grDevices colorRampPalette
#' @importFrom scales label_number cut_short_scale
#' @importFrom govhr fastcount compute_fastsummary
#' @export
overview_server <- function(id, workforce_data, wagebill_data) {
  shiny::moduleServer(id, function(input, output, session) {

    # ── Latest reference dates (one per dataset) ──────────────────────────────

    latest_workforce_date <- reactive({
      max(workforce_data[["ref_date"]], na.rm = TRUE)
    })

    latest_wagebill_date <- reactive({
      max(wagebill_data[["ref_date"]], na.rm = TRUE)
    })

    latest_workforce <- reactive({
      workforce_data |>
        dplyr::filter(.data[["ref_date"]] == latest_workforce_date())
    })

    latest_wagebill <- reactive({
      wagebill_data |>
        dplyr::filter(.data[["ref_date"]] == latest_wagebill_date())
    })

    # Detect which salary columns are present in wagebill_data
    salary_cols <- intersect(
      c("base_salary_lcu", "gross_salary_lcu", "net_salary_lcu", "allowance_lcu"),
      names(wagebill_data)
    )

    # value boxes
    output$vb_date_label <- shiny::renderUI({
      shiny::tags$span(
        paste0("Headcount (", format(latest_workforce_date(), "%b %Y"), ")")
      )
    })

    output$vb_wagebill_label <- shiny::renderUI({
      shiny::tags$span(
        paste0("Wage Bill (", format(latest_wagebill_date(), "%b %Y"), ")")
      )
    })

    output$vb_headcount <- shiny::renderUI({
      n <- nrow(latest_workforce())
      shiny::tags$span(
        scales::label_number(scale_cut = scales::cut_short_scale())(n)
      )
    })

    output$vb_wagebill <- shiny::renderUI({
      if (length(salary_cols) == 0) {
        return(shiny::tags$span("N/A"))
      }
      d <- latest_wagebill()
      base  <- if ("base_salary_lcu" %in% names(d)) d[["base_salary_lcu"]] else 0
      allow <- if ("allowance_lcu"   %in% names(d)) d[["allowance_lcu"]]   else 0
      total <- sum(base + allow, na.rm = TRUE)
      shiny::tags$span(
        scales::label_number(
          scale_cut = scales::cut_short_scale()
        )(total),
        bslib::tooltip(
          bsicons::bs_icon("info-circle", style = "font-size: 0.75em; margin-left: 4px;"),
          "Sum of base salary and allowances in local currency units (LCU)."
        )
      )
    })

    # headcount panel

    heacount_panel <- reactive({
      workforce_data |>
        govhr::fastcount(.data[["ref_date"]], name = "value")
    })

    # ── Wage bill time series ──────────────────────────────────────────────────
    # Total compensation = base_salary_lcu + allowance_lcu
    # Complementary colour of #C34729 (hue ~15°) → hue ~195° = #29A3C3

    wagebill_panel <- reactive({
      has_base  <- "base_salary_lcu" %in% names(wagebill_data)
      has_allow <- "allowance_lcu"   %in% names(wagebill_data)

      wagebill_data |>
        dplyr::mutate(
          total_compensation = dplyr::case_when(
            has_base  & has_allow ~ .data[["base_salary_lcu"]] + .data[["allowance_lcu"]],
            has_base              ~ .data[["base_salary_lcu"]],
            has_allow             ~ .data[["allowance_lcu"]],
            TRUE                  ~ NA_real_
          )
        ) |>
        dplyr::group_by(.data[["ref_date"]]) |>
        dplyr::summarise(
          value = sum(.data[["total_compensation"]], na.rm = TRUE),
          .groups = "drop"
        )
    })

    # ── Chart area (switches on display_mode) ────────────────────────────────

    output$chart_area <- shiny::renderUI({
      switch(
        input$display_mode,
        headcount = bslib::card(
          full_screen = TRUE,
          bslib::card_header("Headcount"),
          bslib::card_body(
            plotly::plotlyOutput(session$ns("plot_headcount"), height = "420px")
          )
        ),
        wagebill = bslib::card(
          full_screen = TRUE,
          bslib::card_header("Wage Bill"),
          bslib::card_body(
            plotly::plotlyOutput(session$ns("plot_wagebill"), height = "420px")
          )
        ),
        integrated = bslib::card(
          full_screen = TRUE,
          bslib::card_header(
            "Integrated: Headcount and Wage Bill",
            bslib::tooltip(
              bsicons::bs_icon("info-circle"),
              "Both series are indexed to 100 for the earliest reference date."
            )
          ),
          bslib::card_body(
            plotly::plotlyOutput(session$ns("plot_integrated"), height = "420px")
          )
        )
      )
    })

    # plot option 1. headcount

    output$plot_headcount <- plotly::renderPlotly({
      plot <- heacount_panel() |>
        ggplot2::ggplot(ggplot2::aes(x = .data[["ref_date"]], y = .data[["value"]])) +
        ggplot2::geom_point() +
        ggplot2::geom_line() +
        ggplot2::scale_y_continuous(
          labels = scales::label_number(scale_cut = scales::cut_short_scale())
        ) +
        ggplot2::labs(x = "Time", y = "Headcount")

      plotly::ggplotly(plot)
    })

    # plot option 2. total wage bill
    output$plot_wagebill <- plotly::renderPlotly({
      plot <- wagebill_panel() |>
        ggplot2::ggplot(ggplot2::aes(x = .data[["ref_date"]], y = .data[["value"]])) +
        ggplot2::geom_point(colour = "#29A3C3") +
        ggplot2::geom_line(colour  = "#29A3C3") +
        ggplot2::scale_y_continuous(
          labels = scales::label_number(scale_cut = scales::cut_short_scale())
        ) +
        ggplot2::labs(x = "Time", y = "Total compensation (LCU)")

      plotly::ggplotly(plot)
    })

    # plot option 3. integrated
    output$plot_integrated <- plotly::renderPlotly({
      index_to_100 <- function(df, label) {
        df |>
          dplyr::arrange(.data[["ref_date"]]) |>
          dplyr::mutate(
            value  = .data[["value"]] / dplyr::first(.data[["value"]]) * 100,
            series = label
          )
      }

      combined <- dplyr::bind_rows(
        index_to_100(heacount_panel(), "Headcount"),
        index_to_100(wagebill_panel(),  "Total compensation")
      )

      palette <- c("Headcount" = "#C34729", "Total compensation" = "#29A3C3")

      plot <- combined |>
        ggplot2::ggplot(
          ggplot2::aes(
            x     = .data[["ref_date"]],
            y     = .data[["value"]],
            color = .data[["series"]],
            group = .data[["series"]]
          )
        ) +
        ggplot2::geom_point() +
        ggplot2::geom_line() +
        ggplot2::geom_hline(
          yintercept = 100,
          linetype   = "dashed",
          colour     = "grey50"
        ) +
        ggplot2::scale_color_manual(values = palette) +
        ggplot2::scale_y_continuous(
          labels = scales::label_number(suffix = "")
        ) +
        ggplot2::labs(
          x     = "Time",
          y     = "Baseline index (earliest period = 100)",
          color = NULL
        )

      plotly::ggplotly(plot)
    })
  })
}
