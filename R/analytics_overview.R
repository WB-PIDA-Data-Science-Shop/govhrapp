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
#' @importFrom dplyr filter mutate arrange group_by ungroup summarise bind_rows n_distinct first
#' @export
overview_ui <- function(id, workforce_data, wagebill_data) {
  ns <- shiny::NS(id)

  bslib::layout_columns(
    fillable = FALSE,
    col_widths = 12,

    # value boxes
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::value_box(
        title = shiny::uiOutput(ns("vb_date_label")),
        value = shiny::uiOutput(ns("vb_headcount")),
        showcase = bsicons::bs_icon("people-fill"),
        theme = bslib::value_box_theme(bg = "#C34729", fg = "#FFFFFF"),
        max_height = "150px"
      ),
      bslib::value_box(
        title = shiny::uiOutput(ns("vb_wagebill_label")),
        value = shiny::uiOutput(ns("vb_wagebill")),
        showcase = bsicons::bs_icon("cash-stack"),
        theme = bslib::value_box_theme(bg = "#004181", fg = "#FFFFFF"),
        max_height = "150px"
      )
    ),
    
    # plot
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
#' @param cache List of cached data frames for improved performance.
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
overview_server <- function(id, cache) {
  shiny::moduleServer(id, function(input, output, session) {
    workforce_overview <- cache[["workforce_trend"]]
    wagebill_overview <- cache[["wagebill_trend"]]

    latest_ref_date <- max(workforce_overview[["ref_date"]])

    # value boxes
    output$vb_date_label <- shiny::renderUI({
      shiny::tags$span(
        paste0("Headcount (", format(latest_ref_date, "%b %Y"), ")")
      )
    })

    output$vb_wagebill_label <- shiny::renderUI({
      shiny::tags$span(
        paste0("Wage Bill (", format(latest_ref_date, "%b %Y"), ")")
      )
    })

    output$vb_headcount <- shiny::renderUI({
      n <- workforce_overview |>
        dplyr::filter(.data[["ref_date"]] == latest_ref_date) |>
        dplyr::pull(.data[["value"]])
      
      shiny::tags$span(
        scales::label_number(scale_cut = scales::cut_short_scale())(n)
      )
    })

    output$vb_wagebill <- shiny::renderUI({
      wagebill_value <- wagebill_overview |>
        dplyr::filter(.data[["ref_date"]] == latest_ref_date) |>
        dplyr::pull(.data[["value"]])
      
      shiny::tags$span(
        scales::label_number(
          scale_cut = scales::cut_short_scale()
        )(wagebill_value),
        bslib::popover(
          bsicons::bs_icon("info-circle-fill", style = "font-size: 0.75em; margin-left: 4px;"),
          "Gross salary in local currency units (LCU).",
          placement = "left"
        )
      )
    })

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
            bslib::popover(
              bsicons::bs_icon("info-circle-fill"),
              "Both series are indexed to 100 for the earliest reference date.",
              placement = "left"
            ),
            class = "d-flex justify-content-between"
          ),
          bslib::card_body(
            plotly::plotlyOutput(session$ns("plot_integrated"), height = "420px")
          )
        )
      )
    })

    # plot option 1. headcount
    output$plot_headcount <- plotly::renderPlotly({
      plot <- cache[["workforce_trend"]] |>
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
      plot <- cache[["wagebill_trend"]] |>
        ggplot2::ggplot(ggplot2::aes(x = .data[["ref_date"]], y = .data[["value"]])) +
        ggplot2::geom_point(colour = "#004181") +
        ggplot2::geom_line(colour  = "#004181") +
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
        index_to_100(cache[["workforce_trend"]], "Headcount"),
        index_to_100(cache[["wagebill_trend"]],  "Total compensation")
      )

      palette <- c("Headcount" = "#C34729", "Total compensation" = "#004181")

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
