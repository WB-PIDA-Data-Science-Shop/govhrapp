#' Overview UI Module
#'
#' Summary dashboard tab combining headline wage bill and headcount indicators.
#'
#' @param id Module id.
#'
#' @importFrom bslib layout_columns layout_sidebar sidebar card card_header card_body value_box value_box_theme popover
#' @importFrom bsicons bs_icon
#' @importFrom shiny NS textOutput uiOutput radioButtons tagList
#' @importFrom plotly plotlyOutput
#' @export
overview_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_columns(
    fillable = FALSE,
    col_widths = 12,

    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::value_box(
        title = shiny::textOutput(ns("vb_date_label")),
        value = shiny::textOutput(ns("vb_headcount")),
        showcase = bsicons::bs_icon("people-fill"),
        theme = bslib::value_box_theme(bg = "#C34729", fg = "#FFFFFF"),
        max_height = "150px"
      ),
      bslib::value_box(
        title = shiny::textOutput(ns("vb_wagebill_label")),
        value = shiny::tagList(
          shiny::textOutput(ns("vb_wagebill")),
          bslib::popover(
            bsicons::bs_icon("info-circle-fill", style = "font-size: 0.75em; margin-left: 4px;"),
            "Gross salary in local currency units (LCU).",
            placement = "left"
          )
        ),
        showcase = bsicons::bs_icon("cash-stack"),
        theme = bslib::value_box_theme(bg = "#004181", fg = "#FFFFFF"),
        max_height = "150px"
      )
    ),

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

      # navset_hidden binds panel visibility to input$display_mode via
      # each panel's `value`, and Shiny's tab-based suspend/resume is
      # reliable even inside full_screen cards (unlike conditionalPanel).
      bslib::navset_hidden(
        id = ns("display_mode"),

        bslib::nav_panel(
          value = "headcount",
          bslib::card(
            full_screen = TRUE,
            bslib::card_header("Headcount"),
            bslib::card_body(
              plotly::plotlyOutput(ns("plot_headcount"), height = "350px")
            )
          )
        ),

        bslib::nav_panel(
          value = "wagebill",
          bslib::card(
            full_screen = TRUE,
            bslib::card_header("Wage Bill"),
            bslib::card_body(
              plotly::plotlyOutput(ns("plot_wagebill"), height = "350px")
            )
          )
        ),

        bslib::nav_panel(
          value = "integrated",
          bslib::card(
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
              plotly::plotlyOutput(ns("plot_integrated"), height = "350px")
            )
          )
        )
      )
    )
  )
}


#' Overview Server Module
#'
#' @param id Module id.
#' @param cache List of cached data frames for improved performance.
#'
#' @importFrom shiny moduleServer renderUI renderText tags
#' @importFrom plotly renderPlotly ggplotly
#' @importFrom dplyr filter mutate arrange bind_rows first
#' @importFrom ggplot2 ggplot aes geom_point geom_line scale_y_continuous scale_color_manual geom_hline labs
#' @importFrom scales label_number cut_short_scale
#' @export
overview_server <- function(id, cache) {
  shiny::moduleServer(id, function(input, output, session) {

    workforce_overview <- cache[["workforce_trend"]]
    wagebill_overview  <- cache[["wagebill_trend"]]

    latest_ref_date <- max(workforce_overview[["ref_date"]], na.rm = TRUE)
    date_label <- format(as.Date(latest_ref_date), "%b %Y")

    fmt <- scales::label_number(scale_cut = scales::cut_short_scale())

    headcount_val <- fmt(
      workforce_overview |>
        dplyr::filter(.data[["ref_date"]] == latest_ref_date) |>
        dplyr::pull(.data[["value"]])
    )

    wagebill_val <- fmt(
      wagebill_overview |>
        dplyr::filter(.data[["ref_date"]] == latest_ref_date) |>
        dplyr::pull(.data[["value"]])
    )

    output$vb_date_label     <- shiny::renderText(paste0("Headcount (", date_label, ")"))
    output$vb_wagebill_label <- shiny::renderText(paste0("Wage Bill (", date_label, ")"))
    output$vb_headcount      <- shiny::renderText(headcount_val)
    output$vb_wagebill       <- shiny::renderText(wagebill_val)

    axis_fmt <- scales::label_number(scale_cut = scales::cut_short_scale())

    output$plot_headcount <- plotly::renderPlotly({
      plotly::plot_ly(
        data = workforce_overview,
        x = ~ref_date,
        y = ~value,
        type = "scatter",
        mode = "lines+markers",
        line = list(color = "#000000"),
        marker = list(color = "#000000")
      ) |>
        plotly::layout(
          xaxis = list(title = "Time"),
          yaxis = list(title = "Headcount", tickformat = "~s")
        )
    })

    output$plot_wagebill <- plotly::renderPlotly({
      plotly::plot_ly(
        data = wagebill_overview,
        x = ~ref_date,
        y = ~value,
        type = "scatter",
        mode = "lines+markers",
        line = list(color = "#004181"),
        marker = list(color = "#004181")
      ) |>
        plotly::layout(
          xaxis = list(title = "Time"),
          yaxis = list(title = "Total compensation (LCU)", tickformat = "~s")
        )
    })

    # Indexed series are only computed the first time the "integrated"
    # panel is actually selected, not eagerly at module init.
    output$plot_integrated <- plotly::renderPlotly({
      indexed_workforce <- workforce_overview |>
        dplyr::arrange(.data[["ref_date"]]) |>
        dplyr::mutate(
          value  = .data[["value"]] / dplyr::first(.data[["value"]]) * 100,
          series = "Headcount"
        )

      indexed_wagebill <- wagebill_overview |>
        dplyr::arrange(.data[["ref_date"]]) |>
        dplyr::mutate(
          value  = .data[["value"]] / dplyr::first(.data[["value"]]) * 100,
          series = "Total compensation"
        )

      combined <- dplyr::bind_rows(indexed_workforce, indexed_wagebill)
      palette  <- c("Headcount" = "#C34729", "Total compensation" = "#004181")

      plotly::plot_ly(colors = palette) |>
        plotly::add_trace(
          data = combined,
          x = ~ref_date,
          y = ~value,
          color = ~series,
          type = "scatter",
          mode = "lines+markers"
        ) |>
        plotly::layout(
          xaxis = list(title = "Time"),
          yaxis = list(title = "Baseline index (earliest period = 100)"),
          shapes = list(
            list(
              type = "line",
              x0 = 0, x1 = 1, xref = "paper",
              y0 = 100, y1 = 100,
              line = list(color = "grey50", dash = "dash")
            )
          ),
          legend = list(title = list(text = ""))
        )
    }) |>
      shiny::bindEvent(input$display_mode, ignoreInit = FALSE)
  })
}
