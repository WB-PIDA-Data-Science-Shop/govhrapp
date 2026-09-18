#' Overview UI
#'
#' Summary dashboard tab combining headline wage bill and headcount indicators.
#'
#' @param id Character. Module namespace ID.
#'
#' @return A Shiny UI definition.
#'
#' @importFrom bsicons bs_icon
#' @importFrom bslib card card_body card_header layout_columns layout_sidebar popover sidebar value_box value_box_theme
#' @importFrom plotly plotlyOutput
#' @importFrom shiny NS conditionalPanel radioButtons tagList textOutput
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
            bsicons::bs_icon(
              "info-circle-fill",
              style = "font-size: 0.75em; margin-left: 4px;"
            ),
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
            "Workforce" = "headcount",
            "Wage Bill" = "wagebill",
            "Integrated" = "integrated"
          ),
          selected = "headcount"
        )
      ),

      # plot 1. headcount
      shiny::conditionalPanel(
        condition = sprintf("input['%s'] == 'headcount'", ns("display_mode")),
        bslib::card(
          full_screen = TRUE,
          bslib::card_header("Headcount"),
          bslib::card_body(
            plotly::plotlyOutput(ns("plot_headcount"), height = "450px")
          )
        )
      ),

      # plot 2. wage bill
      shiny::conditionalPanel(
        condition = sprintf("input['%s'] == 'wagebill'", ns("display_mode")),
        bslib::card(
          full_screen = TRUE,
          bslib::card_header("Wage Bill"),
          bslib::card_body(
            plotly::plotlyOutput(ns("plot_wagebill"), height = "450px")
          )
        )
      ),

      # plot 3. integrated
      shiny::conditionalPanel(
        condition = sprintf("input['%s'] == 'integrated'", ns("display_mode")),
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
            plotly::plotlyOutput(ns("plot_integrated"), height = "450px")
          )
        )
      )
    )
  )
}

#' Overview Server
#'
#' Renders the headline headcount and wage bill indicators and the three
#' summary charts, all read from the analytics cache.
#'
#' @param id Character. Module namespace ID.
#' @param cache List of pre-computed summaries from [build_analytics_cache()].
#'
#' @return A Shiny module server function.
#'
#' @importFrom dplyr arrange bind_rows filter first mutate pull
#' @importFrom ggplot2 aes geom_hline geom_line geom_point ggplot labs scale_color_manual scale_y_continuous
#' @importFrom plotly ggplotly renderPlotly
#' @importFrom purrr pluck
#' @importFrom scales cut_short_scale label_number
#' @importFrom shiny moduleServer renderText req
#' @export
overview_server <- function(id, cache) {
  shiny::moduleServer(id, function(input, output, session) {
    workforce_overview <- cache |>
      purrr::pluck("workforce", "workforce_overview")

    wagebill_overview <- cache |>
      purrr::pluck("wagebill", "wagebill_overview")

    latest_ref_date <- max(workforce_overview[["ref_date"]], na.rm = TRUE)
    date_label <- format(as.Date(latest_ref_date), "%b %Y")

    headcount_val <- workforce_overview |>
      dplyr::filter(.data[["ref_date"]] == latest_ref_date) |>
      dplyr::pull(.data[["value"]]) |>
      scales::comma(accuracy = 1)

    wagebill_val <- wagebill_overview |>
      dplyr::filter(.data[["ref_date"]] == latest_ref_date) |>
      dplyr::pull(.data[["value"]]) |>
      scales::comma(accuracy = 1)

    output$vb_date_label <- shiny::renderText(paste0(
      "Headcount (",
      date_label,
      ")"
    ))
    output$vb_wagebill_label <- shiny::renderText(paste0(
      "Wage Bill (",
      date_label,
      ")"
    ))
    output$vb_headcount <- shiny::renderText(headcount_val)
    output$vb_wagebill <- shiny::renderText(wagebill_val)

    output$plot_headcount <- plotly::renderPlotly({
      req(input$display_mode == "headcount")

      plotly::ggplotly(
        workforce_overview |>
          ggplot2::ggplot(ggplot2::aes(
            x = .data[["ref_date"]],
            y = .data[["value"]]
          )) +
          ggplot2::geom_point() +
          ggplot2::geom_line() +
          ggplot2::scale_y_continuous(
            labels = scales::label_number(scale_cut = scales::cut_short_scale())
          ) +
          ggplot2::labs(x = "Time", y = "Headcount")
      )
    })

    output$plot_wagebill <- plotly::renderPlotly({
      req(input$display_mode == "wagebill")

      plotly::ggplotly(
        wagebill_overview |>
          filter(
            .data[["indicator"]] == "gross_salary_lcu_sum"
          ) |>
          ggplot2::ggplot(ggplot2::aes(
            x = .data[["ref_date"]],
            y = .data[["value"]]
          )) +
          ggplot2::geom_point(colour = "#004181") +
          ggplot2::geom_line(colour = "#004181") +
          ggplot2::scale_y_continuous(
            labels = scales::label_number(scale_cut = scales::cut_short_scale())
          ) +
          ggplot2::labs(x = "Time", y = "Wage Bill (LCU)")
      )
    })

    output$plot_integrated <- plotly::renderPlotly({
      req(input$display_mode == "integrated")

      palette <- c("Headcount" = "#C34729", "Wage Bill" = "#00070e", "Average Wage" = "#004181")

      # index series for the integrated plot only — kept separate, see note below
      indexed_workforce <- workforce_overview |>
        dplyr::arrange(.data[["ref_date"]]) |>
        dplyr::mutate(
          value = .data[["value"]] / dplyr::first(.data[["value"]]) * 100
        )

      indexed_wagebill <- wagebill_overview |>
        dplyr::filter(
          .data[["indicator"]] == "gross_salary_lcu_sum"
        ) |>
        dplyr::arrange(.data[["ref_date"]]) |>
        dplyr::mutate(
          value = .data[["value"]] / dplyr::first(.data[["value"]]) * 100
        )

      indexed_average_wage <- wagebill_overview |>
        filter(
          .data[["indicator"]] == "gross_salary_lcu_mean"
        ) |>
        dplyr::arrange(.data[["ref_date"]]) |>
        dplyr::mutate(
          value = .data[["value"]] / dplyr::first(.data[["value"]]) * 100
        )

      combined <- dplyr::bind_rows(
        dplyr::mutate(indexed_workforce, series = "Headcount"),
        dplyr::mutate(indexed_wagebill, series = "Wage Bill"),
        dplyr::mutate(indexed_average_wage, series = "Average Wage")
      )

      plotly::ggplotly(
        combined |>
          ggplot2::ggplot(
            ggplot2::aes(
              x = .data[["ref_date"]],
              y = .data[["value"]],
              color = .data[["series"]],
              group = .data[["series"]]
            )
          ) +
          ggplot2::geom_point() +
          ggplot2::geom_line() +
          ggplot2::geom_hline(
            yintercept = 100,
            linetype = "dashed",
            colour = "grey50"
          ) +
          ggplot2::scale_color_manual(values = palette) +
          ggplot2::scale_y_continuous(
            labels = scales::label_number(suffix = "")
          ) +
          ggplot2::labs(
            x = "Time",
            y = "Baseline index (earliest period = 100)",
            color = NULL
          )
      )
    })
  })
}
