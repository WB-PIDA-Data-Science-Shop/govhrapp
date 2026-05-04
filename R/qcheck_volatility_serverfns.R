#############################################################################################
############################# VOLATILITY MODULE SERVER #######################################
#############################################################################################

#' Volatility Server Module
#'
#' Server logic for the Volatility Analysis section, processing and visualizing
#' temporal volatility in salary, headcount, and other HR metrics.
#'
#' @param id Character string. The module namespace ID.
#' @param qc_obj A quality control object from \code{govhr::compute_qualitycontrol()}.
#'
#' @return None. Called for side effects (renders Shiny outputs).
#'
#' @importFrom shiny moduleServer observe updateSelectInput req
#' @importFrom data.table copy
#' @importFrom stats setNames
#' @importFrom highcharter renderHighchart highchart hc_chart hc_xAxis hc_yAxis hc_add_series hc_title hc_tooltip hc_colorAxis hc_legend
#'
#' @keywords internal
volatility_server <- function(id, qc_obj) {

  shiny::moduleServer(id, function(input, output, session) {

    vol_obj <- qc_obj$volatility

    # Helper: select top N entities by max absolute vol_stat
    prepare_top_volatile <- function(vol_dt, top_n = 50, indicator_filter = NULL) {
      if (is.null(vol_dt) || nrow(vol_dt) == 0) return(NULL)

      if (!is.null(indicator_filter)) {
        vol_dt <- vol_dt[as.character(indicator) == indicator_filter]
      }

      vol_dt <- vol_dt[!is.na(vol_stat) & !is.nan(vol_stat)]
      if (nrow(vol_dt) == 0) return(NULL)

      vol_dt[, max_vol := max(abs(vol_stat), na.rm = TRUE), by = group_val]
      top_ids <- vol_dt[order(-max_vol)][!duplicated(group_val)][seq_len(min(top_n, .N))]$group_val
      vol_dt[group_val %in% top_ids]
    }

    # Populate contract indicator choices
    shiny::observe({
      if (!is.null(vol_obj$contract) && nrow(vol_obj$contract) > 0) {
        ind_dt  <- unique(vol_obj$contract[, .(indicator, indicator_label)])
        choices <- setNames(as.character(ind_dt$indicator), ind_dt$indicator_label)
        shiny::updateSelectInput(session, "contract_indicator", choices = choices)
      }
    })

    # Helper to build a heatmap highchart
    build_heatmap <- function(plot_dt, title_text) {
      group_vals <- unique(plot_dt$group_val)
      ref_dates  <- sort(unique(plot_dt$ref_date))
      group_label <- plot_dt$group_var_label[1]

      heatmap_data <- plot_dt[, .(
        x     = match(as.character(ref_date), as.character(ref_dates)) - 1L,
        y     = match(group_val, group_vals) - 1L,
        value = vol_stat * 100
      )]

      highcharter::highchart() |>
        highcharter::hc_chart(type = "heatmap") |>
        highcharter::hc_xAxis(
          categories = as.character(ref_dates),
          title      = list(text = "Reference Date")
        ) |>
        highcharter::hc_yAxis(
          categories = group_vals,
          title      = list(text = group_label)
        ) |>
        highcharter::hc_add_series(
          data       = heatmap_data,
          type       = "heatmap",
          name       = "% Change",
          dataLabels = list(enabled = FALSE)
        ) |>
        highcharter::hc_colorAxis(
          min   = 0,
          stops = list(
            list(0,    "#000004"),
            list(0.25, "#56106E"),
            list(0.5,  "#BB3754"),
            list(0.75, "#F98C0A"),
            list(1,    "#FCFFA4")
          )
        ) |>
        highcharter::hc_title(text = title_text) |>
        highcharter::hc_tooltip(
          pointFormat = paste0(
            group_label, ": <b>{point.y}</b><br>",
            "Date: <b>{point.x}</b><br>",
            "Change: <b>{point.value:.1f}%</b>"
          )
        ) |>
        highcharter::hc_legend(enabled = TRUE) |>
        add_export_menu()
    }

    # Contract volatility heatmap
    output$contract_heatmap <- highcharter::renderHighchart({
      shiny::req(input$contract_indicator, input$contract_top_n)

      plot_dt <- data.table::copy(vol_obj$contract)
      plot_dt <- prepare_top_volatile(plot_dt,
                                      top_n            = input$contract_top_n,
                                      indicator_filter = input$contract_indicator)

      if (is.null(plot_dt) || nrow(plot_dt) == 0) return(NULL)
      plot_dt <- plot_dt[!is.na(vol_stat) & !is.nan(vol_stat)]
      if (nrow(plot_dt) == 0) return(NULL)

      ind_label <- plot_dt$indicator_label[1]
      build_heatmap(
        plot_dt,
        paste0("Top ", input$contract_top_n, " by Volatility: ", ind_label)
      )
    })

    # Headcount (personnel) volatility heatmap
    output$personnel_heatmap <- highcharter::renderHighchart({
      shiny::req(input$personnel_top_n)

      plot_dt <- data.table::copy(vol_obj$personnel)
      plot_dt <- prepare_top_volatile(plot_dt, top_n = input$personnel_top_n)

      if (is.null(plot_dt) || nrow(plot_dt) == 0) return(NULL)
      plot_dt <- plot_dt[!is.na(vol_stat) & !is.nan(vol_stat)]
      if (nrow(plot_dt) == 0) return(NULL)

      build_heatmap(
        plot_dt,
        paste0("Top ", input$personnel_top_n, " Establishments by Headcount Volatility")
      )
    })

  })
}
