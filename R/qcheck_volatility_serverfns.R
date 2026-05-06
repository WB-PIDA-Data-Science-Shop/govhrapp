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
#' @importFrom shiny moduleServer observe observeEvent updateSelectInput req
#' @importFrom data.table copy
#' @importFrom stats setNames
#' @importFrom highcharter renderHighchart highchart hc_chart hc_xAxis hc_yAxis hc_add_series hc_title hc_tooltip hc_colorAxis hc_legend
#'
#' @keywords internal
volatility_server <- function(id, qc_obj) {

  shiny::moduleServer(id, function(input, output, session) {

    vol_contract  <- qc_obj$volatility$contract
    vol_personnel <- qc_obj$volatility$personnel

    # Human-readable labels for stat_type values
    stat_type_labels <- c(
      salary_vol         = "Salary Volatility",
      ctrcount_vol       = "Contract Count Volatility",
      workhours_vol      = "Work Hours Volatility",
      occ_diversity_vol  = "Occupation Diversity Volatility",
      ctype_diversity_vol = "Contract Type Diversity Volatility"
    )

    # -------------------------------------------------------------------------
    # CONTRACT: populate stat_type choices on load
    # -------------------------------------------------------------------------
    shiny::observe({
      shiny::req(!is.null(vol_contract) && nrow(vol_contract) > 0)
      types   <- unique(vol_contract$stat_type)
      labels  <- ifelse(types %in% names(stat_type_labels), unname(stat_type_labels[types]), types)
      choices <- stats::setNames(types, labels)
      shiny::updateSelectInput(session, "contract_stat_type", choices = choices)
    })

    # CONTRACT: cascade group_var from stat_type
    shiny::observeEvent(input$contract_stat_type, {
      shiny::req(input$contract_stat_type)
      sub_dt  <- vol_contract[stat_type == input$contract_stat_type]
      grp_dt  <- unique(sub_dt[, .(group_var, group_var_label)])
      labels  <- ifelse(!is.na(grp_dt$group_var_label), grp_dt$group_var_label, grp_dt$group_var)
      choices <- stats::setNames(grp_dt$group_var, labels)
      shiny::updateSelectInput(session, "contract_group_var", choices = choices)
    })

    # CONTRACT: cascade indicator from stat_type (only relevant for salary_vol)
    shiny::observeEvent(input$contract_stat_type, {
      shiny::req(input$contract_stat_type)
      sub_dt  <- vol_contract[stat_type == input$contract_stat_type]
      ind_dt  <- unique(sub_dt[, .(indicator, indicator_label)])
      labels  <- ifelse(!is.na(ind_dt$indicator_label), ind_dt$indicator_label, as.character(ind_dt$indicator))
      choices <- stats::setNames(as.character(ind_dt$indicator), labels)
      shiny::updateSelectInput(session, "contract_indicator", choices = choices)
    })

    # -------------------------------------------------------------------------
    # PERSONNEL: populate group_var choices on load
    # -------------------------------------------------------------------------
    shiny::observe({
      shiny::req(!is.null(vol_personnel) && nrow(vol_personnel) > 0)
      grp_dt  <- unique(vol_personnel[, .(group_var, group_var_label)])
      labels  <- ifelse(!is.na(grp_dt$group_var_label), grp_dt$group_var_label, grp_dt$group_var)
      choices <- stats::setNames(grp_dt$group_var, labels)
      shiny::updateSelectInput(session, "personnel_group_var", choices = choices)
    })

    # -------------------------------------------------------------------------
    # Helper: top N entities by max absolute vol_stat
    # -------------------------------------------------------------------------
    prepare_top_volatile <- function(vol_dt, top_n) {
      vol_dt <- vol_dt[!is.na(vol_stat) & !is.nan(vol_stat) & is.finite(vol_stat)]
      if (nrow(vol_dt) == 0) return(NULL)
      vol_dt[, max_vol := max(abs(vol_stat), na.rm = TRUE), by = group_val]
      top_ids <- vol_dt[order(-max_vol)][!duplicated(group_val)][seq_len(min(top_n, .N))]$group_val
      vol_dt[group_val %in% top_ids]
    }

    # -------------------------------------------------------------------------
    # Helper: build heatmap
    # -------------------------------------------------------------------------
    build_heatmap <- function(plot_dt, title_text) {
      group_vals  <- unique(plot_dt$group_val)
      ref_dates   <- sort(unique(plot_dt$ref_date))
      group_label <- plot_dt$group_var_label[1]

      heatmap_data <- plot_dt[, .(
        x     = match(as.character(ref_date), as.character(ref_dates)) - 1L,
        y     = match(group_val, group_vals) - 1L,
        value = round(vol_stat * 100, 2)
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
            group_label, ": <b>{point.yCategory}</b><br>",
            "Date: <b>{point.xCategory}</b><br>",
            "% Change: <b>{point.value:.1f}%</b>"
          )
        ) |>
        highcharter::hc_legend(enabled = TRUE) |>
        add_export_menu()
    }

    # -------------------------------------------------------------------------
    # CONTRACT heatmap
    # -------------------------------------------------------------------------
    output$contract_heatmap <- highcharter::renderHighchart({
      shiny::req(input$contract_stat_type, input$contract_group_var, input$contract_top_n)

      plot_dt <- data.table::copy(vol_contract)

      # Filter to selected stat_type and group_var
      plot_dt <- plot_dt[
        stat_type == input$contract_stat_type &
        group_var == input$contract_group_var
      ]

      # For salary_vol (multiple indicators), further filter to selected indicator
      if (input$contract_stat_type == "salary_vol") {
        shiny::req(input$contract_indicator)
        plot_dt <- plot_dt[as.character(indicator) == input$contract_indicator]
      }

      plot_dt <- prepare_top_volatile(plot_dt, top_n = input$contract_top_n)
      if (is.null(plot_dt) || nrow(plot_dt) == 0) return(NULL)

      stat_label <- stat_type_labels[input$contract_stat_type]
      grp_label  <- plot_dt$group_var_label[1]
      ind_label  <- if (input$contract_stat_type == "salary_vol") {
        paste0(" -- ", plot_dt$indicator_label[1])
      } else ""

      build_heatmap(
        plot_dt,
        paste0("Top ", input$contract_top_n, " ", grp_label, "s by ",
               stat_label, ind_label)
      )
    })

    # -------------------------------------------------------------------------
    # PERSONNEL heatmap
    # -------------------------------------------------------------------------
    output$personnel_heatmap <- highcharter::renderHighchart({
      shiny::req(input$personnel_group_var, input$personnel_top_n)

      plot_dt <- data.table::copy(vol_personnel)
      plot_dt <- plot_dt[group_var == input$personnel_group_var]

      plot_dt <- prepare_top_volatile(plot_dt, top_n = input$personnel_top_n)
      if (is.null(plot_dt) || nrow(plot_dt) == 0) return(NULL)

      grp_label <- plot_dt$group_var_label[1]

      build_heatmap(
        plot_dt,
        paste0("Top ", input$personnel_top_n, " ", grp_label, "s by Headcount Volatility")
      )
    })

  })
}
