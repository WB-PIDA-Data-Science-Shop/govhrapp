#############################################################################################
############################# VOLATILITY MODULE SERVER #######################################
#############################################################################################

#' Volatility Server Module
#'
#' Server logic for the Volatility Analysis section, processing and visualizing
#' temporal volatility in salary, contract counts, and work hours.
#'
#' @param id Character string. The module namespace ID.
#' @param dynamicqc_obj A dynamicqc object created by \code{compute_dynamicqc()}.
#'
#' @return None. Called for side effects (renders Shiny outputs).
#'
#' @importFrom shiny moduleServer updateSelectInput observeEvent
#' @importFrom data.table data.table copy setorder
#' @importFrom highcharter renderHighchart highchart hc_chart hc_xAxis hc_yAxis hc_add_series hc_title hc_subtitle hc_tooltip hc_legend hc_colorAxis
#'
#' @keywords internal
volatility_server <- function(id, dynamicqc_obj) {
  
  shiny::moduleServer(id, function(input, output, session) {
    
    # Extract volatility data
    vol_obj <- dynamicqc_obj$qc_obj$volatility
    
    # Update salary indicator choices dynamically
    shiny::observe({
      if (!is.null(vol_obj$salary_vol)) {
        indicators <- unique(vol_obj$salary_vol$indicator)
        indicators <- indicators[!is.na(indicators)]
        
        shiny::updateSelectInput(
          session,
          "salary_indicator",
          choices = as.character(indicators),
          selected = if (length(indicators) > 0) as.character(indicators[1]) else NULL
        )
      }
    })
    
    # Helper function to prepare top volatile entities
    prepare_top_volatile <- function(vol_dt, vol_col, id_col, top_n, indicator_filter = NULL) {
      
      if (nrow(vol_dt) == 0) return(NULL)
      
      # Filter by indicator if specified
      if (!is.null(indicator_filter)) {
        vol_dt <- vol_dt[indicator == indicator_filter]
      }
      
      # Remove rows where volatility column is NA
      vol_dt <- vol_dt[!is.na(get(vol_col))]
      
      if (nrow(vol_dt) == 0) return(NULL)
      
      # Calculate max volatility per entity
      agg_col <- paste0("max_", vol_col)
      vol_dt[, (agg_col) := max(get(vol_col), na.rm = TRUE), by = id_col]
      
      # Get top N entities
      top_ids <- vol_dt[
        order(-get(agg_col))
      ][
        !duplicated(get(id_col))
      ][
        1:min(top_n, .N)
      ][[id_col]]
      
      # Filter to top entities
      result <- vol_dt[get(id_col) %in% top_ids]
      
      return(result)
    }
    
    # Salary Volatility Heatmap
    output$salary_heatmap <- highcharter::renderHighchart({
      
      req(input$salary_indicator, input$salary_top_n)
      
      salary_vol <- data.table::copy(vol_obj$salary_vol)
      
      if (nrow(salary_vol) == 0) {
        return(NULL)
      }
      
      # Prepare top contracts
      plot_dt <- prepare_top_volatile(
        vol_dt = salary_vol,
        vol_col = "rolling_cv",
        id_col = "contract_id",
        top_n = input$salary_top_n,
        indicator_filter = input$salary_indicator
      )
      
      if (is.null(plot_dt) || nrow(plot_dt) == 0) {
        return(NULL)
      }
      
      # Remove NA values
      plot_dt <- plot_dt[!is.na(rolling_cv)]
      
      if (nrow(plot_dt) == 0) {
        return(NULL)
      }
      
      # Create categories
      contract_ids <- unique(plot_dt$contract_id)
      ref_dates <- unique(plot_dt$ref_date)
      
      # Prepare data for heatmap
      heatmap_data <- plot_dt[, .(
        x = match(as.character(ref_date), as.character(sort(ref_dates))) - 1,
        y = match(contract_id, contract_ids) - 1,
        value = rolling_cv
      )]
      
      highcharter::highchart() |>
        highcharter::hc_chart(type = "heatmap") |>
        highcharter::hc_xAxis(
          categories = as.character(sort(ref_dates)),
          title = list(text = "Reference Date")
        ) |>
        highcharter::hc_yAxis(
          categories = contract_ids,
          title = list(text = "Contract ID")
        ) |>
        highcharter::hc_add_series(
          data = heatmap_data,
          type = "heatmap",
          name = "Rolling CV",
          dataLabels = list(enabled = FALSE)
        ) |>
        highcharter::hc_colorAxis(
          min = 0,
          stops = list(
            list(0, "#440154"),
            list(0.25, "#31688e"),
            list(0.5, "#35b779"),
            list(0.75, "#fde724"),
            list(1, "#fde724")
          )
        ) |>
        highcharter::hc_title(
          text = paste0("Top ", input$salary_top_n, " Contracts by Salary Volatility: ", input$salary_indicator)
        ) |>
        highcharter::hc_tooltip(
          pointFormat = "Contract: <b>{point.y}</b><br>Date: <b>{point.x}</b><br>CV: <b>{point.value:.3f}</b>"
        ) |>
        highcharter::hc_legend(enabled = TRUE)
    })
    
    # Contract Count Volatility Heatmap
    output$contract_count_heatmap <- highcharter::renderHighchart({
      
      req(input$contract_count_top_n)
      
      ctrcount_vol <- data.table::copy(vol_obj$ctrcount_vol)
      
      if (nrow(ctrcount_vol) == 0) {
        return(NULL)
      }
      
      # Prepare top establishments
      plot_dt <- prepare_top_volatile(
        vol_dt = ctrcount_vol,
        vol_col = "pct_change",
        id_col = "est_id",
        top_n = input$contract_count_top_n
      )
      
      if (is.null(plot_dt) || nrow(plot_dt) == 0) {
        return(NULL)
      }
      
      # Remove NA values
      plot_dt <- plot_dt[!is.na(pct_change)]
      
      if (nrow(plot_dt) == 0) {
        return(NULL)
      }
      
      # Create categories
      est_ids <- unique(plot_dt$est_id)
      ref_dates <- unique(plot_dt$ref_date)
      
      # Prepare data for heatmap
      heatmap_data <- plot_dt[, .(
        x = match(as.character(ref_date), as.character(sort(ref_dates))) - 1,
        y = match(est_id, est_ids) - 1,
        value = pct_change
      )]
      
      highcharter::highchart() |>
        highcharter::hc_chart(type = "heatmap") |>
        highcharter::hc_xAxis(
          categories = as.character(sort(ref_dates)),
          title = list(text = "Reference Date")
        ) |>
        highcharter::hc_yAxis(
          categories = est_ids,
          title = list(text = "Establishment ID")
        ) |>
        highcharter::hc_add_series(
          data = heatmap_data,
          type = "heatmap",
          name = "% Change",
          dataLabels = list(enabled = FALSE)
        ) |>
        highcharter::hc_colorAxis(
          stops = list(
            list(0, "#440154"),
            list(0.25, "#31688e"),
            list(0.5, "#35b779"),
            list(0.75, "#fde724"),
            list(1, "#fde724")
          )
        ) |>
        highcharter::hc_title(
          text = paste0("Top ", input$contract_count_top_n, " Establishments by Contract Count Volatility")
        ) |>
        highcharter::hc_tooltip(
          pointFormat = "Establishment: <b>{point.y}</b><br>Date: <b>{point.x}</b><br>% Change: <b>{point.value:.1f}%</b>"
        ) |>
        highcharter::hc_legend(enabled = TRUE)
    })
    
    # Work Hours Volatility Heatmap
    output$workhours_heatmap <- highcharter::renderHighchart({
      
      req(input$workhours_top_n)
      
      workhours_vol <- data.table::copy(vol_obj$workhours_vol)
      
      if (nrow(workhours_vol) == 0) {
        return(NULL)
      }
      
      # Prepare top contracts
      plot_dt <- prepare_top_volatile(
        vol_dt = workhours_vol,
        vol_col = "rolling_cv",
        id_col = "contract_id",
        top_n = input$workhours_top_n
      )
      
      if (is.null(plot_dt) || nrow(plot_dt) == 0) {
        return(NULL)
      }
      
      # Remove NA values
      plot_dt <- plot_dt[!is.na(rolling_cv)]
      
      if (nrow(plot_dt) == 0) {
        return(NULL)
      }
      
      # Create categories
      contract_ids <- unique(plot_dt$contract_id)
      ref_dates <- unique(plot_dt$ref_date)
      
      # Prepare data for heatmap
      heatmap_data <- plot_dt[, .(
        x = match(as.character(ref_date), as.character(sort(ref_dates))) - 1,
        y = match(contract_id, contract_ids) - 1,
        value = rolling_cv
      )]
      
      highcharter::highchart() |>
        highcharter::hc_chart(type = "heatmap") |>
        highcharter::hc_xAxis(
          categories = as.character(sort(ref_dates)),
          title = list(text = "Reference Date")
        ) |>
        highcharter::hc_yAxis(
          categories = contract_ids,
          title = list(text = "Contract ID")
        ) |>
        highcharter::hc_add_series(
          data = heatmap_data,
          type = "heatmap",
          name = "Rolling CV",
          dataLabels = list(enabled = FALSE)
        ) |>
        highcharter::hc_colorAxis(
          min = 0,
          stops = list(
            list(0, "#440154"),
            list(0.25, "#31688e"),
            list(0.5, "#35b779"),
            list(0.75, "#fde724"),
            list(1, "#fde724")
          )
        ) |>
        highcharter::hc_title(
          text = paste0("Top ", input$workhours_top_n, " Contracts by Work Hours Volatility")
        ) |>
        highcharter::hc_tooltip(
          pointFormat = "Contract: <b>{point.y}</b><br>Date: <b>{point.x}</b><br>CV: <b>{point.value:.3f}</b>"
        ) |>
        highcharter::hc_legend(enabled = TRUE)
    })
  })
}
