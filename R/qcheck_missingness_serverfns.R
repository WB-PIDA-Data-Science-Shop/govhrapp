#' Missingness Server Module
#'
#' Server logic for the Missingness section, processing and visualizing missing data patterns.
#'
#' @param id Character string. The module namespace ID.
#' @param dynamicqc_obj A dynamicqc object created by \code{compute_dynamicqc()}.
#'
#' @return None. Called for side effects (renders Shiny outputs).
#'
#' @importFrom shiny moduleServer
#' @importFrom data.table data.table copy setorder
#' @importFrom highcharter renderHighchart highchart hc_chart hc_xAxis hc_yAxis hc_add_series hc_title hc_subtitle hc_tooltip hc_legend hcaes hc_colorAxis
#' @importFrom scales percent_format
#'
#' @keywords internal
missingness_server <- function(id, dynamicqc_obj) {
  
  shiny::moduleServer(id, function(input, output, session) {
    
    # Extract missingness data
    miss_obj <- dynamicqc_obj$qc_obj$missingness
    
    # Overall Missingness Plot
    output$overall_plot <- highcharter::renderHighchart({
      overall_dt <- data.table::copy(miss_obj$overall)
      
      # Filter to only show variables with missing data
      plot_dt <- overall_dt[pct_missing > 0]
      
      if (nrow(plot_dt) == 0) {
        return(NULL)
      }
      
      # Sort by missingness
      data.table::setorder(plot_dt, -pct_missing)
      
      highcharter::highchart() |>
        highcharter::hc_chart(type = "bar") |>
        highcharter::hc_xAxis(
          categories = plot_dt$variable,
          title = list(text = "Variable")
        ) |>
        highcharter::hc_yAxis(
          title = list(text = "Percent Missing"),
          labels = list(format = "{value}%"),
          max = 100
        ) |>
        highcharter::hc_add_series(
          data = plot_dt$pct_missing * 100,
          name = "Percent Missing",
          color = "#d32f2f"
        ) |>
        highcharter::hc_title(text = "Variables with Missing Data") |>
        highcharter::hc_tooltip(
          pointFormat = "<b>{point.y:.1f}%</b> missing",
          valueSuffix = "%"
        ) |>
        highcharter::hc_legend(enabled = FALSE)
    })
    
    # Missingness by Occupation
    output$occupation_plot <- highcharter::renderHighchart({
      miss_occ <- data.table::copy(miss_obj$by_occupation)
      
      if (nrow(miss_occ) == 0 || !"occupation_native" %in% names(miss_occ)) {
        return(NULL)
      }
      
      # Get top 15 occupations by mean missingness
      top_occ <- miss_occ[
        , .(mean_missing = mean(pct_missing)), by = occupation_native
      ][order(-mean_missing)][1:15]
      
      plot_dt <- miss_occ[occupation_native %in% top_occ$occupation_native]
      
      # Filter to variables with some missingness
      plot_dt <- plot_dt[pct_missing > 0]
      
      if (nrow(plot_dt) == 0) {
        return(NULL)
      }
      
      highcharter::highchart() |>
        highcharter::hc_chart(type = "heatmap") |>
        highcharter::hc_xAxis(
          categories = unique(plot_dt$occupation_native),
          title = list(text = "Occupation")
        ) |>
        highcharter::hc_yAxis(
          categories = unique(plot_dt$variable),
          title = list(text = "Variable")
        ) |>
        highcharter::hc_add_series(
          data = plot_dt[, .(
            x = match(occupation_native, unique(plot_dt$occupation_native)) - 1,
            y = match(variable, unique(plot_dt$variable)) - 1,
            value = pct_missing * 100
          )],
          type = "heatmap",
          name = "Missingness",
          dataLabels = list(enabled = FALSE)
        ) |>
        highcharter::hc_colorAxis(
          min = 0,
          max = 100,
          stops = list(
            list(0, "#ffffff"),
            list(0.5, "#ffeb3b"),
            list(1, "#d32f2f")
          ),
          labels = list(format = "{value}%")
        ) |>
        highcharter::hc_title(text = "Top 15 Occupations by Missingness") |>
        highcharter::hc_tooltip(
          pointFormat = "<b>{point.value:.1f}%</b> missing"
        )
    })
    
    # Missingness by ISCO
    output$isco_plot <- highcharter::renderHighchart({
      miss_isco <- data.table::copy(miss_obj$by_isco)
      
      if (nrow(miss_isco) == 0 || !"occupation_isconame" %in% names(miss_isco)) {
        return(NULL)
      }
      
      # Get top 15 ISCO groups by mean missingness
      top_isco <- miss_isco[
        , .(mean_missing = mean(pct_missing)), by = occupation_isconame
      ][order(-mean_missing)][1:15]
      
      plot_dt <- miss_isco[occupation_isconame %in% top_isco$occupation_isconame]
      plot_dt <- plot_dt[pct_missing > 0]
      
      if (nrow(plot_dt) == 0) {
        return(NULL)
      }
      
      highcharter::highchart() |>
        highcharter::hc_chart(type = "heatmap") |>
        highcharter::hc_xAxis(
          categories = unique(plot_dt$occupation_isconame),
          title = list(text = "ISCO Occupation")
        ) |>
        highcharter::hc_yAxis(
          categories = unique(plot_dt$variable),
          title = list(text = "Variable")
        ) |>
        highcharter::hc_add_series(
          data = plot_dt[, .(
            x = match(occupation_isconame, unique(plot_dt$occupation_isconame)) - 1,
            y = match(variable, unique(plot_dt$variable)) - 1,
            value = pct_missing * 100
          )],
          type = "heatmap",
          name = "Missingness",
          dataLabels = list(enabled = FALSE)
        ) |>
        highcharter::hc_colorAxis(
          min = 0,
          max = 100,
          stops = list(
            list(0, "#ffffff"),
            list(0.5, "#ffeb3b"),
            list(1, "#d32f2f")
          ),
          labels = list(format = "{value}%")
        ) |>
        highcharter::hc_title(text = "Top 15 ISCO Groups by Missingness") |>
        highcharter::hc_tooltip(
          pointFormat = "<b>{point.value:.1f}%</b> missing"
        )
    })
    
    # Missingness Heatmap by Reference Date
    output$ref_date_heatmap <- highcharter::renderHighchart({
      miss_ref <- data.table::copy(miss_obj$by_ref)
      
      if (nrow(miss_ref) == 0 || !"ref_date" %in% names(miss_ref)) {
        return(NULL)
      }
      
      # Extract year for grouping
      miss_ref[, year := format(ref_date, "%Y")]
      
      # Filter to variables with some missingness
      plot_dt <- miss_ref[pct_missing > 0]
      
      if (nrow(plot_dt) == 0) {
        return(NULL)
      }
      
      highcharter::highchart() |>
        highcharter::hc_chart(type = "heatmap") |>
        highcharter::hc_xAxis(
          categories = unique(plot_dt$year),
          title = list(text = "Year")
        ) |>
        highcharter::hc_yAxis(
          categories = unique(plot_dt$variable),
          title = list(text = "Variable")
        ) |>
        highcharter::hc_add_series(
          data = plot_dt[, .(
            x = match(year, unique(plot_dt$year)) - 1,
            y = match(variable, unique(plot_dt$variable)) - 1,
            value = pct_missing * 100
          )],
          type = "heatmap",
          name = "Missingness",
          dataLabels = list(enabled = FALSE)
        ) |>
        highcharter::hc_colorAxis(
          min = 0,
          max = 100,
          stops = list(
            list(0, "#ffffff"),
            list(0.5, "#ffeb3b"),
            list(1, "#d32f2f")
          ),
          labels = list(format = "{value}%")
        ) |>
        highcharter::hc_title(text = "Missingness Over Time") |>
        highcharter::hc_tooltip(
          pointFormat = "<b>{point.value:.1f}%</b> missing"
        )
    })
    
    # Missingness by Establishment
    output$establishment_plot <- highcharter::renderHighchart({
      miss_est <- data.table::copy(miss_obj$by_est)
      
      if (nrow(miss_est) == 0 || !"est_id" %in% names(miss_est)) {
        return(NULL)
      }
      
      # Get top 15 establishments by mean missingness
      top_est <- miss_est[
        , .(mean_missing = mean(pct_missing)), by = est_id
      ][order(-mean_missing)][1:15]
      
      plot_dt <- miss_est[est_id %in% top_est$est_id]
      plot_dt <- plot_dt[pct_missing > 0]
      
      if (nrow(plot_dt) == 0) {
        return(NULL)
      }
      
      highcharter::highchart() |>
        highcharter::hc_chart(type = "heatmap") |>
        highcharter::hc_xAxis(
          categories = unique(plot_dt$est_id),
          title = list(text = "Establishment ID")
        ) |>
        highcharter::hc_yAxis(
          categories = unique(plot_dt$variable),
          title = list(text = "Variable")
        ) |>
        highcharter::hc_add_series(
          data = plot_dt[, .(
            x = match(est_id, unique(plot_dt$est_id)) - 1,
            y = match(variable, unique(plot_dt$variable)) - 1,
            value = pct_missing * 100
          )],
          type = "heatmap",
          name = "Missingness",
          dataLabels = list(enabled = FALSE)
        ) |>
        highcharter::hc_colorAxis(
          min = 0,
          max = 100,
          stops = list(
            list(0, "#ffffff"),
            list(0.5, "#ffeb3b"),
            list(1, "#d32f2f")
          ),
          labels = list(format = "{value}%")
        ) |>
        highcharter::hc_title(text = "Top 15 Establishments by Missingness") |>
        highcharter::hc_tooltip(
          pointFormat = "<b>{point.value:.1f}%</b> missing"
        )
    })
  })
}
