#' Missingness Server Module
#'
#' Server logic for the Missingness section, processing and visualizing missing data patterns.
#'
#' @param id Character string. The module namespace ID.
#' @param dynamicqc_obj A dynamicqc object created by \code{compute_dynamicqc()}.
#'
#' @return None. Called for side effects (renders Shiny outputs).
#'
#' @importFrom shiny moduleServer req renderUI observe
#' @importFrom data.table data.table copy setorder dcast
#' @importFrom highcharter renderHighchart highchart hc_chart hc_xAxis hc_yAxis hc_add_series hc_title hc_subtitle hc_tooltip hc_legend hcaes hc_colorAxis hc_exporting
#' @importFrom plotly renderPlotly plot_ly layout colorbar
#' @importFrom scales percent_format
#' @importFrom stats as.formula
#'
#' @keywords internal
missingness_server <- function(id, dynamicqc_obj) {
  
  shiny::moduleServer(id, function(input, output, session) {
    
    # Extract missingness data
    miss_obj <- dynamicqc_obj$qc_obj$missingness
    
    # Overall Missingness Plot
    output$overall_plot <- highcharter::renderHighchart({
      overall_dt <- data.table::copy(miss_obj$overall)
      
      if (nrow(overall_dt) == 0) {
        return(NULL)
      }
      
      # Sort by missingness
      plot_dt <- overall_dt
      data.table::setorder(plot_dt, -pct_missing)
      
      highcharter::highchart() |>
        highcharter::hc_chart(type = "bar") |>
        highcharter::hc_xAxis(
          categories = plot_dt$variable,
          title = list(text = "Field")
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
        highcharter::hc_title(text = "Fields with Missing Data") |>
        highcharter::hc_tooltip(
          pointFormat = "<b>{point.y:.1f}%</b> missing",
          valueSuffix = "%"
        ) |>
        highcharter::hc_legend(enabled = FALSE) |>
        add_export_menu()
    })
    
    # Missingness by Occupation (Consolidated - Native or ISCO)
    output$occupation_plot <- plotly::renderPlotly({
      
      req(input$occupation_type, input$occupation_top_n)
      
      # Select the appropriate dataset based on occupation type
      if (input$occupation_type == "native") {
        miss_data <- data.table::copy(miss_obj$by_occupation)
        occ_col <- "occupation_native"
        title_text <- paste0("Top ", input$occupation_top_n, " Occupations by Missingness")
        x_label <- "Occupation"
      } else {
        miss_data <- data.table::copy(miss_obj$by_isco)
        occ_col <- "occupation_isconame"
        title_text <- paste0("Top ", input$occupation_top_n, " ISCO Occupations by Missingness")
        x_label <- "ISCO Occupation"
      }
      
      if (nrow(miss_data) == 0 || !occ_col %in% names(miss_data)) {
        return(NULL)
      }
      
      # Get top N occupations by mean missingness
      top_occ <- miss_data[
        , .(mean_missing = mean(pct_missing)), by = occ_col
      ][order(-mean_missing)][1:min(input$occupation_top_n, .N)]
      
      plot_dt <- miss_data[get(occ_col) %in% top_occ[[occ_col]]]
      
      if (nrow(plot_dt) == 0) {
        return(NULL)
      }
      
      # Convert to wide format for plotly heatmap
      plot_wide <- data.table::dcast(
        plot_dt,
        as.formula(paste("variable ~", occ_col)),
        value.var = "pct_missing"
      )
      
      # Extract matrix for heatmap
      var_names <- as.character(plot_wide$variable)
      plot_wide[, variable := NULL]
      plot_matrix <- as.matrix(plot_wide) * 100
      
      # Clean column names for UTF-8 issues
      col_names <- colnames(plot_matrix)
      col_names <- iconv(col_names, to = "UTF-8", sub = "byte")
      colnames(plot_matrix) <- col_names
      
      # Order columns by mean missingness
      col_order <- match(top_occ[[occ_col]], colnames(plot_matrix))
      plot_matrix <- plot_matrix[, col_order[!is.na(col_order)]]
      
      plotly::plot_ly(
        x = colnames(plot_matrix),
        y = var_names,
        z = plot_matrix,
        type = "heatmap",
        colors = c("#FFFFFF", "#FEF0D9", "#FDCC8A", "#FC8D59", "#D7301F"),
        zmin = 0,
        zmax = 100,
        hovertemplate = paste0(
          "<b>", x_label, ":</b> %{x}<br>",
          "<b>Field:</b> %{y}<br>",
          "<b>Missing:</b> %{z:.1f}%<extra></extra>"
        )
      ) |>
        plotly::layout(
          title = title_text,
          xaxis = list(title = x_label),
          yaxis = list(title = "Field"),
          margin = list(l = 150, r = 50, t = 80, b = 100)
        ) |>
        plotly::colorbar(title = "Missing %")
    })
    
    # Missingness Heatmap by Reference Date
    output$ref_date_heatmap <- highcharter::renderHighchart({
      miss_ref <- data.table::copy(miss_obj$by_ref)
      
      if (nrow(miss_ref) == 0 || !"ref_date" %in% names(miss_ref)) {
        return(NULL)
      }
      
      # Extract year for grouping
      miss_ref[, year := format(ref_date, "%Y")]
      
      plot_dt <- miss_ref
      
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
          title = list(text = "Field")
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
            list(0, "#FFFFFF"),
            list(0.25, "#FEF0D9"),
            list(0.5, "#FDCC8A"),
            list(0.75, "#FC8D59"),
            list(1, "#D7301F")
          ),
          labels = list(format = "{value}%")
        ) |>
        highcharter::hc_title(text = "Missingness Over Time") |>
        highcharter::hc_tooltip(
          pointFormat = "<b>{point.value:.1f}%</b> missing"
        ) |>
        add_export_menu()
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
          title = list(text = "Field")
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
            list(0, "#FFFFFF"),
            list(0.25, "#FEF0D9"),
            list(0.5, "#FDCC8A"),
            list(0.75, "#FC8D59"),
            list(1, "#D7301F")
          ),
          labels = list(format = "{value}%")
        ) |>
        highcharter::hc_title(text = "Top 15 Establishments by Missingness") |>
        highcharter::hc_tooltip(
          pointFormat = "<b>{point.value:.1f}%</b> missing"
        ) |>
        add_export_menu()
    })
  })
}
