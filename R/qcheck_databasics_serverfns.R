#' Data Basics Server Module
#'
#' Server logic for the Data Basics section, processing quality control diagnostics
#' from the dynamicqc_obj object and rendering tables and visualizations.
#'
#' @param id Character string. The module namespace ID.
#' @param dynamicqc_obj A dynamicqc object created by \code{compute_dynamicqc()}.
#'
#' @return None. Called for side effects (renders Shiny outputs).
#'
#' @importFrom shiny moduleServer renderPlot renderUI tagList updateSliderInput
#' @importFrom data.table data.table as.data.table rbindlist
#' @importFrom ggplot2 ggplot aes geom_line geom_point scale_y_continuous scale_color_manual labs theme_minimal theme element_blank element_text annotate
#' @importFrom gt render_gt gt fmt_number fmt_markdown cols_align cols_width px tab_style cell_fill cell_text cells_body opt_table_outline opt_row_striping tab_header md tab_footnote
#' @importFrom scales comma_format
#' @importFrom utils head
#' @importFrom highcharter renderHighchart highchart hc_xAxis hc_yAxis hc_add_series hc_title hc_subtitle hc_tooltip hc_legend hcaes datetime_to_timestamp
#'
#' @keywords internal
databasics_server <- function(id, dynamicqc_obj) {

  shiny::moduleServer(id, 
                      function(input, output, session) {
    
    # Extract components from dynamicqc_obj
    qc_obj <- dynamicqc_obj$qc_obj
    temporal_cov <- dynamicqc_obj$temporal_coverage
    metadata <- dynamicqc_obj$metadata
    
    # Module Dimensions Table
    output$dimensions_table <- gt::render_gt({
      dim_dt <- data.table::data.table(
        Module = c("Contract", "Personnel", "Establishment"),
        Observations = c(
          metadata$n_obs$contract,
          metadata$n_obs$personnel,
          metadata$n_obs$establishment
        ),
        Variables = c(
          metadata$n_vars$contract,
          metadata$n_vars$personnel,
          metadata$n_vars$establishment
        )
      )
      
      dim_dt |>
        gt::gt() |>
        gt::fmt_number(
          columns = c(Observations, Variables),
          use_seps = TRUE,
          decimals = 0
        ) |>
        gt::cols_align(
          align = "center",
          columns = c(Observations, Variables)
        ) |>
        gt::tab_style(
          style = list(gt::cell_fill(color = "#f7f7f7")),
          locations = gt::cells_body()
        ) |>
        gt::opt_table_outline() |>
        gt::opt_row_striping()
    })
    
    # Dynamic date slider UI
    output$date_slider_ui <- shiny::renderUI({
      date_range <- metadata$date_range
      
      shiny::sliderInput(
        session$ns("date_range"),
        "Select Date Range:",
        min = date_range[1],
        max = date_range[2],
        value = c(date_range[1], date_range[2]),
        timeFormat = "%Y-%m-%d",
        width = "100%"
      )
    })
    
    # Temporal Coverage Plot
    output$coverage_plot <- highcharter::renderHighchart({
      # Combine pre-aggregated temporal coverage data
      coverage_list <- list()
      
      if (!is.null(temporal_cov$contract)) {
        coverage_list[[1]] <- data.table::copy(temporal_cov$contract)[, module := "Contract"]
      }
      
      if (!is.null(temporal_cov$personnel)) {
        coverage_list[[2]] <- data.table::copy(temporal_cov$personnel)[, module := "Personnel"]
      }
      
      if (!is.null(temporal_cov$establishment)) {
        coverage_list[[3]] <- data.table::copy(temporal_cov$establishment)[, module := "Establishment"]
      }
      
      coverage_dt <- data.table::rbindlist(coverage_list, use.names = TRUE, fill = TRUE)
      
      if (nrow(coverage_dt) == 0) {
        return(NULL)
      }
      
      # Filter by selected date range (only if slider has been rendered)
      if (!is.null(input$date_range) && length(input$date_range) == 2) {
        coverage_dt <- coverage_dt[ref_date >= input$date_range[1] & ref_date <= input$date_range[2]]
      }
      
      # Create highchart
      hc <- 
        highcharter::highchart() |>
        highcharter::hc_xAxis(type = "datetime", 
                              title = list(text = "Reference Date")) |>
        highcharter::hc_yAxis(title = list(text = "Number of Observations")) |>
        highcharter::hc_title(
          text = "Observation Counts by Reference Period and Module"
        ) |>
        highcharter::hc_subtitle(
          text = "Spikes or gaps may indicate data quality issues or organizational changes"
        ) |>
        highcharter::hc_tooltip(
          shared = TRUE,
          crosshairs = TRUE
        ) |>
        highcharter::hc_legend(enabled = TRUE)
      
      # Add Contract series if data exists
      contract_data <- coverage_dt[module == "Contract"]
      if (nrow(contract_data) > 0) {
        hc <- hc |>
          highcharter::hc_add_series(
            data = contract_data,
            type = "line",
            name = "Contract",
            color = "#1f77b4",
            hcaes(x = datetime_to_timestamp(ref_date), y = n_obs)
          )
      }
      
      # Add Personnel series if data exists
      personnel_data <- coverage_dt[module == "Personnel"]
      if (nrow(personnel_data) > 0) {
        hc <- hc |>
          highcharter::hc_add_series(
            data = personnel_data,
            type = "line",
            name = "Personnel",
            color = "#ff7f0e",
            hcaes(x = datetime_to_timestamp(ref_date), y = n_obs)
          )
      }
      
      # Add Establishment series if data exists
      establishment_data <- coverage_dt[module == "Establishment"]
      if (nrow(establishment_data) > 0) {
        hc <- hc |>
          highcharter::hc_add_series(
            data = establishment_data,
            type = "line",
            name = "Establishment",
            color = "#2ca02c",
            hcaes(x = datetime_to_timestamp(ref_date), y = n_obs)
          )
      }
      
      hc
    })
    
    # Variable Structure Table
    output$structure_table <- gt::render_gt({
      structure_list <- qc_obj$structure
      
      structure_dt <- data.table::data.table(
        Module = c("Contract", "Establishment", "Personnel"),
        `Missing Variables` = c(
          structure_list[[1]]$missing,
          structure_list[[2]]$missing,
          structure_list[[3]]$missing
        ),
        `Extra Variables` = c(
          structure_list[[1]]$extra,
          structure_list[[2]]$extra,
          structure_list[[3]]$extra
        )
      )
      
      structure_dt |>
        gt::gt() |>
        gt::fmt_markdown(columns = c(`Missing Variables`, `Extra Variables`)) |>
        gt::cols_width(
          Module ~ gt::px(120),
          `Missing Variables` ~ gt::px(400),
          `Extra Variables` ~ gt::px(400)
        ) |>
        gt::tab_style(
          style = list(gt::cell_text(weight = "bold")),
          locations = gt::cells_body(columns = Module)
        ) |>
        gt::opt_table_outline() |>
        gt::opt_row_striping()
    })
    
    # Primary Key Integrity Table
    output$keys_table <- gt::render_gt({
      keys_obj <- qc_obj$keys
      
      keys_dt <- data.table::data.table(
        Check = c("Primary Keys Unique", "Number of Duplicate Groups"),
        Status = c(
          ifelse(keys_obj$is_unique, 
                 "<span style='background:#4caf50;color:white;padding:2px 8px;border-radius:4px;'>\u2713 PASS</span>",
                 "<span style='background:#f44336;color:white;padding:2px 8px;border-radius:4px;'>\u2717 FAIL</span>"),
          as.character(nrow(keys_obj$duplicate_groups))
        )
      )
      
      keys_dt |>
        gt::gt() |>
        gt::fmt_markdown(columns = Status) |>
        gt::cols_width(
          Check ~ gt::px(250),
          Status ~ gt::px(150)
        ) |>
        gt::opt_table_outline() |>
        gt::opt_row_striping()
    })
    
    # Duplicate Keys Details (conditional)
    output$duplicate_keys_ui <- shiny::renderUI({
      keys_obj <- qc_obj$keys
      
      if (nrow(keys_obj$duplicate_groups) > 0) {
        gt::gt_output(session$ns("duplicate_keys_table"))
      }
    })
    
    output$duplicate_keys_table <- gt::render_gt({
      keys_obj <- qc_obj$keys
      
      if (nrow(keys_obj$duplicate_groups) > 0) {
        keys_obj$duplicate_groups |>
          head(20) |>
          gt::gt() |>
          gt::tab_header(title = gt::md("### Duplicate Key Groups (First 20)")) |>
          gt::fmt_number(columns = N, decimals = 0) |>
          gt::opt_table_outline() |>
          gt::opt_row_striping()
      }
    })
    
    # Orphan Checks Table
    output$orphans_table <- gt::render_gt({
      orphans_obj <- qc_obj$orphans
      
      orphans_dt <- data.table::data.table(
        Check = c(
          "Personnel IDs in Contract missing from Personnel module",
          "Establishment IDs in Contract missing from Establishment module"
        ),
        `Missing Count` = c(
          orphans_obj$personnel_vs_contract$n_missing,
          orphans_obj$establishment_vs_contract$n_missing
        ),
        Status = c(
          ifelse(orphans_obj$personnel_vs_contract$n_missing == 0,
                 "<span style='background:#4caf50;color:white;padding:2px 8px;border-radius:4px;'>\u2713 PASS</span>",
                 "<span style='background:#ff9800;color:white;padding:2px 8px;border-radius:4px;'>\u26a0 WARNING</span>"),
          ifelse(orphans_obj$establishment_vs_contract$n_missing == 0,
                 "<span style='background:#4caf50;color:white;padding:2px 8px;border-radius:4px;'>\u2713 PASS</span>",
                 "<span style='background:#ff9800;color:white;padding:2px 8px;border-radius:4px;'>\u26a0 WARNING</span>")
        )
      )
      
      orphans_dt |>
        gt::gt() |>
        gt::fmt_markdown(columns = Status) |>
        gt::fmt_number(columns = `Missing Count`, decimals = 0, use_seps = TRUE) |>
        gt::cols_width(
          Check ~ gt::px(450),
          `Missing Count` ~ gt::px(150),
          Status ~ gt::px(120)
        ) |>
        gt::opt_table_outline() |>
        gt::opt_row_striping()
    })
    
    # Orphan Details (conditional)
    output$orphan_details_ui <- shiny::renderUI({
      orphans_obj <- qc_obj$orphans
      
      shiny::tagList(
        if (orphans_obj$establishment_vs_contract$n_missing > 0) {
          gt::gt_output(session$ns("orphan_est_table"))
        },
        if (orphans_obj$personnel_vs_contract$n_missing > 0) {
          gt::gt_output(session$ns("orphan_pers_table"))
        }
      )
    })
    
    output$orphan_est_table <- gt::render_gt({
      orphans_obj <- qc_obj$orphans
      
      if (orphans_obj$establishment_vs_contract$n_missing > 0) {
        orphan_est_dt <- data.table::data.table(
          `Orphaned Establishment IDs` = head(orphans_obj$establishment_vs_contract$missing_ids, 25)
        )
        
        orphan_est_dt |>
          gt::gt() |>
          gt::tab_header(title = gt::md("### Sample Orphaned Establishment IDs (First 25)")) |>
          gt::opt_table_outline() |>
          gt::opt_row_striping() |>
          gt::tab_footnote(
            footnote = paste0("Total orphaned establishment IDs: ", 
                              orphans_obj$establishment_vs_contract$n_missing)
          )
      }
    })
    
    output$orphan_pers_table <- gt::render_gt({
      orphans_obj <- qc_obj$orphans
      
      if (orphans_obj$personnel_vs_contract$n_missing > 0) {
        orphan_pers_dt <- data.table::data.table(
          `Orphaned Personnel IDs` = head(orphans_obj$personnel_vs_contract$missing_ids, 25)
        )
        
        orphan_pers_dt |>
          gt::gt() |>
          gt::tab_header(title = gt::md("### Sample Orphaned Personnel IDs (First 25)")) |>
          gt::opt_table_outline() |>
          gt::opt_row_striping() |>
          gt::tab_footnote(
            footnote = paste0("Total orphaned personnel IDs: ", 
                              orphans_obj$personnel_vs_contract$n_missing)
          )
      }
    })
    
  })
}