#' Data Basics Server Module
#'
#' Server logic for the Data Basics section, processing quality control diagnostics
#' from the qc_obj object and rendering tables and visualizations.
#'
#' @param id Character string. The module namespace ID.
#' @param qc_obj A quality control object created by \code{govhr::compute_qualitycontrol()}.
#'
#' @return None. Called for side effects (renders Shiny outputs).
#'
#' @importFrom shiny moduleServer renderUI tagList
#' @importFrom data.table data.table as.data.table rbindlist copy
#' @importFrom gt render_gt gt fmt_number fmt_markdown cols_align cols_width px tab_style cell_fill cell_text cells_body opt_table_outline opt_row_striping tab_header md tab_footnote
#' @importFrom utils head
#' @importFrom highcharter renderHighchart highchart hc_xAxis hc_yAxis hc_add_series hc_title hc_subtitle hc_tooltip hc_legend hcaes datetime_to_timestamp
#'
#' @keywords internal
databasics_server <- function(id, qc_obj) {

  shiny::moduleServer(id, 
                      function(input, output, session) {
    
    # Module Dimensions Table
    output$dimensions_table <- gt::render_gt({
      meta <- qc_obj$metadata
      dim_dt <- data.table::data.table(
        Module  = c("Contract", "Personnel", "Establishment"),
        Records = c(
          meta$n_obs$contract,
          meta$n_obs$personnel,
          meta$n_obs$establishment
        ),
        Fields  = c(
          meta$n_vars$contract,
          meta$n_vars$personnel,
          meta$n_vars$establishment
        ),
        `Date Range` = paste0(
          format(meta$date_range[1], "%Y-%m"),
          " to ",
          format(meta$date_range[2], "%Y-%m")
        )
      )

      dim_dt |>
        gt::gt() |>
        gt::fmt_number(
          columns = c(Records, Fields),
          use_seps = TRUE,
          decimals = 0
        ) |>
        gt::cols_align(align = "center", columns = c(Records, Fields, `Date Range`)) |>
        gt::tab_style(
          style     = list(gt::cell_fill(color = "#f7f7f7")),
          locations = gt::cells_body()
        ) |>
        gt::opt_table_outline() |>
        gt::opt_row_striping()
    })

    # Temporal Coverage Chart
    output$coverage_plot <- highcharter::renderHighchart({
      tc <- qc_obj$temporal_coverage

      coverage_list <- list()
      if (!is.null(tc$contract))
        coverage_list[[1]] <- data.table::copy(tc$contract)[, module := "Contract"]
      if (!is.null(tc$personnel))
        coverage_list[[2]] <- data.table::copy(tc$personnel)[, module := "Personnel"]

      coverage_dt <- data.table::rbindlist(coverage_list, use.names = TRUE, fill = TRUE)
      if (nrow(coverage_dt) == 0) return(NULL)

      hc <- highcharter::highchart() |>
        highcharter::hc_xAxis(type = "datetime", title = list(text = "Reference Date")) |>
        highcharter::hc_yAxis(title = list(text = "Number of Records")) |>
        highcharter::hc_title(text = "Record Counts by Reference Period and Module") |>
        highcharter::hc_subtitle(
          text = "Spikes or gaps may indicate data quality issues or organisational changes"
        ) |>
        highcharter::hc_tooltip(shared = TRUE, crosshairs = TRUE) |>
        highcharter::hc_legend(enabled = TRUE)

      contract_data <- coverage_dt[module == "Contract"]
      if (nrow(contract_data) > 0)
        hc <- hc |> highcharter::hc_add_series(
          data  = contract_data, type = "line", name = "Contract",
          color = "#1f77b4",
          highcharter::hcaes(x = highcharter::datetime_to_timestamp(ref_date), y = n_obs)
        )

      personnel_data <- coverage_dt[module == "Personnel"]
      if (nrow(personnel_data) > 0)
        hc <- hc |> highcharter::hc_add_series(
          data  = personnel_data, type = "line", name = "Personnel",
          color = "#ff7f0e",
          highcharter::hcaes(x = highcharter::datetime_to_timestamp(ref_date), y = n_obs)
        )

      hc |> add_export_menu()
    })
    
    # Variable Structure Table
    output$structure_table <- gt::render_gt({
      structure_list <- qc_obj$structure
      
      structure_dt <- data.table::data.table(
        Module = c("Contract", "Establishment", "Personnel"),
        `Missing Fields` = c(
          structure_list[[1]]$missing,
          structure_list[[2]]$missing,
          structure_list[[3]]$missing
        ),
        `Extra Fields` = c(
          structure_list[[1]]$extra,
          structure_list[[2]]$extra,
          structure_list[[3]]$extra
        )
      )
      
      structure_dt |>
        gt::gt() |>
        gt::fmt_markdown(columns = c(`Missing Fields`, `Extra Fields`)) |>
        gt::cols_width(
          Module ~ gt::px(120),
          `Missing Fields` ~ gt::px(400),
          `Extra Fields` ~ gt::px(400)
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

      if (is.null(keys_obj)) {
        # Fall back to validation rules summary — use $report with new structure
        contract_report <- if (is.data.frame(qc_obj$validation$contract)) {
          qc_obj$validation$contract
        } else {
          qc_obj$validation$contract$report
        }
        unique_rules <- contract_report[
          grepl("[Uu]nique", contract_report$Rule), ]
        data.table::data.table(
          Check = unique_rules$Rule,
          Description = unique_rules$Description,
          `Pass Rate` = paste0(round(unique_rules$`Pass Rate`, 1), "%"),
          Fails = unique_rules$Fails
        ) |>
          gt::gt() |>
          gt::opt_table_outline() |>
          gt::opt_row_striping()
      } else {
        keys_dt <- data.table::data.table(
          Check = c("Are all employee IDs unique?", "Number of duplicate ID groups found"),
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
            Check  ~ gt::px(250),
            Status ~ gt::px(150)
          ) |>
          gt::opt_table_outline() |>
          gt::opt_row_striping()
      }
    })    # Duplicate Keys Details (conditional)
    output$duplicate_keys_ui <- shiny::renderUI({
      keys_obj <- qc_obj$keys
      if (is.null(keys_obj)) return(NULL)
      if (nrow(keys_obj$duplicate_groups) > 0) {
        gt::gt_output(session$ns("duplicate_keys_table"))
      }
    })
    
    output$duplicate_keys_table <- gt::render_gt({
      keys_obj <- qc_obj$keys
      if (is.null(keys_obj)) return(gt::gt(data.frame()))
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
      if (is.null(orphans_obj)) return(gt::gt(data.frame(Note = "No orphan check data available.")))
      orphans_dt <- data.table::data.table(
        Check = c(
          "Are all contract employee IDs found in the personnel file?",
          "Are all contract establishment IDs found in the establishment file?"
        ),
        `Unmatched Records` = c(
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
        gt::fmt_number(columns = `Unmatched Records`, decimals = 0, use_seps = TRUE) |>
        gt::cols_width(
          Check ~ gt::px(450),
          `Unmatched Records` ~ gt::px(150),
          Status ~ gt::px(120)
        ) |>
        gt::opt_table_outline() |>
        gt::opt_row_striping()
    })
    
    # Orphan Details (conditional)
    output$orphan_details_ui <- shiny::renderUI({
      orphans_obj <- qc_obj$orphans
      if (is.null(orphans_obj)) return(NULL)
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