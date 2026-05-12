#############################################################################################
############################# VALIDATION MODULE SERVER #######################################
#############################################################################################

#' Validation Rules Server Module
#'
#' Server logic for the Validation Rules section, rendering pass/fail tables
#' for contract and personnel validation rules.
#'
#' @param id Character string. The module namespace ID.
#' @param qc_obj A quality control object from \code{govhr::compute_qualitycontrol()}.
#'
#' @return None. Called for side effects (renders Shiny outputs).
#'
#' @importFrom shiny moduleServer renderText reactive reactiveVal observeEvent req
#' @importFrom gt render_gt gt fmt_number fmt_markdown cols_align tab_style cell_fill opt_table_outline opt_row_striping
#' @importFrom writexl write_xlsx
#' @importFrom jsonlite base64_enc
#'
#' @keywords internal
validation_server <- function(id, qc_obj) {

  shiny::moduleServer(id, function(input, output, session) {

    contract_report     <- qc_obj$validation$contract$report
    contract_violation  <- qc_obj$validation$contract$violations
    personnel_report    <- qc_obj$validation$personnel$report
    personnel_violation <- qc_obj$validation$personnel$violations

    # Contract overall pass rate (weighted across all rules)
    output$contract_pass_rate <- shiny::renderText({
      df <- contract_report
      total_passes  <- sum(df$Passes,          na.rm = TRUE)
      total_records <- sum(df$`Total Records`, na.rm = TRUE)
      paste0(round(total_passes / total_records * 100, 1), "%")
    })

    # Personnel overall pass rate (exclude errored rules)
    output$personnel_pass_rate <- shiny::renderText({
      df <- personnel_report[!personnel_report$Errors, ]
      total_passes  <- sum(df$Passes,          na.rm = TRUE)
      total_records <- sum(df$`Total Records`, na.rm = TRUE)
      paste0(round(total_passes / total_records * 100, 1), "%")
    })

    # Helper: build clickable badge HTML for WARNING/FAIL rules
    make_badge <- function(rule, pass_rate, is_error, input_id) {
      if (is_error) {
        return("<span style='background:#9e9e9e;color:white;padding:2px 8px;border-radius:4px;'>Does Not Apply</span>")
      }
      if (pass_rate >= 100) {
        return("<span style='background:#4caf50;color:white;padding:2px 8px;border-radius:4px;'>PASS</span>")
      }
      label  <- if (pass_rate >= 80) "WARNING" else "FAIL"
      colour <- if (pass_rate >= 80) "#ff9800" else "#f44336"
      rule_js <- gsub("'", "\\\\'", rule)
      paste0(
        "<span style='background:", colour, ";color:white;padding:2px 8px;",
        "border-radius:4px;cursor:pointer;' ",
        "title='Click to download violations' ",
        "onclick=\"Shiny.setInputValue('", session$ns(input_id), "', '",
        rule_js, "', {priority: 'event'})\">",
        label, " &#11123;</span>"
      )
    }

    # Helper: build styled gt validation table with clickable badges
    make_validation_table <- function(df, input_id) {
      df$Status <- mapply(
        make_badge,
        rule      = df$Rule,
        pass_rate = df$`Pass Rate`,
        is_error  = df$Errors,
        input_id  = input_id
      )

      df |>
        gt::gt() |>
        gt::fmt_markdown(columns = Status) |>
        gt::fmt_number(
          columns  = c(`Total Records`, Passes, Fails),
          decimals = 0,
          use_seps = TRUE
        ) |>
        gt::fmt_number(
          columns = `Pass Rate`,
          decimals = 1,
          suffix   = "%"
        ) |>
        gt::cols_align(
          align   = "center",
          columns = c(`Total Records`, Passes, Fails, `Pass Rate`, Status)
        ) |>
        gt::tab_style(
          style     = gt::cell_fill(color = "#fff3cd"),
          locations = gt::cells_body(rows = `Pass Rate` < 100 & `Pass Rate` >= 80 & !Errors)
        ) |>
        gt::tab_style(
          style     = gt::cell_fill(color = "#fce4e4"),
          locations = gt::cells_body(rows = `Pass Rate` < 80 & !Errors)
        ) |>
        gt::opt_table_outline() |>
        gt::opt_row_striping(row_striping = FALSE)
    }

    output$contract_table <- gt::render_gt({
      make_validation_table(contract_report, "contract_rule_click")
    })

    output$personnel_table <- gt::render_gt({
      make_validation_table(personnel_report, "personnel_rule_click")
    })

    # Helper: write data.table to temp xlsx and return base64-encoded bytes
    to_filename <- function(rule) gsub("[^A-Za-z0-9_]", "_", trimws(rule))

    send_xlsx_download <- function(violations, rule) {
      dt <- violations[[rule]]
      shiny::req(!is.null(dt), nrow(dt) > 0)
      tmp <- tempfile(fileext = ".xlsx")
      on.exit(unlink(tmp), add = TRUE)
      writexl::write_xlsx(as.data.frame(dt), tmp)
      b64      <- jsonlite::base64_enc(readBin(tmp, "raw", file.info(tmp)$size))
      filename <- paste0(to_filename(rule), "_violations.xlsx")
      session$sendCustomMessage("download_blob", list(b64 = b64, filename = filename))
    }

    shiny::observeEvent(input$contract_rule_click, {
      send_xlsx_download(contract_violation, input$contract_rule_click)
    })

    shiny::observeEvent(input$personnel_rule_click, {
      send_xlsx_download(personnel_violation, input$personnel_rule_click)
    })

  })
}
