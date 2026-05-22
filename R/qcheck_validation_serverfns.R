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
#' @importFrom shiny moduleServer renderText observeEvent updateSelectInput downloadHandler req
#' @importFrom gt render_gt gt fmt_number fmt_markdown cols_align tab_style cell_fill opt_table_outline opt_row_striping
#' @importFrom writexl write_xlsx
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

    # Helper: non-clickable badge HTML
    make_badge <- function(pass_rate, is_error) {
      if (is_error) {
        return("<span style='background:#9e9e9e;color:white;padding:2px 8px;border-radius:4px;'>Does Not Apply</span>")
      }
      if (pass_rate >= 100) {
        return("<span style='background:#4caf50;color:white;padding:2px 8px;border-radius:4px;'>PASS</span>")
      }
      if (pass_rate >= 80) {
        return("<span style='background:#ff9800;color:white;padding:2px 8px;border-radius:4px;'>WARNING</span>")
      }
      "<span style='background:#f44336;color:white;padding:2px 8px;border-radius:4px;'>FAIL</span>"
    }

    # Helper: build styled gt validation table
    make_validation_table <- function(df) {
      df$Status <- mapply(make_badge, pass_rate = df$`Pass Rate`, is_error = df$Errors)

      df |>
        gt::gt() |>
        gt::fmt_markdown(columns = Status) |>
        gt::fmt_number(
          columns  = c(`Total Records`, Passes, Fails),
          decimals = 0,
          use_seps = TRUE
        ) |>
        gt::fmt_number(
          columns  = `Pass Rate`,
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

    output$contract_table <- gt::render_gt(make_validation_table(contract_report))
    output$personnel_table <- gt::render_gt(make_validation_table(personnel_report))

    # Populate selectInputs with only rules that have violations
    failing_rules <- function(report) {
      report$Rule[!is.na(report$`Pass Rate`) & report$`Pass Rate` < 100 & !report$Errors]
    }

    shiny::observe({
      rules <- failing_rules(contract_report)
      shiny::updateSelectInput(session, "contract_rule_select",
        choices  = if (length(rules) > 0) rules else c("No failing rules" = ""),
        selected = if (length(rules) > 0) rules[1] else ""
      )
    })

    shiny::observe({
      rules <- failing_rules(personnel_report)
      shiny::updateSelectInput(session, "personnel_rule_select",
        choices  = if (length(rules) > 0) rules else c("No failing rules" = ""),
        selected = if (length(rules) > 0) rules[1] else ""
      )
    })

    # Helper: sanitise data.frame — strip illegal XML chars from character cols
    sanitise_df <- function(df) {
      df <- as.data.frame(df)
      char_cols <- vapply(df, is.character, logical(1))
      df[char_cols] <- lapply(df[char_cols], function(x) {
        gsub("[[:cntrl:]]", "", x)
      })
      df
    }

    to_filename <- function(rule) gsub("[^A-Za-z0-9_]", "_", trimws(rule))

    output$contract_download <- shiny::downloadHandler(
      filename = function() {
        paste0(to_filename(shiny::req(input$contract_rule_select)), "_violations.xlsx")
      },
      content = function(file) {
        rule <- shiny::req(input$contract_rule_select)
        dt   <- contract_violation[[rule]]
        shiny::req(!is.null(dt), nrow(dt) > 0)
        writexl::write_xlsx(sanitise_df(dt), file)
      }
    )

    output$personnel_download <- shiny::downloadHandler(
      filename = function() {
        paste0(to_filename(shiny::req(input$personnel_rule_select)), "_violations.xlsx")
      },
      content = function(file) {
        rule <- shiny::req(input$personnel_rule_select)
        dt   <- personnel_violation[[rule]]
        shiny::req(!is.null(dt), nrow(dt) > 0)
        writexl::write_xlsx(sanitise_df(dt), file)
      }
    )

  })
}
