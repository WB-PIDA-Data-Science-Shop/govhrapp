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
#' @importFrom shiny moduleServer renderText
#' @importFrom gt render_gt gt fmt_number fmt_markdown cols_align tab_style cell_fill opt_table_outline opt_row_striping
#'
#' @keywords internal
validation_server <- function(id, qc_obj) {

  shiny::moduleServer(id, function(input, output, session) {

    validation <- qc_obj$validation

    # Contract overall pass rate (weighted across all rules)
    output$contract_pass_rate <- shiny::renderText({
      df <- validation$contract
      total_passes  <- sum(df$Passes,        na.rm = TRUE)
      total_records <- sum(df$`Total Records`, na.rm = TRUE)
      paste0(round(total_passes / total_records * 100, 1), "%")
    })

    # Personnel overall pass rate (exclude errored rules)
    output$personnel_pass_rate <- shiny::renderText({
      df <- validation$personnel[!validation$personnel$Errors, ]
      total_passes  <- sum(df$Passes,        na.rm = TRUE)
      total_records <- sum(df$`Total Records`, na.rm = TRUE)
      paste0(round(total_passes / total_records * 100, 1), "%")
    })

    # Helper to build a styled gt validation table
    make_validation_table <- function(df) {
      df$Status <- ifelse(
        df$Errors,
        "<span style='background:#9e9e9e;color:white;padding:2px 8px;border-radius:4px;'>ERROR</span>",
        ifelse(
          df$`Pass Rate` >= 100,
          "<span style='background:#4caf50;color:white;padding:2px 8px;border-radius:4px;'>PASS</span>",
          ifelse(
            df$`Pass Rate` >= 80,
            "<span style='background:#ff9800;color:white;padding:2px 8px;border-radius:4px;'>WARNING</span>",
            "<span style='background:#f44336;color:white;padding:2px 8px;border-radius:4px;'>FAIL</span>"
          )
        )
      )

      df |>
        gt::gt() |>
        gt::fmt_markdown(columns = Status) |>
        gt::fmt_number(
          columns = c(`Total Records`, Passes, Fails),
          decimals = 0,
          use_seps = TRUE
        ) |>
        gt::fmt_number(
          columns = `Pass Rate`,
          decimals = 1,
          suffix = "%"
        ) |>
        gt::cols_align(
          align = "center",
          columns = c(`Total Records`, Passes, Fails, `Pass Rate`, Status)
        ) |>
        gt::tab_style(
          style = gt::cell_fill(color = "#fff3cd"),
          locations = gt::cells_body(rows = `Pass Rate` < 100 & `Pass Rate` >= 80 & !Errors)
        ) |>
        gt::tab_style(
          style = gt::cell_fill(color = "#fce4e4"),
          locations = gt::cells_body(rows = `Pass Rate` < 80 & !Errors)
        ) |>
        gt::opt_table_outline() |>
        gt::opt_row_striping(row_striping = FALSE)
    }

    output$contract_table <- gt::render_gt({
      make_validation_table(validation$contract)
    })

    output$personnel_table <- gt::render_gt({
      make_validation_table(validation$personnel)
    })

  })
}
