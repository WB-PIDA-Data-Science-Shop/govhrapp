#' Validation Rules UI Module
#'
#' UI components for the Validation Rules section of the quality control dashboard.
#'
#' @param id Character string. The module namespace ID.
#'
#' @return A Shiny UI object containing validation rule cards.
#'
#' @importFrom shiny NS tagList textOutput selectInput downloadButton p tags
#' @importFrom bslib layout_columns card card_header card_body value_box
#' @importFrom gt gt_output
#' @importFrom bsicons bs_icon
#'
#' @keywords internal
validation_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    bslib::card(
      bslib::card_header(
        "Validation: Overview",
        bslib::popover(
          bsicons::bs_icon("info-circle-fill"),
          "Validation rules are applied to the personnel and contract datasets to identify potential data quality issues. The pass rate indicates the percentage of records that meet the validation criteria.",
          title = "Validation Rules Overview",
          placement = "left"
        ),
        class = "d-flex justify-content-between"
      ),
      # Summary value boxes
      bslib::layout_columns(
        col_widths = c(6, 6),
        uiOutput(
          ns("validation_personnel")
        ),
        uiOutput(
          ns("validation_contract")
        )
      )
    ),

    # Contract rules table
    bslib::layout_columns(
      col_widths = c(12),
      bslib::card(
        bslib::card_header("Contract Validation Rules"),
        bslib::card_body(
          gt::gt_output(ns("contract_table")),
          shiny::tags$hr(),
          bslib::layout_columns(
            col_widths = c(8, 4),
            shiny::selectInput(
              ns("contract_rule_select"),
              label = "Download violations for rule:",
              choices = NULL
            ),
            shiny::tags$div(
              style = "padding-top: 1.7em;",
              shiny::downloadButton(ns("contract_download"), "Download (.xlsx)")
            )
          )
        )
      )
    ),

    # Personnel rules table
    bslib::layout_columns(
      col_widths = c(12),
      bslib::card(
        bslib::card_header("Personnel Validation Rules"),
        bslib::card_body(
          gt::gt_output(ns("personnel_table")),
          shiny::tags$hr(),
          bslib::layout_columns(
            col_widths = c(8, 4),
            shiny::selectInput(
              ns("personnel_rule_select"),
              label = "Download violations for rule:",
              choices = NULL
            ),
            shiny::tags$div(
              style = "padding-top: 1.7em;",
              shiny::downloadButton(
                ns("personnel_download"),
                "Download (.xlsx)"
              )
            )
          )
        )
      )
    )
  )
}

#' Validation Rules Server Module
#'
#' Server logic for the Validation Rules section, rendering pass/fail tables
#' for contract and personnel validation rules.
#'
#' @param id Character string. The module namespace ID.
#' @param personnel_validation List. The personnel validation results, including reports and violations.
#' @param contract_validation List. The contract validation results, including reports and violations.
#'
#' @return None. Called for side effects (renders Shiny outputs).
#'
#' @importFrom shiny moduleServer renderText observeEvent updateSelectInput downloadHandler req
#' @importFrom gt render_gt gt fmt_number fmt_markdown cols_align tab_style cell_fill opt_table_outline opt_row_striping
#' @importFrom writexl write_xlsx
#' @importFrom govhr validate_data
#'
#' @keywords internal
validation_server <- function(id, personnel_validation, contract_validation) {
  shiny::moduleServer(id, function(input, output, session) {
    contract_report <- contract_validation$report
    contract_violation <- contract_validation$violations
    personnel_report <- personnel_validation$report
    personnel_violation <- personnel_validation$violations

    # value boxes
    output$validation_personnel <- render_validation_box(
      personnel_report,
      "Personnel Validation Rate",
      "people-fill"
    )

    output$validation_contract <- render_validation_box(
      contract_report,
      "Contract Validation Rate",
      "file-text-fill"
    )

    # Helper: non-clickable badge HTML
    make_badge <- function(pass_rate, is_error) {
      if (is_error) {
        return(
          "<span style='background:#9e9e9e;color:white;padding:2px 8px;border-radius:4px;'>Does Not Apply</span>"
        )
      }
      if (pass_rate >= 100) {
        return(
          "<span style='background:#4caf50;color:white;padding:2px 8px;border-radius:4px;'>PASS</span>"
        )
      }
      if (pass_rate >= 80) {
        return(
          "<span style='background:#ff9800;color:white;padding:2px 8px;border-radius:4px;'>WARNING</span>"
        )
      }
      "<span style='background:#f44336;color:white;padding:2px 8px;border-radius:4px;'>FAIL</span>"
    }

    # Helper: build styled gt validation table
    make_validation_table <- function(df) {
      df$Status <- mapply(
        make_badge,
        pass_rate = df$`Pass Rate`,
        is_error = df$Errors
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
          locations = gt::cells_body(
            rows = `Pass Rate` < 100 & `Pass Rate` >= 80 & !Errors
          )
        ) |>
        gt::tab_style(
          style = gt::cell_fill(color = "#fce4e4"),
          locations = gt::cells_body(rows = `Pass Rate` < 80 & !Errors)
        ) |>
        gt::opt_table_outline() |>
        gt::opt_row_striping(row_striping = FALSE)
    }

    output$contract_table <- gt::render_gt(make_validation_table(
      contract_report
    ))
    output$personnel_table <- gt::render_gt(make_validation_table(
      personnel_report
    ))

    # Populate selectInputs with only rules that have violations
    failing_rules <- function(report) {
      report$Rule[
        !is.na(report$`Pass Rate`) & report$`Pass Rate` < 100 & !report$Errors
      ]
    }

    shiny::observe({
      rules <- failing_rules(contract_report)
      shiny::updateSelectInput(
        session,
        "contract_rule_select",
        choices = if (length(rules) > 0) rules else c("No failing rules" = ""),
        selected = if (length(rules) > 0) rules[1] else ""
      )
    })

    shiny::observe({
      rules <- failing_rules(personnel_report)
      shiny::updateSelectInput(
        session,
        "personnel_rule_select",
        choices = if (length(rules) > 0) rules else c("No failing rules" = ""),
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
        paste0(
          to_filename(shiny::req(input$contract_rule_select)),
          "_violations.xlsx"
        )
      },
      content = function(file) {
        rule <- shiny::req(input$contract_rule_select)
        dt <- contract_violation[[rule]]
        shiny::req(!is.null(dt), nrow(dt) > 0)
        writexl::write_xlsx(sanitise_df(dt), file)
      }
    )

    output$personnel_download <- shiny::downloadHandler(
      filename = function() {
        paste0(
          to_filename(shiny::req(input$personnel_rule_select)),
          "_violations.xlsx"
        )
      },
      content = function(file) {
        rule <- shiny::req(input$personnel_rule_select)
        dt <- personnel_violation[[rule]]
        shiny::req(!is.null(dt), nrow(dt) > 0)
        writexl::write_xlsx(sanitise_df(dt), file)
      }
    )
  })
}

run_validation_app <- function(
  est_data,
  personnel_data,
  contract_data,
  ...
) {
  theme <- bslib::bs_theme(
    bootswatch = "litera"
  )

  ui <- validation_ui("test")

  server <- function(input, output, session) {
    validation_server("test", personnel_data, contract_data)
  }

  shiny::shinyApp(ui, server, ...)
}
