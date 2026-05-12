#############################################################################################
################################# VALIDATION MODULE UI #######################################
#############################################################################################

#' Validation Rules UI Module
#'
#' UI components for the Validation Rules section of the quality control dashboard.
#'
#' @param id Character string. The module namespace ID.
#'
#' @return A Shiny UI object containing validation rule cards.
#'
#' @importFrom shiny NS tagList textOutput icon tags
#' @importFrom bslib layout_columns card card_header card_body value_box
#' @importFrom gt gt_output
#'
#' @keywords internal
validation_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(

    # JS handler: receives base64 xlsx from server and triggers browser download
    shiny::tags$script(shiny::HTML("
      Shiny.addCustomMessageHandler('download_blob', function(msg) {
        var bytes = Uint8Array.from(atob(msg.b64), function(c){ return c.charCodeAt(0); });
        var blob  = new Blob([bytes], {type: 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet'});
        var url   = URL.createObjectURL(blob);
        var a     = document.createElement('a');
        a.href    = url;
        a.download = msg.filename;
        document.body.appendChild(a);
        a.click();
        document.body.removeChild(a);
        URL.revokeObjectURL(url);
      });
    ")),
    # Summary value boxes
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::value_box(
        title    = "Contract Rules Pass Rate",
        value    = shiny::textOutput(ns("contract_pass_rate")),
        showcase = shiny::icon("file-contract"),
        theme    = "primary"
      ),
      bslib::value_box(
        title    = "Personnel Rules Pass Rate",
        value    = shiny::textOutput(ns("personnel_pass_rate")),
        showcase = shiny::icon("users"),
        theme    = "primary"
      )
    ),

    # Contract rules table
    bslib::layout_columns(
      col_widths = c(12),
      bslib::card(
        bslib::card_header("Contract Validation Rules"),
        bslib::card_body(
          shiny::p(
            "Click a ",
            shiny::tags$span("WARNING", style = "background:#ff9800;color:white;padding:1px 6px;border-radius:4px;"),
            " or ",
            shiny::tags$span("FAIL", style = "background:#f44336;color:white;padding:1px 6px;border-radius:4px;"),
            " badge to download violations as Excel.",
            style = "font-size:0.85em;color:#555;margin-bottom:6px;"
          ),
          gt::gt_output(ns("contract_table"))
        )
      )
    ),

    # Personnel rules table
    bslib::layout_columns(
      col_widths = c(12),
      bslib::card(
        bslib::card_header("Personnel Validation Rules"),
        bslib::card_body(
          shiny::p(
            "Click a ",
            shiny::tags$span("WARNING", style = "background:#ff9800;color:white;padding:1px 6px;border-radius:4px;"),
            " or ",
            shiny::tags$span("FAIL", style = "background:#f44336;color:white;padding:1px 6px;border-radius:4px;"),
            " badge to download violations as Excel.",
            style = "font-size:0.85em;color:#555;margin-bottom:6px;"
          ),
          gt::gt_output(ns("personnel_table"))
        )
      )
    )
  )
}
