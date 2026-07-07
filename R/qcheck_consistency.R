#' Consistency UI Module
#'
#' UI components for the consistency section of the quality control dashboard.
#'
#' @param id Character string. The module namespace ID.
#' @param est_data Data frame. The establishment-level data.
#' @param personnel_data Data frame. The personnel-level data.
#' @param contract_data Data frame. The contract-level data.
#'
#' @return A Shiny UI object containing coverage analysis cards.
#'
#' @import shiny
#' @import bslib
#'
#' @keywords internal
consistency_ui <- function(id, est_data, personnel_data, contract_data) {
  # coverage per module
  value_boxes <- list(
    uiOutput(
      NS(id, "consistency_est")
    ),
    uiOutput(
      NS(id, "consistency_contract")
    ),
    uiOutput(
      NS(id, "consistency_personnel")
    )
  )

  bslib::layout_columns(
    fillable = FALSE,
    col_widths = 12,

    # 1. value boxes for coverage metrics
    bslib::card(
      bslib::card_header(
        "Data Coverage",
        bslib::tooltip(
            bsicons::bs_icon("info-circle"),
            "Consistency, by module. Computed as the global average of consistency, at the record and value levels, in each module."
        )
      ),
      layout_column_wrap(
        fill = FALSE,
        !!!value_boxes
      )
    )
    
    # # 2. coverage by module
    # bslib::navset_underline(
    #   bslib::nav_panel(
    #     title = "Establishment",
    #     coverage_panel_ui(NS(id, "est"), est_data)
    #   ),
    #   bslib::nav_panel(
    #     title = "Personnel",
    #     coverage_panel_ui(NS(id, "personnel"), personnel_data)
    #   ),
    #   bslib::nav_panel(
    #     title = "Contract",
    #     coverage_panel_ui(NS(id, "contract"), contract_data)
    #   )
    # )
  )
}


#' Consistency Server Module
#'
#' Server logic for the consistency section, processing and visualizing whether data is consistent.
#'
#' @param id Character string. The module namespace ID.
#' @param est_data Data frame. The establishment-level data.
#' @param personnel_data Data frame. The personnel-level data.
#' @param contract_data Data frame. The contract-level data.
#'
#' @return None. Called for side effects (renders Shiny outputs).
#'
#' @import shiny
#'
#' @keywords internal
consistency_server <- function(id, est_data, personnel_data, contract_data) {
  shiny::moduleServer(id, function(input, output, session) {
    # 1. value boxes for coverage metrics
    output$consistency_est <- render_consistency_box(
      est_data,
      id_col = "est_id",
      value_cols = "est_name_native",
      "Establishments",
      "building"
    )

    output$consistency_personnel <- render_consistency_box(
      personnel_data,
      id_col = "personnel_id",
      value_cols = c("birth_date"),
      "Personnel",
      "users"
    )

    output$consistency_contract <- render_consistency_box(
      contract_data,
      id_col = "contract_id",
      value_cols = c("start_date", "contract_type"),
      "Contracts",
      "file-contract"
    )

    # # per-dataset panel sub-modules
    # consistency_panel_server("est",       est_data)
    # consistency_panel_server("personnel", personnel_data)
    # consistency_panel_server("contract",  contract_data)
  })
}

run_consistency_app <- function(
  est_data,
  personnel_data,
  contract_data,
  ...
) {
  ui <- consistency_ui("test", est_data, personnel_data, contract_data)

  server <- function(input, output, session) {
    consistency_server("test", est_data, personnel_data, contract_data)
  }

  shiny::shinyApp(ui, server, ...)
}
