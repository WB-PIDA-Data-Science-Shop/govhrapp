#############################################################################################
################################# INTERNAL HELPER FUNCTIONS ##################################
#############################################################################################

#' Add Export Menu to Highcharter Plots
#'
#' Helper function to add consistent export/download options to highcharter plots.
#'
#' @param hc A highcharter object.
#'
#' @return A highcharter object with export menu enabled.
#'
#' @importFrom highcharter hc_exporting
#'
#' @keywords internal
add_export_menu <- function(hc) {
  hc |>
    highcharter::hc_exporting(
      enabled = TRUE,
      buttons = list(
        contextButton = list(
          menuItems = c("downloadPNG", "downloadJPEG", "downloadPDF", "downloadSVG", "separator", "downloadCSV", "downloadXLS")
        )
      )
    )
}
