#' Run the Quality Assessment Shiny Application
#'
#' Launches an interactive Shiny dashboard for govhr data visualization and analysis.
#'
#' @param dynamicqc_obj A dynamicqc object created by \code{\link{compute_dynamicqc}}.
#' @param ... Additional arguments passed to \code{\link[shiny]{shinyApp}}.
#'
#' @return A Shiny app object.
#'
#' @examples
#' \dontrun{
#' qc_data <- compute_dynamicqc(
#'   contract_dt = govhr::bra_hrmis_contract,
#'   personnel_dt = govhr::bra_hrmis_personnel,
#'   est_dt = govhr::bra_hrmis_est
#' )
#' run_qcheckapp(dynamicqc_obj = qc_data)
#' }
#'
#' @import shiny bslib ggplot2 data.table
#' @export
run_qcheckapp <- function(dynamicqc_obj, ...) {
  
  # Validate input
  if (!inherits(dynamicqc_obj, "dynamicqc")) {
    stop("Input must be a dynamicqc object created by compute_dynamicqc()")
  }
  
  # add path to visual assets (image and css)
  shiny::addResourcePath("assets", "inst/www")
  thematic::thematic_shiny(font = "auto")

  ui <- bslib::page_navbar(
    title = "govhr quality control",
    fillable = FALSE,

    # set theme
    theme = bslib::bs_theme(
      bootswatch = "litera",
      base_font = font_google("Source Sans Pro"),
      code_font = font_google("Source Sans Pro"),
      heading_font = font_google("Fira Sans"),
      navbar_bg = "#FFFFFF"
    ) |>
      bslib::bs_add_rules(
        readLines("inst/www/styles.css")
      ),

    navbar_options = navbar_options(
      underline = TRUE
    ),

    padding = "20px",

    # custom CSS
    # shiny::tags$head(shiny::includeCSS("www/styles.css")),

    # panel 1: home
    bslib::nav_panel(
      "Home",
      icon = shiny::icon("home"),

      # content
      bslib::card(
        bslib::card_header(
          shiny::tags$img(src = "assets/govhr_logo.png", width = "80%")
        ),
        bslib::card_body(
          shiny::tags$br(),
          shiny::tags$h3("Welcome to govhr: Quality Control Suite."),
          shiny::markdown(
            readLines("inst/markdown/qcheck_home.md")
          )          
        )
      )
    ),
    
    # panel 2: Data Basics
    bslib::nav_panel(
      "Data Basics",
      icon = shiny::icon("database"),
      databasics_ui("databasics")
    ),
    
    # panel 3: Missingness
    bslib::nav_panel(
      "Missingness",
      icon = shiny::icon("magnifying-glass-chart"),
      missingness_ui("missingness")
    ),
    
    # panel 4: Volatility
    bslib::nav_panel(
      "Volatility",
      icon = shiny::icon("chart-line"),
      volatility_ui("volatility")
    )

 )

  server <- function(input, output, session){
    databasics_server("databasics", dynamicqc_obj)
    missingness_server("missingness", dynamicqc_obj)
    volatility_server("volatility", dynamicqc_obj)
  }

  shiny::shinyApp(ui, server, ...)
}
