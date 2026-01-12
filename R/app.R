#' Run the govhr Shiny Dashboard Application
#'
#' Launches an interactive Shiny dashboard for govhr data visualization and analysis.
#'
#' @param ... Additional arguments passed to \code{\link[shiny]{shinyApp}}.
#'
#' @return A Shiny app object.
#'
#' @examples
#' \dontrun{
#' run_govhr_app()
#' }
#'
#' @importFrom shiny addResourcePath shinyApp icon tags includeCSS
#' @import bslib
#' @importFrom icon icon
run_govhr_app <- function(...) {
  # add path to visual assets (image and css)
  shiny::addResourcePath("assets", "www")

  ui <- bslib::page_navbar(
    title = "govhr dashboard",
    

    # set theme
    theme = bslib::bs_theme(
      bootswatch = "litera",
      base_font = font_google("Source Sans Pro"),
      code_font = font_google("Source Sans Pro"),
      heading_font = font_google("Fira Sans"),
      navbar_bg = "#FFFFFF"
    ) |> 
      bslib::bs_add_rules(
        readLines("www/styles.css")
      ),

    navbar_options = navbar_options(
      position = "fixed-top",
      collapse_below = "xl",
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
      div(
        style = "padding: 40px 20px;",
        bslib::card(
          bslib::card_header(
            shiny::tags$img(src = "assets/govhr_logo.png", width = "80%")
          ),
          bslib::card_body(
            shiny::tags$br(),
            shiny::tags$h3("Welcome to govhr."),
            shiny::markdown(
              "
                This dashboard provides a standard set of indicators and visualizations on HRMIS analytics, building on the World Bank's [Public Sector and Employment Assessment Framework](https://documents1.worldbank.org/curated/en/324801640074379484/pdf/Public-Sector-Employment-and-Compensation-An-Assessment-Framework.pdf). The objective of this assessment is to conduct public sector employment and compensation assessments that can help develop evidence-based reforms.

                The assessment framework consists of two parts. The first part assesses both the institutional and behavioral aspects of employment and compensation practices. It comprises six dimensions:

                1. Wage bill planning.
                2. Wage bill controls.
                3. Employment levels and distribution of the public sector workforce.
                4. Wage competitiveness of the public sector.
                5. Wage equity.
                6. Wage incentives.

                The second part assesses the impact of these employment and compensation practices on:

                1. Fiscal sustainability.
                2. Public sector productivity.
                3. Labor allocation between the public and private sectors.
              "
            )
          )
        )
      )
    ),

    # panel 2: wage bill
    bslib::nav_panel(
      "Wage Bill",
      icon = shiny::icon("money"),

      # content
      div(
        style = "padding: 40px 20px;",
        bslib::card(
          bslib::card_header(
            "Wage Bill"
          ),
          bslib::card_body(
            shiny::markdown(
              "
              The wage bill represents a large and less flexible component of government expenditures with significant future liabilities. Globally, and noting the difficulties with cross-country comparisons, the wage bill represents approximately 30 percent of government expenditures,
              with significant variation around this average. This section presents some indicators that can help governments better understand and manage their wage bill.

              Guidance questions for the assessment:

              1. What are the main stylized facts about the wage bill: its actual size, evolution, and decomposition into employment
              and wages?
              2. Is wage bill management anchored in comprehensive, accurate, and timely data?
                  a. Does the data cover all government employees paid from the budget and from extra-budgetary funds?
                  b. Is the data derived from integrated payroll and human resource systems that are updated every pay cycle?
                  c. Are budget classifications transparent so that all payroll expenditures are accurately recorded?
              3. Is wage bill planning integrated in a broader medium-term fiscal framework? Are forward budget estimates derived
              from models of future wage bill commitments?
              4. How does the institutional regime for salary setting impact wage bill planning? Are there particular challenges
              for subnational wage bill planning due to mismatches in staffing decisions and funding between central and
              subnational governments?
              5. Are staffing increases informed by needs and medium-term strategic planning, or primarily by short-term
              political considerations?
              6. Is there any public debate or public disclosure around wage bill policies?
              "
            )
          )
        )
      )
      )
    )

  server <- function(input, output, session) {}

  shiny::shinyApp(ui, server, ...)
}