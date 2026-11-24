#' Run the quality check app
#'
#' Launches the govhr quality check app, which shows how complete and
#' consistent the establishment, personnel and contract data are, and which
#' validation rules they pass.
#'
#' @param est_data Establishment data, one row per establishment and reference
#'   date: a data frame.
#' @param personnel_data Personnel data, one row per person and reference
#'   date: a data frame.
#' @param contract_data Contract data, one row per contract and reference date:
#'   a data frame.
#' @param personnel_validation List. Validation results for `personnel_data`,
#'   as returned by [govhr::validate_data()].
#' @param contract_validation List. Validation results for `contract_data`, as
#'   returned by [govhr::validate_data()].
#' @param ... Additional arguments passed to [shiny::shinyApp()].
#'
#' @returns A Shiny app object.
#'
#' @seealso [run_qcheck_demo()] to try the app on sample data.
#'
#' @examples
#' \dontrun{
#' run_govhrapp_qcheck(
#'   est_data,
#'   personnel_data,
#'   contract_data,
#'   personnel_validation = govhr::validate_data(
#'     personnel_data,
#'     govhr::personnel_rules
#'   ),
#'   contract_validation = govhr::validate_data(
#'     contract_data,
#'     govhr::contract_rules
#'   )
#' )
#' }
#'
#' @importFrom shiny shinyApp addResourcePath
#' @importFrom bslib page_navbar nav_panel nav_spacer bs_theme bs_add_rules navbar_options font_google
#' @importFrom ggplot2 theme_set theme_minimal theme element_text update_geom_defaults
#' @importFrom thematic thematic_shiny
#' @importFrom lubridate year
#' @importFrom scales label_number cut_short_scale
#' @export
run_govhrapp_qcheck <- function(
  est_data,
  personnel_data,
  contract_data,
  personnel_validation,
  contract_validation,
  ...
) {
  # add path to visual assets (image and css)
  shiny::addResourcePath("assets", system.file("www", package = "govhrapp"))

  # ensure ggplot2 and plotly inherit bslib themes
  ggplot2::theme_set(
    ggplot2::theme_minimal(base_size = 14) +
      ggplot2::theme(
        axis.text = ggplot2::element_text(size = 10.5)
      )
  )

  thematic::thematic_shiny(
    font = "auto",
    accent = "#C34729",
    sequential = "#C34729"
  )

  ggplot2::update_geom_defaults("point", list(colour = "#C34729"))
  ggplot2::update_geom_defaults("line", list(colour = "#C34729"))
  ggplot2::update_geom_defaults("col", list(fill = "#C34729"))

  cache <- build_qcheck_cache(est_data, personnel_data, contract_data)

  ui <- bslib::page_navbar(
    fillable = FALSE,

    header = shiny::useBusyIndicators(),

    # set theme
    theme = bslib::bs_theme(
      bootswatch = "litera",
      base_font = font_google("Figtree", local = FALSE),
      code_font = font_google("Source Sans Pro", local = FALSE),
      heading_font = font_google("Libre Baskerville", local = FALSE),
      navbar_bg = "#ffffff"
    ) |>
      bslib::bs_add_rules(
        readLines(system.file("www/styles.css", package = "govhrapp"))
      ),

    navbar_options = navbar_options(
      underline = TRUE
    ),

    padding = "20px",

    # cache

    # custom CSS
    # shiny::tags$head(shiny::includeCSS("www/styles.css")),

    # panel 1: home
    bslib::nav_panel(
      "Home",
      icon = shiny::icon("home"),

      # content
      bslib::layout_columns(
        col_widths = bslib::breakpoints(
          sm = 12,
          md = c(1, 10, 1),
          lg = c(1.5, 9, 1.5)
        ),
        shiny::div(),
        bslib::card(
          bslib::card_header(
            shiny::tags$img(
              src = "assets/govhr_logo.png",
              style = "max-width: 1200px; display: block; margin-left: auto; margin-right: auto;",
              width = "80%"
            )
          ),
          bslib::card_body(
            shiny::tags$div(
              style = "max-width: 800px; margin: 0 auto; padding: 2rem 3rem;",
              shiny::tags$h3("Welcome to govhr."),
              shiny::markdown(
                readLines(system.file(
                  "markdown/qcheck_home.md",
                  package = "govhrapp"
                ))
              )
            )
          )
        ),
        shiny::div()
      )
    ),

    # panel 2: coverage
    bslib::nav_panel(
      "Coverage",
      icon = bsicons::bs_icon("clipboard-data-fill"),

      # content
      coverage_ui("coverage", est_data, personnel_data, contract_data)
    ),

    bslib::nav_panel(
      "Consistency",
      icon = bsicons::bs_icon("check-circle"),

      # content
      consistency_ui("consistency", est_data, personnel_data, contract_data)
    ),

    bslib::nav_panel(
      "Validation",
      icon = bsicons::bs_icon("bookmark-check-fill"),

      # content
      validation_ui("validation")
    )
  )

  server <- function(input, output, session) {
    coverage_server(
      "coverage",
      est_data,
      personnel_data,
      contract_data,
      cache = cache[["coverage"]]
    )
    consistency_server(
      "consistency",
      est_data,
      personnel_data,
      contract_data,
      cache = cache[["consistency"]]
    )
    validation_server("validation", personnel_validation, contract_validation)
  }

  shiny::shinyApp(ui, server, ...)
}

#' Build the quality check cache
#'
#' Precomputes the coverage and record consistency of each table by reference
#' date, which the coverage and consistency tabs show before the user applies
#' a filter.
#'
#' @inheritParams run_govhrapp_qcheck
#'
#' @returns A list with `coverage` and `consistency` elements, each a list of
#'   data frames named `est`, `personnel` and `contract`.
#'
#' @importFrom govhr compute_coverage compute_record_consistency
#' @importFrom purrr imap map
#' @keywords internal
build_qcheck_cache <- function(est_data, personnel_data, contract_data) {
  tables <- list(
    est = est_data,
    personnel = personnel_data,
    contract = contract_data
  )

  id_cols <- c(
    est = "est_id",
    personnel = "personnel_id",
    contract = "contract_id"
  )

  coverage <- tables |>
    purrr::map(
      \(table) {
        govhr::compute_coverage(
          table,
          include_ref_date = TRUE,
          aggregate = TRUE
        )
      }
    )

  consistency <- tables |>
    purrr::imap(
      \(table, name) {
        govhr::compute_record_consistency(
          table,
          id_col = id_cols[[name]],
          group_cols = "ref_date"
        )
      }
    ) |>
    collect_cache()

  list(
    coverage = coverage,
    consistency = consistency
  )
}
