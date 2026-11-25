#' Run the analytics app
#'
#' Launches the govhr analytics app, which shows the size, movements and cost
#' of the public sector workforce, with tabs for an overview, the workforce and
#' the wage bill.
#'
#' @param workforce_data Personnel data, one row per person and reference date:
#'   a data frame or a database table (`tbl_dbi`). Must contain `ref_date`,
#'   `personnel_id`, `employment_status` (`"active"` or `"pensioner"`),
#'   `birth_date`, `gender` and `educat7`. Other categorical columns listed in
#'   `govhr::dictionary` become grouping options.
#' @param wagebill_data Contract data, one row per contract and reference date:
#'   a data frame or a database table (`tbl_dbi`). Must contain `ref_date`,
#'   `personnel_id`, `employment_status`, `birth_date`, `contract_type` and
#'   `gross_salary_lcu`. Other pay columns listed in `govhr::dictionary`
#'   become pay measure options.
#' @param cache List of precomputed summaries from [build_analytics_cache()].
#'   Default builds one from `workforce_data` and `wagebill_data`.
#' @param secure Logical. Require users to log in. Default `FALSE`.
#' @param credentials_path Character. Path to the SQLite database of user
#'   credentials, used when `secure = TRUE`. The default is relative to the
#'   working directory and only exists when running from the govhrapp source,
#'   so pass your own path otherwise.
#' @param ... Additional arguments passed to [shiny::shinyApp()].
#'
#' @returns A Shiny app object.
#'
#' @details
#' Both inputs are checked for their required columns before the app starts,
#' and an error names any that are missing.
#'
#' With database tables, every summary is computed in the database and only
#' its result is brought into memory, as are the date range and the distinct
#' values offered by the sidebar pickers.
#'
#' @seealso [run_analytics_demo()] to try the app on sample data.
#'
#' @examples
#' \dontrun{
#' run_govhrapp_analytics(workforce_data, wagebill_data)
#' }
#'
#' @importFrom shiny shinyApp addResourcePath useBusyIndicators
#' @importFrom bslib page_navbar nav_panel nav_spacer bs_theme bs_add_rules navbar_options font_google
#' @importFrom ggplot2 theme_set theme_minimal theme element_text update_geom_defaults
#' @importFrom thematic thematic_shiny
#' @importFrom lubridate year
#' @importFrom scales label_number cut_short_scale
#' @importFrom tidyr complete 
#' @export
run_govhrapp_analytics <- function(
  workforce_data,
  wagebill_data,
  cache = build_analytics_cache(workforce_data, wagebill_data),
  secure = FALSE,
  credentials_path = "inst/app/analytics/credentials.sqlite",
  ...
) {
  # checked before `cache` is first used, since its default builds from the
  # same inputs
  check_analytics_data(workforce_data, wagebill_data)

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

  ui <- bslib::page_navbar(
    fillable = FALSE,

    header = shiny::useBusyIndicators(),

    navbar_options = navbar_options(
      underline = TRUE
    ),

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

    padding = "10px",

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
                  "markdown/analytics_home.md",
                  package = "govhrapp"
                ))
              )
            )
          )
        ),
        shiny::div()
      )
    ),

    # panel 2: overview
    bslib::nav_panel(
      "Overview",
      icon = shiny::icon("gauge"),
      overview_ui("overview")
    ),

    # panel 3: workforce planning
    bslib::nav_panel(
      "Workforce",
      icon = shiny::icon("person-walking"),
      workforce_ui("workforce", workforce_data, wagebill_data)
    ),

    # panel 4: wage bill
    bslib::nav_panel(
      "Wage bill",
      icon = shiny::icon("money-bill"),
      wagebill_ui("wagebill", wagebill_data)
    ),

    # panel 5: code
    nav_menu(
      title = "Code",
      icon = shiny::icon("github"),
      bslib::nav_item(
        shiny::tags$a(
          "govhrapp",
          href = "https://github.com/WB-PIDA-Data-Science-Shop/govhrapp",
          target = "_blank"
        )
      ),
      bslib::nav_item(
        shiny::tags$a(
          "govhr",
          href = "https://github.com/WB-PIDA-Data-Science-Shop/govhr",
          target = "_blank"
        )
      )
    )
  )

  server <- function(input, output, session) {
    overview_server("overview", cache = cache)
    wagebill_server(
      "wagebill",
      wagebill_data,
      cache = cache
    )
    workforce_server(
      "workforce",
      workforce_data,
      wagebill_data,
      cache = cache
    )
  }

  shiny_app_with_login(ui, server, secure, credentials_path, ...)
}