# shared by the share computation, the plot, its height and the card's help
# text, so changing one of these settings reaches every place that relies on it
composition_residual <- c(other = "Other", missing = "Missing")
composition_max_categories <- 6
composition_panel_cols <- 3

#' "Shares by" breakdown input
#'
#' @param id Character. Module namespace ID.
#' @param .data Data frame whose columns bound the available choices.
#'
#' @returns A Shiny select input.
#'
#' @importFrom shiny NS selectInput
#' @keywords internal
breakdown_filter_ui <- function(id, .data) {
  shiny::selectInput(
    shiny::NS(id, "breakdown_filter"),
    "Shares by",
    choices = identify_choices(.data),
    selected = "none"
  )
}

#' Read the dictionary label of a variable
#'
#' @param variable_id Character. A `govhr::dictionary` variable ID.
#'
#' @returns The variable's name, or `variable_id` itself when the dictionary
#'   does not list it.
#'
#' @keywords internal
variable_label <- function(variable_id) {
  dictionary <- govhr::dictionary

  label <- dictionary[["variable_name"]][
    dictionary[["variable_id"]] == variable_id
  ]

  if (length(label) == 0) variable_id else label[[1]]
}

#' Compute the composition of a measure by category
#'
#' Splits an additive measure into the share of each breakdown category,
#' within each reference date and group, so the shares of a date and group add
#' up to 100.
#'
#' @param composition Data frame with `ref_date`, `value_col`, `breakdown_col`
#'   and, when grouped, `group_col`, as returned by a govhr compute function
#'   called with `group_cols = c(group_col, breakdown_col)`.
#' @param value_col Character. Column holding the additive measure, such as
#'   `"headcount"` or `"wagebill"`.
#' @param group_col Character. Grouping column, or `"ref_date"` for no
#'   grouping.
#' @param breakdown_col Character. Column whose categories split the measure.
#' @param max_categories Integer. Number of largest categories shown on their
#'   own; the rest are combined into `"Other"`. Default
#'   `composition_max_categories`.
#'
#' @returns A data frame with `ref_date`, the grouping column when grouped,
#'   `category` (a factor ordered from the largest category to `"Other"` and
#'   `"Missing"`), `value` and `share` in percent.
#'
#' @importFrom dplyr across all_of arrange coalesce cross_join desc distinct filter
#'   if_else left_join mutate pull summarise tibble
#' @keywords internal
compute_composition_share <- function(
  composition,
  value_col,
  group_col,
  breakdown_col,
  max_categories = composition_max_categories
) {
  series_cols <- c("ref_date", group_col_to_null(group_col))

  # compute_wagebill() gives a series absent on a date an NA total, and a
  # single NA would turn every share of its date and group into NA. records
  # missing the breakdown get a band of their own, so every stack adds up to
  # the whole headcount or wage bill rather than to a shrinking total
  labeled <- composition |>
    dplyr::filter(!is.na(.data[[value_col]])) |>
    dplyr::mutate(
      category = dplyr::coalesce(
        as.character(.data[[breakdown_col]]),
        composition_residual[["missing"]]
      ),
      value = .data[[value_col]]
    )

  # past a handful of bands, adjacent colours become hard to tell apart, so
  # the smallest categories over the whole period are combined
  ranked <- labeled |>
    dplyr::filter(.data[["category"]] != composition_residual[["missing"]]) |>
    dplyr::summarise(total = sum(.data[["value"]]), .by = "category") |>
    # ties are broken by name, since a database returns rows in no fixed
    # order and the order sets each category's colour
    dplyr::arrange(dplyr::desc(.data[["total"]]), .data[["category"]]) |>
    dplyr::pull(.data[["category"]])

  kept <- ranked[seq_len(min(length(ranked), max_categories))]

  summarized <- labeled |>
    dplyr::mutate(
      category = dplyr::if_else(
        .data[["category"]] %in% c(kept, composition_residual[["missing"]]),
        .data[["category"]],
        composition_residual[["other"]]
      )
    ) |>
    dplyr::summarise(
      value = sum(.data[["value"]]),
      .by = dplyr::all_of(c(series_cols, "category"))
    )

  category_levels <- intersect(
    c(kept, unname(composition_residual)),
    summarized[["category"]]
  )

  # a stacked area needs every category on every date of a panel; a category
  # absent there is a zero share, not a gap that would break the stack
  summarized |>
    dplyr::distinct(dplyr::across(dplyr::all_of(series_cols))) |>
    dplyr::cross_join(dplyr::tibble(category = category_levels)) |>
    dplyr::left_join(summarized, by = c(series_cols, "category")) |>
    dplyr::mutate(
      value = dplyr::coalesce(.data[["value"]], 0),
      share = .data[["value"]] / sum(.data[["value"]]) * 100,
      .by = dplyr::all_of(series_cols)
    ) |>
    dplyr::mutate(
      category = factor(.data[["category"]], levels = category_levels)
    )
}

#' Colours for composition categories
#'
#' @param categories Character vector of category levels, as in the `category`
#'   factor of [compute_composition_share()].
#'
#' @returns A character vector of colours named by category.
#'
#' @importFrom scales pal_seq_gradient
#' @keywords internal
composition_palette <- function(categories) {
  residual <- c("#9E9E9E", "#D9D9D9")
  names(residual) <- composition_residual[c("other", "missing")]
  named <- setdiff(categories, names(residual))

  # the package's orange gradient, so the bands read as one family with the
  # other charts; the residual bands are grey to recede behind them
  colours <- scales::pal_seq_gradient("#C34729", "#F5C6A0")(
    seq(0, 1, length.out = length(named))
  )
  names(colours) <- named

  c(colours, residual[intersect(names(residual), categories)])
}

#' Plot a composition over time
#'
#' Draws a 100% stacked area of each category's share over `ref_date`, with
#' one panel per group when grouped.
#'
#' @param data Data frame as returned by [compute_composition_share()].
#' @param group_col Character. Grouping column, or `"ref_date"` for no
#'   grouping.
#' @param y_label Character. y-axis label.
#' @param fill_label Character. Legend title.
#'
#' @returns A ggplot2 object.
#'
#' @importFrom ggplot2 aes facet_wrap geom_area ggplot label_wrap_gen labs
#'   scale_fill_manual scale_y_continuous vars
#' @keywords internal
plot_composition <- function(data, group_col, y_label, fill_label) {
  plot <- data |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = .data[["ref_date"]],
        y = .data[["share"]],
        fill = .data[["category"]],
        # `text` is discrete, so without an explicit group ggplot would split
        # each band into one-point pieces
        group = .data[["category"]],
        text = paste0(
          .data[["category"]],
          ": ",
          round(.data[["share"]], 1),
          "%"
        )
      )
    ) +
    ggplot2::geom_area() +
    ggplot2::scale_fill_manual(
      values = composition_palette(levels(data[["category"]]))
    ) +
    ggplot2::scale_y_continuous(breaks = c(0, 50, 100), expand = c(0, 0)) +
    ggplot2::labs(x = "Time", y = y_label, fill = fill_label)

  if (group_col == "ref_date") {
    return(plot)
  }

  plot +
    ggplot2::facet_wrap(
      ggplot2::vars(.data[[group_col]]),
      ncol = composition_panel_cols,
      labeller = ggplot2::label_wrap_gen(width = 30)
    )
}

#' Height of a composition plot
#'
#' @inheritParams plot_composition
#'
#' @returns Numeric. Plot height in pixels, enough for each row of panels to
#'   stay legible.
#'
#' @importFrom dplyr n_distinct
#' @keywords internal
composition_plot_height <- function(data, group_col) {
  panel_rows <- if (group_col == "ref_date") {
    1
  } else {
    ceiling(dplyr::n_distinct(data[[group_col]]) / composition_panel_cols)
  }

  max(350, 220 * panel_rows)
}

#' Composition card UI
#'
#' A placeholder for the "of which" card, which the server renders only once
#' a breakdown is applied, so the panel is unchanged until one is.
#'
#' @param id Character. Module namespace ID.
#'
#' @returns A Shiny UI output.
#'
#' @importFrom shiny NS uiOutput
#' @keywords internal
composition_ui <- function(id) {
  shiny::uiOutput(shiny::NS(id, "card"))
}

#' Composition card server
#'
#' Splits an additive measure by the applied "of which" breakdown and renders
#' every category's share over time, one panel per group. Only additive
#' measures belong here: an average or a ratio split by category does not add
#' back up to the whole.
#'
#' @param id Character. Module namespace ID.
#' @param filtered Function returning the panel's filtered data, restricted to
#'   the records the measure covers.
#' @param group_col,breakdown_col,apply Functions returning the panel's group
#'   selection, breakdown selection and apply button value.
#' @param measure Function returning the measure to split, as a list of
#'   `compute` (a function taking the data and `group_cols` and returning the
#'   measure by group and `ref_date`, such as [govhr::compute_headcount()]),
#'   `value_col` (the column of `compute`'s output holding the measure) and
#'   `label` (its lower-case name, for titles and axes). It returns `NULL` when
#'   the panel's selected measure is not additive, such as a rate, and the card
#'   is then not shown.
#'
#' @returns Called for its side effect of rendering the module's outputs.
#'
#' @import shiny
#' @importFrom bsicons bs_icon
#' @importFrom bslib card card_body card_header popover
#' @importFrom dplyr collect
#' @importFrom plotly ggplotly plotlyOutput renderPlotly
#' @keywords internal
composition_server <- function(
  id,
  filtered,
  group_col,
  breakdown_col,
  apply,
  measure
) {
  shiny::moduleServer(id, function(input, output, session) {
    applied <- shiny::reactive({
      group <- group_col()
      breakdown <- breakdown_col()
      spec <- measure()

      if (is.null(breakdown) || breakdown == "none" || is.null(spec)) {
        return(NULL)
      }

      same_as_group <- breakdown == group
      compute <- spec[["compute"]]

      shares <- if (!same_as_group) {
        filtered() |>
          compute(group_cols = c(group_col_to_null(group), breakdown)) |>
          dplyr::collect() |>
          compute_composition_share(
            value_col = spec[["value_col"]],
            group_col = group,
            breakdown_col = breakdown
          )
      }

      list(
        group_col = group,
        breakdown_col = breakdown,
        label = spec[["label"]],
        same_as_group = same_as_group,
        shares = shares
      )
    }) |>
      shiny::bindEvent(apply(), ignoreNULL = FALSE)

    output$card <- shiny::renderUI({
      selection <- shiny::req(applied())
      measure_label <- selection[["label"]]

      body <- if (selection[["same_as_group"]]) {
        shiny::p(
          "Choose a \"Shares by\" breakdown different from the group.",
          class = "text-muted"
        )
      } else {
        plotly::plotlyOutput(session$ns("composition"))
      }

      bslib::card(
        full_screen = TRUE,
        fillable = FALSE,
        bslib::card_header(
          paste0(
            "Shares of ",
            measure_label,
            " by ",
            tolower(variable_label(selection[["breakdown_col"]]))
          ),
          bslib::popover(
            bsicons::bs_icon("info-circle-fill"),
            paste0(
              "Share of ",
              measure_label,
              " in each category on each reference date, one panel per ",
              "subgroup. Records missing the breakdown show as Missing. ",
              "Beyond ",
              composition_max_categories,
              " categories, the smallest are combined into Other."
            ),
            placement = "left"
          ),
          class = "d-flex justify-content-between"
        ),
        bslib::card_body(body)
      )
    })

    output$composition <- plotly::renderPlotly({
      selection <- shiny::req(applied())
      shares <- shiny::req(selection[["shares"]])
      group <- selection[["group_col"]]

      plotly::ggplotly(
        plot_composition(
          shares,
          group_col = group,
          y_label = paste0("Share of ", selection[["label"]], " (%)"),
          fill_label = variable_label(selection[["breakdown_col"]])
        ),
        tooltip = c("x", "text"),
        height = composition_plot_height(shares, group)
      )
    })
  })
}
