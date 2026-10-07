#' Compute retirements
#'
#' Counts, for each reference date, how many people are active and how many of
#' them leave active status by the next date into a pension, i.e. whose next
#' status after leaving is pensioner. Mirrors [govhr::compute_movement()], so
#' the two can be read side by side: retirements are a subset of its
#' separations, and both rates share the same denominator.
#'
#' @param data Data frame or remote database table (`tbl_dbi`) with one row
#'   per person-record. Must contain `personnel_id`, `ref_date` and the column
#'   named in `status_col`.
#' @param group_cols Character vector of columns to group by, such as
#'   `"est_id"`, or `NULL` (default) for the whole workforce. Must not include
#'   `ref_date`.
#' @param status_col Character. Column holding employment status, with active
#'   personnel recorded as `"active"` and retirees as `"pensioner"`. Default
#'   `"employment_status"`.
#' @param ... Arguments passed to methods.
#'
#' @returns A table with one row per `ref_date` and group, containing:
#' \describe{
#'   \item{headcount}{Number of active people.}
#'   \item{retirements}{Active people who are no longer active on the next
#'     date and are next recorded as pensioners. `NA` on the last date, which
#'     has nothing to compare with.}
#'   \item{retirement_rate}{`retirements` divided by `headcount`.}
#' }
#' A data.table for data frame input; a lazy table for `tbl_dbi` input (use
#' [dplyr::collect()] to bring it into memory).
#'
#' @details
#' The next date is the neighbouring date found in the data, so the dates do
#' not need to be evenly spaced. People are counted once per date, even if they
#' hold several contracts. With `group_cols`, each person is counted in the
#' group they belong to on that date.
#'
#' Pension registration can lag the exit, so the pensioner record may appear
#' at any later date, not only the next one. A retirement is dated by the exit,
#' not by the registration. A person who returns to active work before any
#' pensioner record counts as a separation, not a retirement, at the earlier
#' exit. There is no limit on the lag, so an exit followed years later by a
#' deferred pension also counts as a retirement.
#'
#' This is a candidate for govhr, where it would sit alongside
#' [govhr::compute_movement()].
#'
#' @examples
#' \dontrun{
#' hr <- data.frame(
#'   personnel_id = c(1, 2, 1, 2, 2),
#'   ref_date = as.Date(c(
#'     "2020-01-01", "2020-01-01", "2021-01-01", "2021-01-01", "2022-01-01"
#'   )),
#'   employment_status = c("active", "active", "pensioner", "active", "active")
#' )
#' compute_retirement(hr)
#' }
#'
#' @export
compute_retirement <- function(data, ...) {
  UseMethod("compute_retirement")
}

#' @rdname compute_retirement
#' @importFrom data.table := .N as.data.table data.table fifelse setorderv shift
#' @importFrom rlang check_dots_empty
#' @export
compute_retirement.data.frame <- function(
  data,
  group_cols = NULL,
  status_col = "employment_status",
  ...
) {
  rlang::check_dots_empty()

  if ("ref_date" %in% group_cols) {
    stop("`ref_date` should not be included in `group_cols`")
  }

  dt <- data.table::as.data.table(data)
  active <- dt[get(status_col) == "active"]

  # one row per person and date, so people with several contracts count once
  person_dates <- unique(active[, .(personnel_id, ref_date)])
  person_groups <- unique(
    active[, c("personnel_id", "ref_date", group_cols), with = FALSE]
  )
  pensioner_dates <- unique(
    dt[get(status_col) == "pensioner", .(personnel_id, ref_date)]
  )

  dates <- sort(unique(person_dates$ref_date))
  calendar <- data.table::data.table(
    ref_date = dates,
    next_date = data.table::shift(dates, type = "lead")
  )

  events <- calendar[person_dates, on = "ref_date"]

  # separated: no active record on the next date, as in
  # govhr::compute_movement(). a pension drawn alongside an active contract is
  # therefore not a retirement
  events[, separated := TRUE]
  events[
    person_dates,
    on = .(personnel_id, next_date = ref_date),
    separated := FALSE
  ]

  # pension registration can lag the exit, so roll forward to the first
  # pensioner and active records from the next date onwards rather than
  # looking at the next date only
  next_pension <- pensioner_dates[
    events,
    on = .(personnel_id, ref_date = next_date),
    roll = -Inf,
    x.ref_date
  ]
  next_return <- person_dates[
    events,
    on = .(personnel_id, ref_date = next_date),
    roll = -Inf,
    x.ref_date
  ]

  # NA when there is no next date to compare with. a pension that starts by
  # the time the person returns, e.g. a retiree rehired on contract, still
  # marks the exit as a retirement
  events[
    , retired := data.table::fifelse(
      is.na(next_date),
      NA,
      separated &
        !is.na(next_pension) &
        (is.na(next_return) | next_pension <= next_return)
    )
  ]

  retirement <- events[person_groups, on = c("personnel_id", "ref_date")][
    , .(
      headcount = .N,
      retirements = sum(retired)
    ),
    by = c("ref_date", group_cols)
  ][
    , retirement_rate := retirements / headcount
  ]

  data.table::setorderv(retirement, c("ref_date", group_cols))

  retirement[]
}

#' @rdname compute_retirement
#' @importFrom dplyr all_of anti_join coalesce distinct filter if_else
#'   inner_join join_by lead left_join mutate n rename select summarise
#' @importFrom rlang .data check_dots_empty
#' @export
compute_retirement.tbl_dbi <- function(
  data,
  group_cols = NULL,
  status_col = "employment_status",
  ...
) {
  rlang::check_dots_empty()

  if ("ref_date" %in% group_cols) {
    stop("`ref_date` should not be included in `group_cols`")
  }

  active <- data |>
    dplyr::filter(.data[[status_col]] == "active")

  # one row per person and date, so people with several contracts count once
  person_dates <- active |>
    dplyr::select(dplyr::all_of(c("personnel_id", "ref_date"))) |>
    dplyr::distinct()

  person_groups <- active |>
    dplyr::select(dplyr::all_of(c("personnel_id", "ref_date", group_cols))) |>
    dplyr::distinct()

  pensioner_dates <- data |>
    dplyr::filter(.data[[status_col]] == "pensioner") |>
    dplyr::select(dplyr::all_of(c("personnel_id", "ref_date"))) |>
    dplyr::distinct()

  calendar <- person_dates |>
    dplyr::distinct(ref_date) |>
    dplyr::mutate(next_date = dplyr::lead(ref_date, order_by = ref_date))

  # no active record on the next date, as in govhr::compute_movement(). a
  # pension drawn alongside an active contract is therefore not a retirement
  separations <- person_dates |>
    dplyr::inner_join(calendar, by = "ref_date") |>
    dplyr::filter(!is.na(next_date)) |>
    dplyr::anti_join(
      person_dates,
      by = dplyr::join_by(personnel_id, next_date == ref_date)
    )

  # pension registration can lag the exit, so take the first pensioner and
  # active records from the next date onwards. SQL has no rolling join, hence
  # the inequality join and min()
  first_record_after_exit <- function(records, name) {
    separations |>
      dplyr::inner_join(
        records |> dplyr::rename(record_date = "ref_date"),
        by = dplyr::join_by(personnel_id, next_date <= record_date)
      ) |>
      dplyr::summarise(
        !!name := min(record_date, na.rm = TRUE),
        .by = dplyr::all_of(c("personnel_id", "ref_date"))
      )
  }

  retired <- separations |>
    dplyr::inner_join(
      first_record_after_exit(pensioner_dates, "next_pension"),
      by = c("personnel_id", "ref_date")
    ) |>
    dplyr::left_join(
      first_record_after_exit(person_dates, "next_return"),
      by = c("personnel_id", "ref_date")
    ) |>
    # a pension that starts by the time the person returns, e.g. a retiree
    # rehired on contract, still marks the exit as a retirement
    dplyr::filter(is.na(next_return) | next_pension <= next_return) |>
    dplyr::select(dplyr::all_of(c("personnel_id", "ref_date"))) |>
    dplyr::mutate(retired = 1)

  person_groups |>
    dplyr::inner_join(calendar, by = "ref_date") |>
    dplyr::left_join(retired, by = c("personnel_id", "ref_date")) |>
    # SQL's SUM() returns NULL, not 0, when nobody retired, so people without
    # a retirement are counted as 0 first
    dplyr::summarise(
      headcount = dplyr::n(),
      retirements = sum(dplyr::coalesce(retired, 0), na.rm = TRUE),
      .by = dplyr::all_of(c("ref_date", "next_date", group_cols))
    ) |>
    # no next date to compare with, so retirements are unknown
    dplyr::mutate(
      retirements = dplyr::if_else(is.na(next_date), NA_real_, retirements),
      retirement_rate = retirements / headcount
    ) |>
    dplyr::select(
      dplyr::all_of(
        c("ref_date", group_cols, "headcount", "retirements", "retirement_rate")
      )
    )
}

#' Project retirements of the current workforce
#'
#' Projects, for each year ahead, how many people in the current workforce
#' reach the retirement age, what share of the current headcount they make up
#' and, optionally, what their retirement costs are. The current workforce is
#' everyone active on the latest reference date.
#'
#' @param data Data frame or remote database table (`tbl_dbi`) with one row
#'   per person-record. Must contain `personnel_id`, `ref_date` and the
#'   columns named in `birth_col` and `status_col`, and in `measure_col` if
#'   given.
#' @param threshold_age Whole number. Age at which people retire. Default
#'   `60`.
#' @param birth_col Character. Column holding dates of birth. Default
#'   `"birth_date"`.
#' @param group_cols Character vector of columns to group by, such as
#'   `"est_id"`, or `NULL` (default) for the whole workforce. Must not include
#'   `ref_date`.
#' @param measure_col Character. Pay column used to cost the retirements, or
#'   `NULL` (default) to only count them.
#' @param status_col Character. Column holding employment status, with active
#'   personnel recorded as `"active"`. Default `"employment_status"`.
#' @param retirement_coefficient Number. Share of their pay that retirees go
#'   on receiving as a pension, used to cost the retirements. Default `0.6`.
#' @param horizon Whole number. How many years past the latest reference date
#'   to project. Default `10`.
#' @param ... Arguments passed to methods.
#'
#' @returns A table with one row per projected year and group, containing:
#' \describe{
#'   \item{ref_date}{The last day of the year in which people reach
#'     `threshold_age`.}
#'   \item{headcount}{Number of people in the current workforce.}
#'   \item{projected_retirements}{People in the current workforce who reach
#'     `threshold_age` that year.}
#'   \item{projected_retirement_rate}{`projected_retirements` divided by
#'     `headcount`.}
#'   \item{projected_cost}{Only with `measure_col`: the retirees' pay on the
#'     latest date, times `retirement_coefficient`.}
#' }
#' A data.table for data frame input; a lazy table for `tbl_dbi` input (use
#' [dplyr::collect()] to bring it into memory).
#'
#' @details
#' Only people active on the latest date are projected: people who left
#' earlier, or whose record that date is not active, are no longer in the
#' workforce. Nor are people who reached `threshold_age` by the latest date,
#' even if they are still active.
#'
#' The projected years are those whose last day falls after the latest date
#' and at most `horizon` years after it. Every group in the current workforce
#' appears in every year, with zero retirements when nobody reaches
#' `threshold_age`.
#'
#' People are counted once, even if they hold several contracts, and the pay
#' on all their contracts is costed. With `group_cols`, each person is counted
#' in the group they belong to on the latest date. People with a missing
#' `personnel_id` are not counted, as in [govhr::compute_headcount()].
#'
#' This is a candidate to replace [govhr::project_retirement()]. Unlike it, it
#' projects only the current active workforce, dates every projection by its
#' year-end, costs each group separately and fills years without retirements
#' with zero.
#'
#' @examples
#' \dontrun{
#' hr <- data.frame(
#'   personnel_id = c(1, 2, 3, 1, 2, 3),
#'   ref_date = as.Date(rep(c("2020-01-01", "2021-01-01"), each = 3)),
#'   employment_status = c(rep("active", 5), "pensioner"),
#'   birth_date = as.Date(rep(c("1962-05-01", "1990-01-01", "1950-01-01"), 2)),
#'   gross_salary_lcu = c(100, 200, 300, 110, 210, 90)
#' )
#' project_retirement(hr, measure_col = "gross_salary_lcu")
#' }
#'
#' @export
project_retirement <- function(data, ...) {
  UseMethod("project_retirement")
}

#' @rdname project_retirement
#' @importFrom data.table := as.data.table setcolorder setnafill setorderv
#'   uniqueN
#' @importFrom govhr compute_headcount
#' @importFrom lubridate add_with_rollback years
#' @importFrom rlang check_dots_empty
#' @export
#' 
#' @seealso [compute_retirement()], which counts the retirements that already happened.
project_retirement.data.frame <- function(
  data,
  threshold_age = 60,
  birth_col = "birth_date",
  group_cols = NULL,
  measure_col = NULL,
  status_col = "employment_status",
  retirement_coefficient = 0.6,
  horizon = 10,
  ...
) {
  rlang::check_dots_empty()

  if ("ref_date" %in% group_cols) {
    stop("`ref_date` should not be included in `group_cols`")
  }

  dt <- data.table::as.data.table(data)
  latest_date <- max(dt$ref_date, na.rm = TRUE)
  calendar <- data.table::as.data.table(
    retirement_calendar(latest_date, horizon)
  )

  workforce <- dt[ref_date == latest_date & get(status_col) == "active"]

  headcount <- govhr::compute_headcount(workforce, group_cols = group_cols)[
    , c(group_cols, "headcount"), with = FALSE
  ]

  # people born after this date reach threshold_age after the latest date
  # this takes into account people who are already past the threshold age
  # but are still active and removes them from the projection
  born_after <- lubridate::add_with_rollback(
    latest_date,
    -lubridate::years(threshold_age)
  )

  retirees <- workforce[
    get(birth_col) > born_after,
    c("personnel_id", birth_col, group_cols, measure_col),
    with = FALSE
  ]
  retirees[, retirement_year := data.table::year(get(birth_col)) + threshold_age]

  # the calendar drops the years beyond the horizon and dates the rest
  retirees <- calendar[retirees, on = "retirement_year", nomatch = NULL]

  by_cols <- c("ref_date", group_cols)

  projected <- if (is.null(measure_col)) {
    retirees[
      , .(projected_retirements = uniqueN(personnel_id, na.rm = TRUE)),
      by = by_cols
    ]
  } else {
    retirees[
      , .(
        projected_retirements = uniqueN(personnel_id, na.rm = TRUE),
        projected_cost = sum(get(measure_col), na.rm = TRUE) *
          retirement_coefficient
      ),
      by = by_cols
    ]
  }

  # every group in the current workforce appears in every projected year:
  # grouping by all of headcount's columns repeats each of its rows per year
  grid <- headcount[, .(ref_date = calendar$ref_date), by = names(headcount)]

  projection <- projected[grid, on = by_cols]

  data.table::setnafill(
    projection,
    fill = 0,
    cols = intersect(
      c("projected_retirements", "projected_cost"),
      names(projection)
    )
  )
  projection[, projected_retirement_rate := projected_retirements / headcount]

  data.table::setcolorder(
    projection,
    c(by_cols, "headcount", "projected_retirements", "projected_retirement_rate")
  )
  data.table::setorderv(projection, by_cols)

  projection[]
}

#' @rdname project_retirement
#' @importFrom dplyr across all_of any_of coalesce cross_join filter
#'   inner_join left_join mutate n_distinct pull select summarise
#' @importFrom govhr compute_headcount
#' @importFrom lubridate add_with_rollback year years
#' @importFrom rlang .data check_dots_empty exprs sym
#' @export
project_retirement.tbl_dbi <- function(
  data,
  threshold_age = 60,
  birth_col = "birth_date",
  group_cols = NULL,
  measure_col = NULL,
  status_col = "employment_status",
  retirement_coefficient = 0.6,
  horizon = 10,
  ...
) {
  rlang::check_dots_empty()

  if ("ref_date" %in% group_cols) {
    stop("`ref_date` should not be included in `group_cols`")
  }

  # the projected years and the age cut-off both hang on the latest date, so
  # that single value is read into memory
  latest_date <- data |>
    dplyr::summarise(ref_date = max(ref_date, na.rm = TRUE)) |>
    dplyr::pull(ref_date)

  # copy_inline() sends the calendar as part of the query, so no write access
  # to the database is needed
  calendar <- dbplyr::copy_inline(
    dbplyr::remote_con(data),
    retirement_calendar(latest_date, horizon)
  )

  workforce <- data |>
    dplyr::filter(
      ref_date == !!latest_date,
      .data[[status_col]] == "active"
    )

  headcount <- workforce |>
    govhr::compute_headcount(group_cols = group_cols) |>
    dplyr::select(dplyr::all_of(c(group_cols, "headcount")))

  # people born after this date reach threshold_age after the latest date
  # this takes into account people who are already past the threshold age
  # but are still active and removes them from the projection
  born_after <- lubridate::add_with_rollback(
    latest_date,
    -lubridate::years(threshold_age)
  )

  cost <- if (!is.null(measure_col)) {
    rlang::exprs(
      projected_cost = sum(!!rlang::sym(measure_col), na.rm = TRUE) *
        !!retirement_coefficient
    )
  }

  projected <- workforce |>
    dplyr::filter(.data[[birth_col]] > !!born_after) |>
    dplyr::select(
      dplyr::all_of(c("personnel_id", birth_col, group_cols, measure_col))
    ) |>
    dplyr::mutate(
      retirement_year = lubridate::year(.data[[birth_col]]) + !!threshold_age
    ) |>
    # the calendar drops the years beyond the horizon and dates the rest
    dplyr::inner_join(calendar, by = "retirement_year") |>
    dplyr::summarise(
      projected_retirements = dplyr::n_distinct(personnel_id, na.rm = TRUE),
      !!!cost,
      .by = dplyr::all_of(c("ref_date", group_cols))
    )

  # every group in the current workforce appears in every projected year
  headcount |>
    dplyr::cross_join(dplyr::select(calendar, ref_date)) |>
    # keep missing groups: sql joins do not match NA keys by default
    dplyr::left_join(
      projected,
      by = c("ref_date", group_cols),
      na_matches = "na"
    ) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::any_of(c("projected_retirements", "projected_cost")),
        \(x) dplyr::coalesce(x, 0)
      ),
      projected_retirement_rate = projected_retirements / headcount
    ) |>
    dplyr::select(
      ref_date,
      dplyr::all_of(group_cols),
      headcount,
      projected_retirements,
      projected_retirement_rate,
      dplyr::any_of("projected_cost")
    )
}

# the projected years are those whose last day falls after the latest date and
# at most `horizon` years after it, each dated by that last day
#' @importFrom lubridate add_with_rollback year years
#' @keywords internal
#' @noRd
retirement_calendar <- function(latest_date, horizon) {
  last_date <- lubridate::add_with_rollback(
    latest_date,
    lubridate::years(horizon)
  )

  retirement_year <- seq(
    lubridate::year(latest_date),
    lubridate::year(last_date)
  )
  year_end <- as.Date(paste0(retirement_year, "-12-31"))
  in_horizon <- year_end > latest_date & year_end <= last_date

  data.frame(
    retirement_year = retirement_year[in_horizon],
    ref_date = year_end[in_horizon]
  )
}
