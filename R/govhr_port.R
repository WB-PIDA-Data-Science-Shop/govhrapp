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
