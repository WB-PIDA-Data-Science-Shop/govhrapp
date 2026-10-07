#' Compute retirements
#'
#' Counts, for each reference date, how many people are active and how many of
#' them leave active status by the next date into a pension, i.e. whose next
#' status after leaving is pensioner.
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
#' @seealso [govhr::compute_movement()], whose separations include these
#'   retirements and whose rates share their denominator.
#'   [detect_retirement()], which flags the retirements of each person.
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
#' @importFrom data.table := .N as.data.table setorderv
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

  # one row per person, date and group, so people with several contracts
  # count once
  person_groups <- unique(
    dt[
      get(status_col) == "active",
      c("personnel_id", "ref_date", group_cols),
      with = FALSE
    ]
  )

  retirement <- detect_retirement(dt, status_col = status_col)[
    person_groups,
    on = c("personnel_id", "ref_date")
  ][
    , .(
      headcount = .N,
      retirements = sum(retirement)
    ),
    by = c("ref_date", group_cols)
  ][
    , retirement_rate := retirements / headcount
  ]

  data.table::setorderv(retirement, c("ref_date", group_cols))

  retirement[]
}

#' @rdname compute_retirement
#' @importFrom dplyr all_of distinct filter if_else inner_join mutate n select
#'   summarise
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

  # one row per person, date and group, so people with several contracts
  # count once
  person_groups <- data |>
    dplyr::filter(.data[[status_col]] == "active") |>
    dplyr::select(dplyr::all_of(c("personnel_id", "ref_date", group_cols))) |>
    dplyr::distinct()

  person_groups |>
    dplyr::inner_join(
      detect_retirement(data, status_col = status_col),
      by = c("personnel_id", "ref_date")
    ) |>
    # retirement is NULL on the last date, so its SUM() stays NULL there.
    # counted as doubles, since some backends, such as SQLite, divide integers
    # without the fraction in retirement_rate
    dplyr::summarise(
      headcount = dplyr::n(),
      retirements = sum(dplyr::if_else(retirement, 1, 0), na.rm = TRUE),
      .by = dplyr::all_of(c("ref_date", group_cols))
    ) |>
    dplyr::mutate(retirement_rate = retirements / headcount) |>
    dplyr::select(
      dplyr::all_of(
        c("ref_date", group_cols, "headcount", "retirements", "retirement_rate")
      )
    )
}

#' Detect hires and separations
#'
#' Flags, for each active person and reference date, whether they were hired
#' since the previous date and whether they are gone by the next date.
#'
#' @param data Data frame or remote database table (`tbl_dbi`) with one row
#'   per person-record. Must contain `personnel_id`, `ref_date` and the column
#'   named in `status_col`.
#' @param status_col Character. Column holding employment status. Only rows
#'   equal to `"active"` are considered. Default `"employment_status"`.
#' @param ... Arguments passed to methods.
#'
#' @returns A table with one row per active person and `ref_date`, containing:
#' \describe{
#'   \item{personnel_id, ref_date}{The person and date.}
#'   \item{prev_date, next_date}{The neighbouring dates found in the data,
#'     which the person's status is compared with. `NA` on the first and last
#'     date.}
#'   \item{hire}{`TRUE` if the person was not active on `prev_date`. `NA` on
#'     the first date, which has nothing to compare with.}
#'   \item{separation}{`TRUE` if the person is not active on `next_date`. `NA`
#'     on the last date.}
#' }
#' A data.table for data frame input; a lazy table for `tbl_dbi` input (use
#' [dplyr::collect()] to bring it into memory).
#'
#' @details
#' The previous and next dates are the neighbouring dates found in the data,
#' so the dates do not need to be evenly spaced. People with several contracts
#' on a date appear once. A separation is any exit from active status,
#' including retirement.
#'
#' This is a candidate for govhr, where [govhr::compute_movement()] could be
#' built by counting its flags.
#'
#' @seealso [govhr::compute_movement()], which counts these hires and
#'   separations. [detect_retirement()], which flags the retirements among the
#'   separations.
#'
#' @examples
#' \dontrun{
#' hr <- data.frame(
#'   personnel_id = c(1, 2, 1, 3, 1, 3),
#'   ref_date = as.Date(rep(c("2020-01-01", "2021-01-01", "2022-01-01"), each = 2)),
#'   employment_status = "active"
#' )
#' detect_movement(hr)
#' }
#'
#' @export
detect_movement <- function(data, ...) {
  UseMethod("detect_movement")
}

#' @rdname detect_movement
#' @importFrom data.table := as.data.table data.table fifelse setorderv shift
#' @importFrom rlang check_dots_empty
#' @export
detect_movement.data.frame <- function(
  data,
  status_col = "employment_status",
  ...
) {
  rlang::check_dots_empty()

  dt <- data.table::as.data.table(data)

  # one row per person and date, so people with several contracts count once
  person_dates <- unique(
    dt[get(status_col) == "active", .(personnel_id, ref_date)]
  )

  # previous and next date for each date in the data
  dates <- sort(unique(person_dates[["ref_date"]]))
  calendar <- data.table::data.table(
    ref_date = dates,
    prev_date = data.table::shift(dates),
    next_date = data.table::shift(dates, type = "lead")
  )

  movement <- calendar[person_dates, on = "ref_date"]

  # hired: no active record on the previous date. NA when there is no
  # previous date to compare with
  movement[, hire := data.table::fifelse(is.na(prev_date), NA, TRUE)]
  movement[
    person_dates,
    on = .(personnel_id, prev_date = ref_date),
    hire := FALSE
  ]

  # separated: no active record on the next date
  movement[, separation := data.table::fifelse(is.na(next_date), NA, TRUE)]
  movement[
    person_dates,
    on = .(personnel_id, next_date = ref_date),
    separation := FALSE
  ]

  data.table::setorderv(movement, c("ref_date", "personnel_id"))

  movement[, .(personnel_id, ref_date, prev_date, next_date, hire, separation)]
}

#' @rdname detect_movement
#' @importFrom dplyr all_of anti_join distinct filter if_else inner_join
#'   join_by lag lead left_join mutate select
#' @importFrom rlang .data check_dots_empty
#' @export
detect_movement.tbl_dbi <- function(
  data,
  status_col = "employment_status",
  ...
) {
  rlang::check_dots_empty()

  # one row per person and date, so people with several contracts count once
  person_dates <- data |>
    dplyr::filter(.data[[status_col]] == "active") |>
    dplyr::select(dplyr::all_of(c("personnel_id", "ref_date"))) |>
    dplyr::distinct()

  # previous and next date for each date in the data
  calendar <- person_dates |>
    dplyr::distinct(ref_date) |>
    dplyr::mutate(
      prev_date = dplyr::lag(ref_date, order_by = ref_date),
      next_date = dplyr::lead(ref_date, order_by = ref_date)
    )

  movement <- person_dates |>
    dplyr::inner_join(calendar, by = "ref_date")

  # hired: no active record on the previous date
  hires <- movement |>
    dplyr::filter(!is.na(prev_date)) |>
    dplyr::anti_join(
      person_dates,
      by = dplyr::join_by(personnel_id, prev_date == ref_date)
    ) |>
    dplyr::select(dplyr::all_of(c("personnel_id", "ref_date"))) |>
    dplyr::mutate(hired = 1)

  # separated: no active record on the next date
  separations <- movement |>
    dplyr::filter(!is.na(next_date)) |>
    dplyr::anti_join(
      person_dates,
      by = dplyr::join_by(personnel_id, next_date == ref_date)
    ) |>
    dplyr::select(dplyr::all_of(c("personnel_id", "ref_date"))) |>
    dplyr::mutate(separated = 1)

  movement |>
    dplyr::left_join(hires, by = c("personnel_id", "ref_date")) |>
    dplyr::left_join(separations, by = c("personnel_id", "ref_date")) |>
    # NA when there is no previous (next) date to compare with
    dplyr::mutate(
      hire = dplyr::if_else(is.na(prev_date), NA, !is.na(hired)),
      separation = dplyr::if_else(is.na(next_date), NA, !is.na(separated))
    ) |>
    dplyr::select(
      personnel_id, ref_date, prev_date, next_date, hire, separation
    )
}

#' Detect retirements
#'
#' Flags, for each active person and reference date, whether they leave active
#' status by the next date into a pension, i.e. whether their next status
#' after leaving is pensioner.
#'
#' @inheritParams detect_movement
#' @param status_col Character. Column holding employment status, with active
#'   personnel recorded as `"active"` and retirees as `"pensioner"`. Default
#'   `"employment_status"`.
#'
#' @returns A table with one row per active person and `ref_date`, containing
#'   `personnel_id`, `ref_date`, `next_date` (the next date found in the data)
#'   and `retirement`: `TRUE` if the person retires by `next_date`, and `NA` on
#'   the last date, which has nothing to compare with. A data.table for data
#'   frame input; a lazy table for `tbl_dbi` input (use [dplyr::collect()] to
#'   bring it into memory).
#'
#' @details
#' A retirement is a separation whose next status is pensioner. A pension
#' drawn alongside an active contract is therefore not a retirement.
#'
#' Pension registration can lag the exit, so the pensioner record may appear
#' at any later date, not only the next one. A retirement is dated by the exit,
#' not by the registration. A person who returns to active work before any
#' pensioner record is not retired at the earlier exit. There is no limit on
#' the lag, so an exit followed years later by a deferred pension also counts
#' as a retirement.
#'
#' This is a candidate to replace [govhr::detect_retirement()]. Unlike it, it
#' allows for lagged pension registration, flags every active person and date
#' instead of listing the retirements only, and works on database tables.
#'
#' @seealso [compute_retirement()], which counts these retirements.
#'   [detect_movement()], which flags the separations they are drawn from.
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
#' detect_retirement(hr)
#' }
#'
#' @export
detect_retirement <- function(data, ...) {
  UseMethod("detect_retirement")
}

#' @rdname detect_retirement
#' @importFrom data.table := as.data.table fifelse
#' @importFrom rlang check_dots_empty
#' @export
detect_retirement.data.frame <- function(
  data,
  status_col = "employment_status",
  ...
) {
  rlang::check_dots_empty()

  dt <- data.table::as.data.table(data)
  movement <- detect_movement(dt, status_col = status_col)

  person_dates <- movement[, .(personnel_id, ref_date)]
  pensioner_dates <- unique(
    dt[get(status_col) == "pensioner", .(personnel_id, ref_date)]
  )

  # pension registration can lag the exit, so roll forward to the first
  # pensioner and active records from the next date onwards rather than
  # looking at the next date only
  next_pension <- pensioner_dates[
    movement,
    on = .(personnel_id, ref_date = next_date),
    roll = -Inf,
    x.ref_date
  ]
  next_return <- person_dates[
    movement,
    on = .(personnel_id, ref_date = next_date),
    roll = -Inf,
    x.ref_date
  ]

  # NA when there is no next date to compare with. a pension that starts by
  # the time the person returns, e.g. a retiree rehired on contract, still
  # marks the exit as a retirement
  movement[
    , retirement := data.table::fifelse(
      is.na(next_date),
      NA,
      separation &
        !is.na(next_pension) &
        (is.na(next_return) | next_pension <= next_return)
    )
  ]

  movement[, .(personnel_id, ref_date, next_date, retirement)]
}

#' @rdname detect_retirement
#' @importFrom dplyr all_of distinct filter if_else inner_join join_by
#'   left_join mutate rename select summarise
#' @importFrom rlang .data check_dots_empty
#' @export
detect_retirement.tbl_dbi <- function(
  data,
  status_col = "employment_status",
  ...
) {
  rlang::check_dots_empty()

  movement <- detect_movement(data, status_col = status_col)

  person_dates <- movement |>
    dplyr::select(dplyr::all_of(c("personnel_id", "ref_date")))

  pensioner_dates <- data |>
    dplyr::filter(.data[[status_col]] == "pensioner") |>
    dplyr::select(dplyr::all_of(c("personnel_id", "ref_date"))) |>
    dplyr::distinct()

  separations <- movement |>
    dplyr::filter(separation)

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

  movement |>
    dplyr::left_join(retired, by = c("personnel_id", "ref_date")) |>
    # NA when there is no next date to compare with
    dplyr::mutate(
      retirement = dplyr::if_else(is.na(next_date), NA, !is.na(retired))
    ) |>
    dplyr::select(personnel_id, ref_date, next_date, retirement)
}

#' Compute the cost of hires and separations
#'
#' Adds up, for each reference date, the pay of the people who are hired or
#' separate at that date.
#'
#' @param data Data frame or remote database table (`tbl_dbi`) with one row
#'   per person-record. Must contain `personnel_id`, `ref_date` and the
#'   columns named in `measure_col` and `status_col`.
#' @param event_type Character vector of the movements to cost: `"hire"`,
#'   `"separation"` or both. Default both.
#' @param measure_col Character. Name of the pay column to add up.
#' @param group_cols Character vector of columns to group by, such as
#'   `"est_id"`, or `NULL` (default) for the whole workforce. Must not include
#'   `ref_date`.
#' @param status_col Character. Column holding employment status. Only rows
#'   equal to `"active"` are considered. Default `"employment_status"`.
#' @param ... Arguments passed to methods.
#'
#' @returns A table with one row per `ref_date`, group and movement type,
#'   containing `movement_type` (one of `event_type`) and `movement_cost`, the
#'   movers' pay. `movement_cost` is 0 when nobody moved, and `NA` on the date
#'   with nothing to compare with: the first date for hires, the last for
#'   separations. A data.table for data frame input; a lazy table for
#'   `tbl_dbi` input (use [dplyr::collect()] to bring it into memory).
#'
#' @details
#' Hires are costed at their pay on the date they are hired, and separations
#' at their pay on their last active date. The pay on all of a mover's active
#' records that date is added up, so people with several contracts are costed
#' in full; pay recorded alongside, such as a pension, is not. Missing pay
#' counts as 0.
#'
#' With `group_cols`, each mover's pay is counted in the group of the record it
#' comes from. Every group with active personnel on a date appears for that
#' date.
#'
#' This is a candidate to replace [govhr::compute_movement_cost()]. Unlike it,
#' it compares neighbouring dates in the data instead of a regular date
#' sequence, uses `"separation"` instead of `"fire"`, leaves retirements to
#' [compute_retirement_cost()], reports dates without movers as 0, and works
#' on database tables.
#'
#' @seealso [detect_movement()], which flags the movers.
#'   [govhr::compute_movement()], which counts them.
#'   [compute_retirement_cost()], which costs the retirements.
#'
#' @examples
#' \dontrun{
#' hr <- data.frame(
#'   personnel_id = c(1, 1, 1, 2, 2),
#'   ref_date = as.Date(c(
#'     "2019-01-01", "2020-01-01", "2021-01-01", "2020-01-01", "2021-01-01"
#'   )),
#'   employment_status = "active",
#'   wage = c(100, 100, 100, 250, 250)
#' )
#' compute_movement_cost(hr, event_type = "hire", measure_col = "wage")
#' }
#'
#' @export
compute_movement_cost <- function(data, ...) {
  UseMethod("compute_movement_cost")
}

#' @rdname compute_movement_cost
#' @importFrom data.table as.data.table fcoalesce fifelse rbindlist setorderv
#' @importFrom rlang arg_match check_dots_empty
#' @export
compute_movement_cost.data.frame <- function(
  data,
  event_type = c("hire", "separation"),
  measure_col,
  group_cols = NULL,
  status_col = "employment_status",
  ...
) {
  rlang::check_dots_empty()
  event_type <- rlang::arg_match(event_type, multiple = TRUE)

  if ("ref_date" %in% group_cols) {
    stop("`ref_date` should not be included in `group_cols`")
  }

  dt <- data.table::as.data.table(data)
  keys <- c("personnel_id", "ref_date")

  flags <- detect_movement(dt, status_col = status_col)[
    , .(personnel_id, ref_date, hire, separation)
  ]

  # every active record carries its person's flags, so a mover's pay is added
  # up over all their contracts that date
  records <- flags[
    dt[
      get(status_col) == "active",
      c(keys, group_cols, measure_col),
      with = FALSE
    ],
    on = keys
  ]

  by_cols <- c("ref_date", group_cols)

  costs <- lapply(event_type, function(movement) {
    # flags are NA on a date with nothing to compare with, which carries
    # through to the cost
    cost <- records[
      , .(
        movement_type = movement,
        movement_cost = sum(
          data.table::fifelse(
            get(movement),
            data.table::fcoalesce(as.numeric(get(measure_col)), 0),
            0
          )
        )
      ),
      by = by_cols
    ]

    data.table::setorderv(cost, by_cols)

    cost
  })

  data.table::rbindlist(costs)
}

#' @rdname compute_movement_cost
#' @importFrom dplyr all_of coalesce filter if_else inner_join mutate select
#'   summarise union_all
#' @importFrom purrr map reduce
#' @importFrom rlang .data arg_match check_dots_empty
#' @export
compute_movement_cost.tbl_dbi <- function(
  data,
  event_type = c("hire", "separation"),
  measure_col,
  group_cols = NULL,
  status_col = "employment_status",
  ...
) {
  rlang::check_dots_empty()
  event_type <- rlang::arg_match(event_type, multiple = TRUE)

  if ("ref_date" %in% group_cols) {
    stop("`ref_date` should not be included in `group_cols`")
  }

  keys <- c("personnel_id", "ref_date")

  flags <- detect_movement(data, status_col = status_col) |>
    dplyr::select(personnel_id, ref_date, hire, separation)

  # every active record carries its person's flags, so a mover's pay is added
  # up over all their contracts that date
  records <- data |>
    dplyr::filter(.data[[status_col]] == "active") |>
    dplyr::select(dplyr::all_of(c(keys, group_cols, measure_col))) |>
    dplyr::inner_join(flags, by = keys)

  # flags are NULL on a date with nothing to compare with, so the SUM() is
  # NULL there too
  event_type |>
    purrr::map(
      \(movement) {
        records |>
          dplyr::summarise(
            movement_cost = sum(
              dplyr::if_else(
                .data[[movement]],
                dplyr::coalesce(.data[[measure_col]], 0),
                0
              ),
              na.rm = TRUE
            ),
            .by = dplyr::all_of(c("ref_date", group_cols))
          ) |>
          dplyr::mutate(movement_type = !!movement)
      }
    ) |>
    purrr::reduce(dplyr::union_all) |>
    dplyr::select(
      ref_date,
      dplyr::all_of(group_cols),
      movement_type,
      movement_cost
    )
}

#' Compute the cost of retirements
#'
#' Adds up, for each reference date, the pay of the people who retire at that
#' date.
#'
#' @inheritParams compute_movement_cost
#' @param status_col Character. Column holding employment status, with active
#'   personnel recorded as `"active"` and retirees as `"pensioner"`. Default
#'   `"employment_status"`.
#'
#' @returns A table with one row per `ref_date` and group, containing
#'   `retirement_cost`, the retirees' pay. `retirement_cost` is 0 when nobody
#'   retired, and `NA` on the last date, which has nothing to compare with. A
#'   data.table for data frame input; a lazy table for `tbl_dbi` input (use
#'   [dplyr::collect()] to bring it into memory).
#'
#' @details
#' Retirees are costed at their pay on their last active date. The pay on all
#' of a retiree's active records that date is added up, so people with several
#' contracts are costed in full; pay recorded alongside, such as a pension, is
#' not. Missing pay counts as 0.
#'
#' With `group_cols`, each retiree's pay is counted in the group of the record
#' it comes from. Every group with active personnel on a date appears for that
#' date.
#'
#' This is a candidate for govhr, where it would take over the retirements of
#' [govhr::compute_movement_cost()].
#'
#' @seealso [detect_retirement()], which flags the retirees.
#'   [compute_retirement()], which counts them. [compute_movement_cost()],
#'   which costs the hires and separations.
#'
#' @examples
#' \dontrun{
#' hr <- data.frame(
#'   personnel_id = c(1, 2, 1, 2),
#'   ref_date = as.Date(rep(c("2020-01-01", "2021-01-01"), each = 2)),
#'   employment_status = c("active", "active", "pensioner", "active"),
#'   wage = c(100, 250, 60, 250)
#' )
#' compute_retirement_cost(hr, measure_col = "wage")
#' }
#'
#' @export
compute_retirement_cost <- function(data, ...) {
  UseMethod("compute_retirement_cost")
}

#' @rdname compute_retirement_cost
#' @importFrom data.table as.data.table fcoalesce fifelse setorderv
#' @importFrom rlang check_dots_empty
#' @export
compute_retirement_cost.data.frame <- function(
  data,
  measure_col,
  group_cols = NULL,
  status_col = "employment_status",
  ...
) {
  rlang::check_dots_empty()

  if ("ref_date" %in% group_cols) {
    stop("`ref_date` should not be included in `group_cols`")
  }

  dt <- data.table::as.data.table(data)
  keys <- c("personnel_id", "ref_date")

  flags <- detect_retirement(dt, status_col = status_col)[
    , .(personnel_id, ref_date, retirement)
  ]

  # every active record carries its person's flag, so a retiree's pay is
  # added up over all their contracts that date
  records <- flags[
    dt[
      get(status_col) == "active",
      c(keys, group_cols, measure_col),
      with = FALSE
    ],
    on = keys
  ]

  by_cols <- c("ref_date", group_cols)

  # the flag is NA on the last date, which carries through to the cost
  cost <- records[
    , .(
      retirement_cost = sum(
        data.table::fifelse(
          retirement,
          data.table::fcoalesce(as.numeric(get(measure_col)), 0),
          0
        )
      )
    ),
    by = by_cols
  ]

  data.table::setorderv(cost, by_cols)

  cost[]
}

#' @rdname compute_retirement_cost
#' @importFrom dplyr all_of coalesce filter if_else inner_join select summarise
#' @importFrom rlang .data check_dots_empty
#' @export
compute_retirement_cost.tbl_dbi <- function(
  data,
  measure_col,
  group_cols = NULL,
  status_col = "employment_status",
  ...
) {
  rlang::check_dots_empty()

  if ("ref_date" %in% group_cols) {
    stop("`ref_date` should not be included in `group_cols`")
  }

  keys <- c("personnel_id", "ref_date")

  flags <- detect_retirement(data, status_col = status_col) |>
    dplyr::select(personnel_id, ref_date, retirement)

  # every active record carries its person's flag, so a retiree's pay is
  # added up over all their contracts that date
  data |>
    dplyr::filter(.data[[status_col]] == "active") |>
    dplyr::select(dplyr::all_of(c(keys, group_cols, measure_col))) |>
    dplyr::inner_join(flags, by = keys) |>
    # the flag is NULL on the last date, so the SUM() is NULL there too
    dplyr::summarise(
      retirement_cost = sum(
        dplyr::if_else(
          retirement,
          dplyr::coalesce(.data[[measure_col]], 0),
          0
        ),
        na.rm = TRUE
      ),
      .by = dplyr::all_of(c("ref_date", group_cols))
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
  latest_date <- max(dt[["ref_date"]], na.rm = TRUE)
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
  grid <- headcount[, .(ref_date = calendar[["ref_date"]]), by = names(headcount)]

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
