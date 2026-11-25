# Tests for the "of which" composition card shared by the analytics panels.

# ---- compute_composition_share -----------------------------------------------

test_that("compute_composition_share fills each stack up to 100", {
  # G1 has no men in 2021, G2's gender is unknown in 2020, and G2 has no
  # measure in 2021, as compute_wagebill() gives a series absent on a date
  composition <- data.frame(
    ref_date = as.Date(c(
      "2020-01-01", "2020-01-01", "2020-01-01", "2021-01-01", "2021-01-01"
    )),
    paygrade = c("G1", "G1", "G2", "G1", "G2"),
    gender = c("female", "male", NA, "female", "male"),
    headcount = c(1, 4, 2, 2, NA)
  )

  shares <- compute_composition_share(
    composition,
    value_col = "headcount",
    group_col = "paygrade",
    breakdown_col = "gender"
  )

  share_of <- function(date, paygrade, category) {
    shares[["share"]][
      shares[["ref_date"]] == as.Date(date) &
        shares[["paygrade"]] == paygrade &
        shares[["category"]] == category
    ]
  }

  stack_totals <- tapply(
    shares[["share"]],
    paste(shares[["ref_date"]], shares[["paygrade"]]),
    sum
  )

  expect_equal(levels(shares[["category"]]), c("male", "female", "Missing"))
  expect_equal(as.vector(stack_totals), rep(100, 3))
  expect_equal(share_of("2020-01-01", "G1", "male"), 80)
  expect_equal(share_of("2021-01-01", "G1", "male"), 0)
  expect_equal(share_of("2020-01-01", "G2", "Missing"), 100)
})

test_that("compute_composition_share combines the smallest categories", {
  many <- data.frame(
    ref_date = as.Date("2020-01-01"),
    occupation = c("a", "b", "c", "d"),
    headcount = c(40, 30, 20, 10)
  )

  shares <- compute_composition_share(
    many,
    value_col = "headcount",
    group_col = "ref_date",
    breakdown_col = "occupation",
    max_categories = 2
  )

  expect_equal(levels(shares[["category"]]), c("a", "b", "Other"))
  expect_equal(shares[["share"]][shares[["category"]] == "Other"], 30)
})

# ---- composition_server ------------------------------------------------------

test_that("composition_server computes only on apply, and only when it can", {
  people <- data.frame(
    personnel_id = c("1", "2", "3", "4", "1", "2", "3", "5"),
    ref_date = as.Date(rep(c("2020-01-01", "2021-01-01"), each = 4)),
    paygrade = c("G1", "G1", "G2", "G2", "G1", "G1", "G2", "G2"),
    gender = c("female", "male", "male", NA, "female", "female", "male", "male")
  )

  selections <- shiny::reactiveValues(
    group = "paygrade",
    breakdown = "none",
    additive = TRUE,
    apply = 0
  )

  apply_selection <- function(session) {
    selections[["apply"]] <- selections[["apply"]] + 1
    session$flushReact()
  }

  shiny::testServer(
    composition_server,
    args = list(
      filtered = \() people,
      group_col = \() selections[["group"]],
      breakdown_col = \() selections[["breakdown"]],
      apply = \() selections[["apply"]],
      measure = \() {
        if (!selections[["additive"]]) {
          return(NULL)
        }

        list(
          compute = govhr::compute_headcount,
          value_col = "headcount",
          label = "headcount"
        )
      }
    ),
    {
      expect_null(applied())

      # an unapplied breakdown does not reach the card
      selections[["breakdown"]] <- "gender"
      session$flushReact()
      expect_null(applied())

      apply_selection(session)
      expect_setequal(
        levels(applied()[["shares"]][["category"]]),
        c("female", "male", "Missing")
      )
      expect_no_error(output$composition)

      selections[["group"]] <- "gender"
      apply_selection(session)
      expect_true(applied()[["same_as_group"]])
      expect_match(as.character(output$card[["html"]]), "different from the group")

      # a rate or ratio has no parts that add up to it
      selections[["additive"]] <- FALSE
      apply_selection(session)
      expect_null(applied())
    }
  )
})
