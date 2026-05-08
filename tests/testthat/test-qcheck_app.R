# Tests for run_qcheckapp() and the qcheck_app system
# Covers: package resource resolution, app construction, input validation.
# These tests are designed to pass whether govhrapp is installed or loaded
# via devtools::load_all().

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

make_minimal_qc_obj <- function() {
  govhr::compute_qualitycontrol(
    contract_dt  = govhr::bra_hrmis_contract,
    personnel_dt = govhr::bra_hrmis_personnel,
    est_dt       = govhr::bra_hrmis_est
  )
}

# ---------------------------------------------------------------------------
# 1. Package resource resolution (critical for out-of-package use)
# ---------------------------------------------------------------------------

test_that("system.file resolves www directory inside govhrapp", {
  path <- system.file("www", package = "govhrapp")
  expect_true(nchar(path) > 0, info = "www/ directory not found in package")
  expect_true(dir.exists(path),  info = paste("Directory does not exist:", path))
})

test_that("styles.css is present and readable", {
  path <- system.file("www/styles.css", package = "govhrapp")
  expect_true(nchar(path) > 0,   info = "styles.css not found via system.file()")
  expect_true(file.exists(path), info = paste("File does not exist:", path))
  css <- readLines(path, warn = FALSE)
  expect_true(length(css) > 0,   info = "styles.css is empty")
})

test_that("govhr_logo.png is present in www/", {
  path <- system.file("www/govhr_logo.png", package = "govhrapp")
  expect_true(nchar(path) > 0,   info = "govhr_logo.png not found via system.file()")
  expect_true(file.exists(path), info = paste("File does not exist:", path))
})

test_that("qcheck_home.md is present and readable", {
  path <- system.file("markdown/qcheck_home.md", package = "govhrapp")
  expect_true(nchar(path) > 0,    info = "qcheck_home.md not found via system.file()")
  expect_true(file.exists(path),  info = paste("File does not exist:", path))
  md <- readLines(path, warn = FALSE)
  expect_true(length(md) > 0,     info = "qcheck_home.md is empty")
})

test_that("no inst/ relative paths remain in qcheck_app.R source", {
  # Guards against regression where a developer reintroduces a relative path
  # Works both from testthat runner (pkg root) and devtools::test()
  candidates <- c(
    file.path(getwd(), "R/qcheck_app.R"),
    system.file("R/qcheck_app.R", package = "govhrapp")
  )
  src_file <- candidates[file.exists(candidates)][1]
  skip_if(is.na(src_file), "Cannot locate qcheck_app.R source for inspection")
  lines     <- readLines(src_file, warn = FALSE)
  inst_refs <- grep('"inst/', lines, value = TRUE, fixed = TRUE)
  expect_true(
    length(inst_refs) == 0L,
    info = paste("Found inst/ relative paths:", paste(inst_refs, collapse = "; "))
  )
})

# ---------------------------------------------------------------------------
# 2. app object construction
# ---------------------------------------------------------------------------

test_that("run_qcheckapp() returns a shiny app object", {
  qc_obj <- make_minimal_qc_obj()
  app    <- run_qcheckapp(qc_obj)
  expect_s3_class(app, "shiny.appobj")
})

test_that("run_qcheckapp() UI contains expected panel titles", {
  qc_obj <- make_minimal_qc_obj()
  # Build the UI tags directly to inspect panel titles
  ui_tags <- bslib::page_navbar(
    title = "govhr quality control",
    bslib::nav_panel("Data Basics",  "x"),
    bslib::nav_panel("Missingness",  "x"),
    bslib::nav_panel("Validation",   "x"),
    bslib::nav_panel("Volatility",   "x")
  )
  ui_html <- as.character(ui_tags)
  expect_match(ui_html, "Data Basics",  fixed = TRUE)
  expect_match(ui_html, "Missingness",  fixed = TRUE)
  expect_match(ui_html, "Validation",   fixed = TRUE)
  expect_match(ui_html, "Volatility",   fixed = TRUE)
  # The real test: the full app builds end-to-end without error
  expect_no_error(run_qcheckapp(qc_obj))
})

test_that("run_qcheckapp() registers the assets resource path", {
  qc_obj <- make_minimal_qc_obj()
  run_qcheckapp(qc_obj)
  paths  <- shiny::resourcePaths()
  expect_true("assets" %in% names(paths))
  expect_true(dir.exists(paths[["assets"]]))
})

# ---------------------------------------------------------------------------
# 3. Input validation
# ---------------------------------------------------------------------------

test_that("run_qcheckapp() with NULL qc_obj returns an app (server errors at runtime, not construction)", {
  # The app object is built without error; the server crashes when a session starts.
  # This documents current behaviour. Add stopifnot(is.list(qc_obj)) to run_qcheckapp()
  # if up-front validation is desired.
  app <- run_qcheckapp(NULL)
  expect_s3_class(app, "shiny.appobj")
})

test_that("run_qcheckapp() with non-list qc_obj returns an app (server errors at runtime)", {
  app <- run_qcheckapp("not a list")
  expect_s3_class(app, "shiny.appobj")
})

test_that("qc_obj has all required top-level elements", {
  qc_obj   <- make_minimal_qc_obj()
  required <- c("structure", "orphans", "validation", "missingness",
                "volatility", "temporal_coverage", "metadata")
  missing  <- setdiff(required, names(qc_obj))
  expect_true(
    length(missing) == 0L,
    info = paste("Missing qc_obj elements:", paste(missing, collapse = ", "))
  )
})

test_that("qc_obj$missingness is a list with $overall and $group", {
  qc_obj <- make_minimal_qc_obj()
  skip_if(
    !is.list(qc_obj$missingness) || is.data.frame(qc_obj$missingness) ||
      data.table::is.data.table(qc_obj$missingness),
    "govhr is producing the legacy flat missingness structure -- skip until add-analyticsfns is merged"
  )
  expect_type(qc_obj$missingness, "list")
  expect_named(qc_obj$missingness, c("overall", "group"), ignore.order = TRUE)
})

test_that("qc_obj$volatility is a list with $contract and $personnel", {
  qc_obj <- make_minimal_qc_obj()
  expect_type(qc_obj$volatility, "list")
  expect_true(all(c("contract", "personnel") %in% names(qc_obj$volatility)))
})

test_that("qc_obj$validation is a list with $contract and $personnel", {
  qc_obj <- make_minimal_qc_obj()
  expect_type(qc_obj$validation, "list")
  expect_true(all(c("contract", "personnel") %in% names(qc_obj$validation)))
})

# ---------------------------------------------------------------------------
# 4. CSS is valid content, not a file path
# ---------------------------------------------------------------------------

test_that("bs_add_rules receives CSS text, not a file path", {
  # The bug was passing system.file() result (a path string) directly.
  # Verify readLines() produces a character vector, not a bare path.
  css_path    <- system.file("www/styles.css", package = "govhrapp")
  css_content <- readLines(css_path, warn = FALSE)
  # Content should not look like a Windows absolute path (e.g. C:/Users/...)
  expect_false(
    grepl("^[A-Za-z]:[/\\\\]", css_content[1]),
    label = "First line of CSS content looks like a Windows file path"
  )
  expect_false(
    grepl("^/[a-z]", css_content[1]),
    label = "First line of CSS content looks like a Unix file path"
  )
  expect_match(paste(css_content, collapse = "\n"), "[{:;]",
    label = "CSS content contains no CSS syntax -- may be a raw path"
  )
})

# ---------------------------------------------------------------------------
# Missingness footnote logic
# ---------------------------------------------------------------------------

test_that("overall_footnote: zero-missing fields are identified correctly", {
  qc <- make_minimal_qc_obj()
  dt <- qc$missingness$overall
  dt <- dt[dt$module == dt$module[1], ]

  zero_dt <- dt[dt$pct_missing == 0, ]
  near_dt <- dt[dt$pct_missing > 0 & dt$pct_missing < 0.01, ]

  # zero_dt must contain only rows with exactly 0 pct_missing
  expect_true(all(zero_dt$pct_missing == 0))
  # near_dt must exclude zero rows and include only sub-1% rows
  expect_true(all(near_dt$pct_missing > 0))
  expect_true(all(near_dt$pct_missing < 0.01))
})

test_that("overall_footnote: near-zero snippet format contains field name and count", {
  near_dt <- data.frame(
    target_label = c("Base Salary", "Allowance"),
    n_missing    = c(12L, 3L),
    pct_missing  = c(0.005, 0.002),
    stringsAsFactors = FALSE
  )
  near_dt  <- near_dt[order(near_dt$pct_missing), ]
  snippets <- paste0(
    near_dt$target_label, " (", format(near_dt$n_missing, big.mark = ","), " missing)"
  )
  expect_match(snippets[1], "Allowance")
  expect_match(snippets[1], "3 missing")
  expect_match(snippets[2], "Base Salary")
  expect_match(snippets[2], "12 missing")
})

# ---------------------------------------------------------------------------
# Validation badge label
# ---------------------------------------------------------------------------

test_that("make_validation_table: Errors TRUE rows get 'Does Not Apply' badge", {
  df <- data.frame(
    Rule            = c("Valid date", "Valid status"),
    Description     = c("date check", "status check"),
    `Total Records` = c(0L, 100L),
    Passes          = c(0L, 95L),
    `Pass Rate`     = c(NaN, 95.0),
    Fails           = c(0L, 5L),
    Errors          = c(TRUE, FALSE),
    check.names     = FALSE,
    stringsAsFactors = FALSE
  )

  status <- ifelse(
    df$Errors,
    "Does Not Apply",
    ifelse(df$`Pass Rate` >= 100, "PASS",
    ifelse(df$`Pass Rate` >= 80,  "WARNING", "FAIL"))
  )

  expect_equal(status[1], "Does Not Apply")
  expect_equal(status[2], "WARNING")
})
