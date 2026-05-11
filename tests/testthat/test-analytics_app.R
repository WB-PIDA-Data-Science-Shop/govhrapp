test_that("run_govhrapp creates a shiny app object", {
  # Test that the function returns a shiny.appobj
  source(
    system.file("R/global.R", package = "govhrapp")
  )

  app <- run_govhrapp(personnel_data)
  
  expect_s3_class(app, "shiny.appobj")
})