test_that("run_govhrapp_analytics creates a shiny app object", {
  skip()
  # Test that the function returns a shiny.appobj
  app <- run_govhrapp_analytics()
  
  expect_s3_class(app, "shiny.appobj")
})