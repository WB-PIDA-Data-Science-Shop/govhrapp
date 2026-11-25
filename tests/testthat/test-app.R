test_that("shiny_app_with_login requires an existing credentials database", {
  ui <- shiny::fluidPage()
  server <- function(input, output, session) {}
  credentials_path <- tempfile(fileext = ".sqlite")
  on.exit(unlink(credentials_path), add = TRUE)

  expect_s3_class(
    shiny_app_with_login(ui, server, secure = FALSE, credentials_path),
    "shiny.appobj"
  )
  expect_error(
    shiny_app_with_login(ui, server, secure = TRUE, credentials_path),
    "No credentials database"
  )

  shinymanager::create_db(
    credentials_data = data.frame(user = "analyst", password = "secret"),
    sqlite_path = credentials_path
  )

  expect_s3_class(
    shiny_app_with_login(ui, server, secure = TRUE, credentials_path),
    "shiny.appobj"
  )
})
