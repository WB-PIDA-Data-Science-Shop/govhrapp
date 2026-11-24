#' Deploy an app to Posit Connect
#'
#' Deploys the analytics or quality check app to an existing Posit Connect
#' content item, whose GUID is read from an environment variable.
#'
#' @param suite Character. The app to deploy: `"analytics"` for the analytics
#'   app, or `"qcheck"` for the quality check app.
#' @param type Character. The content item to deploy to: `"dev"` (default) for
#'   the development copy, or `"prod"` for the one users see.
#'
#' @returns The value of [rsconnect::deployApp()].
#'
#' @details
#' Run it from the root of the govhrapp source, as it deploys the app script in
#' `inst/app/<suite>/`. The GUID of each content item is read from an
#' environment variable named `govhrapp_<suite>_<type>_guid`:
#'
#' * `govhrapp_analytics_dev_guid`
#' * `govhrapp_analytics_prod_guid`
#' * `govhrapp_qcheck_dev_guid`
#' * `govhrapp_qcheck_prod_guid`
#'
#' Set them in your `.Renviron` file or with [Sys.setenv()].
#'
#' @examples
#' \dontrun{
#' Sys.setenv(govhrapp_analytics_dev_guid = "your-guid-here")
#'
#' deploy_govhrapp("analytics")
#' deploy_govhrapp("analytics", type = "prod")
#' }
#'
#' @importFrom rsconnect deployApp
#' @export
deploy_govhrapp <- function(suite, type = c("dev", "prod")) {
  type <- match.arg(type)

  suite <- match.arg(suite, choices = c("qcheck", "analytics"))

  suite_type <- paste0(suite, "_", type)

  # Get the app ID from environment variable
  app_id = switch(
    suite_type,
    qcheck_dev = Sys.getenv("govhrapp_qcheck_dev_guid"),
    qcheck_prod = Sys.getenv("govhrapp_qcheck_prod_guid"),
    analytics_dev = Sys.getenv("govhrapp_analytics_dev_guid"),
    analytics_prod = Sys.getenv("govhrapp_analytics_prod_guid"),
    stop("suite must be either 'qcheck' or 'analytics' and type, 'prod' or 'dev'.")
  )
  
  # Get the app file to deploy
  app_primary_doc = switch(
    suite,
    qcheck = "inst/app/qcheck/qcheck_app.R",
    analytics = "inst/app/analytics/analytics_app.R"
  )
  
  # Check if app file exists
  if (!file.exists(app_primary_doc)) {
    stop("App file '", app_primary_doc, "' not found in package root directory")
  }
  
  # Deploy the app
  rsconnect::deployApp(
    appDir        = ".",
    appId         = app_id,
    appPrimaryDoc = app_primary_doc,
    server        = "internal-server",
    forceUpdate   = TRUE
  )
}