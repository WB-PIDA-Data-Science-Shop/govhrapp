#' Deploy govhrapp to Posit Connect
#'
#' Deploys the govhrapp Shiny application to Posit Connect using stored
#' application GUIDs from environment variables.
#'
#' @param suite Character string specifying which app suite to deploy.
#'   Must be one of:
#'   \describe{
#'     \item{\code{"quality"}}{Deploys the quality control dashboard.
#'       Uses the \code{govhrapp_quality_guid} environment variable.}
#'     \item{\code{"analytics"}}{Deploys the analytics dashboard.
#'       Uses the \code{govhrapp_analytics_guid} environment variable.}
#'   }
#' @param type Character string specifying the deployment type ("dev" or "prod").
#'
#' @return Invisibly returns the deployment information from
#'   \code{\link[rsconnect]{deployApp}}.
#'
#' @details
#' This function requires environment variables to be set with the Posit Connect
#' application GUIDs:
#' \itemize{
#'   \item \code{govhrapp_quality_guid} - GUID for the quality suite
#'   \item \code{govhrapp_analytics_guid} - GUID for the analytics suite
#' }
#'
#' These can be set in your \code{.Renviron} file or through
#' \code{Sys.setenv()}.
#'
#' @examples
#' \dontrun{
#' # Set environment variables first
#' Sys.setenv(govhrapp_analytics_guid = "your-guid-here")
#'
#' # Deploy the analytics suite
#' deploy_govhrapp("analytics")
#'
#' # Deploy the quality suite
#' deploy_govhrapp("quality")
#' }
#'
#' @importFrom rsconnect deployApp
#' @export
deploy_govhrapp <- function(suite, type = c("dev", "prod")) {
  type <- match.arg(type)

  suite <- match.arg(suite, choices = c("quality", "analytics"))

  suite_type <- paste0(suite, "_", type)

  # Get the app ID from environment variable
  app_id = switch(
    suite_type,
    quality_dev = Sys.getenv("govhrapp_quality_dev_guid"),
    quality_prod = Sys.getenv("govhrapp_quality_prod_guid"),
    analytics_dev = Sys.getenv("govhrapp_analytics_dev_guid"),
    analytics_prod = Sys.getenv("govhrapp_analytics_prod_guid"),
    stop("suite must be either 'quality' or 'analytics' and type, 'prod' or 'dev'.")
  )
  
  # Get the app file to deploy
  app_primary_doc = switch(
    suite,
    quality = "inst/app/qcheck/qcheck_app.R",
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