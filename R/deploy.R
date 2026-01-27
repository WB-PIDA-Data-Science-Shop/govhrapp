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
deploy_govhrapp <- function(suite){
  app_id = switch(
    suite,
    quality = Sys.getenv("govhrapp_quality_guid"),
    analytics = Sys.getenv("govhrapp_analytics_guid")
  )

  rsconnect::deployApp(
    appId = app_id
  )
}