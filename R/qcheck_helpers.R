#############################################################################################
########## SOME FUNCTIONS THAT CUT ACROSS MODULES AND INTERFACES OF THE QCHECK APP ##########
#############################################################################################

# (Helper functions now live in govhr package or in qcheck_internal_helpers.R)

#' a function to compute the proportion of missing values in a data frame
#' @param data A data frame.
#' @param digits An integer specifying the number of decimal places to round the result to. Default is 2.
#' @return A numeric value representing the proportion of missing values in the data frame.
compute_global_coverage <- function(data, digits = 2) {
  coverage <- 100 * mean(!is.na(data)) 
  
  coverage |> 
    round(digits)
}