#' Examples of toolmark profiles
#'
#' Example data to try out the functions
#' @format list of four profiles, pr1, pr2, pr3, and pr4. Each profile is a data frame with one variable V1. Measurements are equidistant and ordered. 
#' pr1 and pr2 are known matches, all other pairs of profiles are known non-matches.
#' @examples
#' data(profiles)
#' plot(profiles$pr1$V1)
#' \dontrun{
#' chumbley_non_random(profiles$pr1, profiles$pr2)
#' 
#' res14 <- fixed_width_no_modeling(profiles$pr1, profiles$pr4)
#' res14$dist_pval
#' }
"profiles"