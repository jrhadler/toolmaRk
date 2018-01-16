#' Toolmark profiles dataset
#' 
#' Data set of toolmarks (profiles) created by screwdrivers under different angles. 
#' Tool mark data included here were produced by Prof. Scott Chumbley, Mr. Stephen Davis, Ms. Taylor Grieve, Mr. Ryan Spotts, and Dr. Jeremy Hadler. These data were produced as part of research performed at the Ames Laboratory, located on the Iowa State University campus. Ames Laboratory is operated for the U.S. Department of Energy by Iowa State University under Contract No. DE-Ac02-07CH11358.
#' @format the dataset consists of a sample of 16 toolmark profiles and descriptors. Toolmarks with the same toolmark identifier are known matches, all other profiles are known non-matches.
#' \describe{
#'   \item{ID}{toolmark identifier. Factor variable.}
#'   \item{side}{A or B, indicates the side of the screwdriverused to create the toolmark. Factor variable. }
#'   \item{angle}{degree under which the toolmark was created.}
#'   \item{rep}{replicate number for a toolmark, side, angle composition. Note that the data here provided is not complete. For a more complete data set or more information please contact the references given below.}
#'   \item{profile}{list of data sets with one profile each. Measurements are taken at equispaced intervals across the toolmark. .}
#' }
#' @references 
#' Tool mark data included here were produced by Prof. Scott Chumbley, Mr. Stephen Davis, Ms. Taylor Grieve, Mr. Ryan Spotts, and Dr. Jeremy Hadler.  These data were produced as part of research performed at the Ames Laboratory, located on the Iowa State University campus. Ames Laboratory is operated for the U.S. Department of Energy by Iowa State University under Contract No. DE-Ac02-07CH11358.
#' @examples
#' \dontshow{
#'   data(ameslab)
#'   plot(ameslab$profile[[1]]$V1)
#'   chumbley_non_random(data.frame(ameslab$profile[[1]][1:2000,]), data.frame(ameslab$profile[[2]][1:2000,]))
#'
#'   res14 <- fixed_width_no_modeling(data.frame(ameslab$profile[[1]][1:1250,]), data.frame(ameslab$profile[[4]][1:1250,]), M = 200)
#'   res14$dist_pval
#'  }
#' \donttest{
#'   data(ameslab)
#'   plot(ameslab$profile[[1]]$V1)
#'   chumbley_non_random(ameslab$profile[[1]], ameslab$profile[[2]])
#'
#'   res14 <- fixed_width_no_modeling(ameslab$profile[[1]], ameslab$profile[[4]])
#'   res14$dist_pval
#'  }
"ameslab"
