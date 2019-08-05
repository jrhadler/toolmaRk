#' Distance/threshold test for toolmarks
#' 
#' Compute all possible correlations for windows of length n between the class components.
#' Determine the location of the maximized correlation.
#' Given this location, create a diamond around it in the individual matrix of correlations
#' For each offset in this diamond, compute the maximized correlation
#' (1) Determine the distance between the offset for the class and indiviudal components
#' (2) Compute the Threshold test statistics
#' @param dat1 a one column matrix representing a digitized tool mark
#' @param dat2 a one column matrix representing a second digitized tool mark
#' @param coarse normalization smoothing parameter
#' @param fine decomposition smoothing parameter
#' @param window.size desired window size for the correlations to compute 
#' @param M search area restriction
#' @importFrom dplyr filter group_by summarise 
#' @importFrom stats lowess
#' @importFrom ggplot2 aes xlab ylab geom_vline geom_hline coord_fixed element_text ggtitle
#' @importFrom ggplot2 geom_line theme geom_raster geom_path geom_point scale_fill_gradient
#' @export
#' @return list with 
#' \itemize{
#'  \item{"max_corr"}{maximized indiviudal component correlation}
#'  \item{"Smooth_offset"}{optimal Class offset}
#'  \item{"Resid_offset"}{optimal individual offset}
#'  \item{"dist_pval"} distance p-value
#'  \item{"thresh_pval"} threshold p-value
#'  \item{"Above"} Number of offsets with correlation bigger than threshold
#'  \item{"total_thresh"} 2*M+1
#'  \item{"mark1_decompostion"} plot of decomposition d1
#'  \item{"mark2_decompostion"} plot of decomposition d2
#'  \item{"class_correlations"} plot of class correlation
#'  \item{"individual_correlations"} plot of individual correlation
#'  \item{"distance_plot"} distance_plot
#'  \item{"threshold_plot"} threshold_plot
#'  }
fixed_width_no_modeling <- function(dat1, dat2, coarse = .25, fine = .01, window.size = .6, M = 500){
## initialize variables
  x <- y <- corr <- NULL
  
#  source("F:/MaxAngleStudy/Phd Thesis/multiplot.r")
  
  unity <- function(x) {x / sqrt(sum(x^2))} ##function to scale the columns of a matrix and give them unit vectors
  
  dat1 <- matrix(dat1[round(0.01*nrow(dat1)):round(0.99*nrow(dat1)),], ncol = 1)
  dat2 <- matrix(dat2[round(0.01*nrow(dat2)):round(0.99*nrow(dat2)),], ncol = 1)
  
  window <- round(min(nrow(dat1), nrow(dat2))*window.size)
  
  ##normalize and decompose the tool marks
  y1 <- dat1 - stats::lowess(y = dat1,  x = 1:nrow(dat1), f= coarse)$y
  y1smooth <- stats::lowess(y = y1, x = 1:nrow(y1), f = fine)$y
  resid1 <- as.numeric(y1 - y1smooth)
  
  y2 <- dat2 - stats::lowess(y = dat2,  x = 1:nrow(dat2), f= coarse)$y
  y2smooth <- stats::lowess(y = y2, x = 1:nrow(y2), f = fine)$y
  resid2 <- as.numeric(y2 - y2smooth)
  
  ##Plot the decompositions
  d1 <- ggplot(aes(x = x, y= y), data = NULL) + xlab("Index") + ylab("Depth") + ggtitle("Normalized Mark Decomposition") + 
    geom_line(data=data.frame(x = 1:length(as.vector(y1)), y = as.vector(y1)+15), size = 1) + 
    geom_line(data=data.frame(x = (1:length(y1smooth)), y = y1smooth - 3), size = 1, colour = 'dimgrey') + 
    geom_line(data=data.frame(x = (1:length(resid1)), y = resid1 - 20), size = 1, colour = 'darkgray') + 
    theme(plot.title = element_text(hjust = 0.5))
  
  d2 <- ggplot(aes(x = x, y=y), data = NULL) + xlab("Index") + ylab("Depth") + ggtitle("Normalized Mark Decomposition") + 
    geom_line(data=data.frame(x = 1:length(y2), y = y2+15), size = 1) + 
    geom_line(data=data.frame(x = (1:length(y2smooth)), y = y2smooth - 3), size = 1, colour = 'dimgrey') + 
    geom_line(data=data.frame(x = (1:length(resid2)), y = resid2 - 20), size = 1, colour = 'darkgray') + 
    theme(plot.title = element_text(hjust = 0.5))
  
  ##################################################
  ##Correlations for the Class (smooth) components##
  ##################################################
  ##create matrices where each column corresponds to a window in the tool mark for the class components
  smooth1_mat <- matrix(NA, ncol = length(1:(length(y1smooth) - (window - 1))), nrow = window)
  for(i in 1:(length(y1smooth) - (window - 1))){
    smooth1_mat[,i] <- y1smooth[i:(i+(window - 1))]
  }
  smooth2_mat <- matrix(NA, ncol = length(1:(length(y2smooth) - (window - 1))), nrow = window)
  for(i in 1:(length(y2smooth) - (window - 1))){
    smooth2_mat[,i] <- y2smooth[i:(i+(window - 1))]
  }
  
  ##standardize these matrices and scale them to have length 1 to make correlation computation more efficient
  smooth2_mat <- apply(scale(smooth2_mat), 2, unity)
  smooth1_mat <- apply(scale(smooth1_mat), 2, unity)
  
  ##Compute the correlations between all pairs of windows
  ##Rows in the following matrix are mark 2, columns are mark 1
  corr_mat_smooth <- t(smooth2_mat) %*% smooth1_mat
  browser()
  ##Melt the matrix to three columns
  melt_corr_mat_smooth <- as.data.frame.table(corr_mat_smooth)
  names(melt_corr_mat_smooth) <- c('row', 'col', 'corr')
  melt_corr_mat_smooth$row <- as.integer(melt_corr_mat_smooth$row)
  melt_corr_mat_smooth$col <- as.integer(melt_corr_mat_smooth$col)
  
  ##Determine the pair of windows resulting in the maximized correlation
  ##Eliminate the first and last M columns from consideration
  melt_corr_mat_smooth_search_area <- dplyr::filter(melt_corr_mat_smooth, 
                                                    row > M,
                                                    col > M,
                                                    row <= nrow(corr_mat_smooth) - M,
                                                    col <= ncol(corr_mat_smooth) - M)
  
  max_corr_smooth <- melt_corr_mat_smooth_search_area[which(melt_corr_mat_smooth_search_area$corr == max(melt_corr_mat_smooth_search_area$corr)),]
  max_corr_smooth_offset <- max_corr_smooth$row - max_corr_smooth$col
  
  
  ##Define the corners of the search window for easy plotting
  search_area <- data.frame(x = c(M+1, ncol(corr_mat_smooth) - M, ncol(corr_mat_smooth) - M, M+1, M+1),
                            y = c(M+1, M+1, nrow(corr_mat_smooth) - M, nrow(corr_mat_smooth) - M, M+1))
  
  ##Plot the correlation matrix for the class components.
  p1 <- ggplot2::ggplot() + 
    coord_fixed(ratio = 1) +
    xlab("s") + ylab("t") + 
    ggtitle("Class") +
    geom_raster(data = melt_corr_mat_smooth, aes(x = col, y = row, fill=corr)) +
    geom_path(data = search_area, aes(x = x, y = y), size = 1) +
    geom_point(data = max_corr_smooth, aes(x = col, y = row), size = 1.5) +
    scale_fill_gradient(name = "Correlation", high = 'black', low = 'lightgray') + 
    theme(axis.title = element_text(size = 13),
          title = element_text(size = 13),
          plot.title = element_text(hjust = 0.5))
  
  ##Coordinates that create a diamond around the maximized location
  indices <- expand.grid(row = 1:nrow(corr_mat_smooth),
                         col = 1:ncol(corr_mat_smooth))
  
  ##Create the diamond search window
  diamond <- dplyr::filter(indices, 
                           abs(col - max_corr_smooth$col) <= M,
                           abs(row - max_corr_smooth$row) <= (M - abs(col - max_corr_smooth$col)))
  
  ##Add a data point to the bottom of each column on the left side of center
  diamond_left_of_center <- dplyr::filter(diamond, col - max_corr_smooth$col < 0)
  # diamond_added_points <- diamond_left_of_center %>%
  #   group_by(col) %>%
  #   summarise(
  #     row = min(row) - 1
  #     )
  ## line below is equivalent to the above statement without using the pipe operator
  diamond_added_points <- summarise(group_by(diamond_left_of_center, col), row = min(row) - 1)
  
  diamond <- as.matrix(rbind(diamond, diamond_added_points))
  
  ###############################################################################
  ##Correlations for the residuals at the locations in the diamond just defined##
  ###############################################################################
  ##Create matrices where each column is a window in the individual components
  resid1_mat <- matrix(NA, ncol = length(1:(length(resid1) - (window - 1))), nrow = window)
  for(i in 1:(length(resid1) - (window - 1))){
    resid1_mat[,i] <- resid1[i:(i+(window - 1))]
  }
  resid2_mat <- matrix(NA, ncol = length(1:(length(resid2) - (window - 1))), nrow = window)
  for(i in 1:(length(resid2) - (window - 1))){
    resid2_mat[,i] <- resid2[i:(i+(window - 1))]
  }
  
  ##Compute the correlations
  ##Rows in the following matrix are mark 2, columns are mark 1
  resid2_mat <- apply(scale(resid2_mat), 2, unity)
  resid1_mat <- apply(scale(resid1_mat), 2, unity)
  
  ##Convert the correlation matrix to the long format so it can be plotted and pull out just the elements in the diamond region
  corr_mat_resid <- t(resid2_mat) %*% resid1_mat
  melt_corr_mat_resid <- as.data.frame.table(corr_mat_resid)
  names(melt_corr_mat_resid) <- c('row', 'col', 'corr')
  melt_corr_mat_resid$row <- as.integer(melt_corr_mat_resid$row)
  melt_corr_mat_resid$col <- as.integer(melt_corr_mat_resid$col)
  
  melt_diamond <- merge(diamond, melt_corr_mat_resid, by = c('row', 'col'))
  
  ##Define the corners of the search diamond for easy plotting
  search_area <- data.frame(x = c(max_corr_smooth$col - M, max_corr_smooth$col, max_corr_smooth$col + M, max_corr_smooth$col, max_corr_smooth$col - M),
                            y = c(max_corr_smooth$row, max_corr_smooth$row + M, max_corr_smooth$row, max_corr_smooth$row - M, max_corr_smooth$row))
  
  ##Plot the correlation matrices for the individual components
  p2 <- ggplot2::ggplot() + 
    coord_fixed(ratio = 1) +
    xlab("s") + ylab("t") +    
    ggtitle(paste("Individual")) +
    geom_raster(data = melt_corr_mat_resid, aes(x = col, y = row, fill=corr)) +
    geom_point(data = max_corr_smooth, aes(x = col, y = row), size = 1.5) +
    geom_path(data = search_area, aes(x = x, y = y), size = 1) +
    scale_fill_gradient(name = "Correlation", high = 'black', low = 'lightgray') + 
    theme(axis.title = element_text(size = 13),
          title = element_text(size = 13),
          plot.title = element_text(hjust = 0.5))
  
  ##Determine the offset at each location and compute the max correlation along each offset
  melt_diamond$diag <- melt_diamond$row - melt_diamond$col
  # melt_diamond_summary <- melt_diamond %>% group_by(diag) %>%
  #   summarise(
  #     max = max(corr), 
  #     count = length(corr))
  ## w/o the pipe this becomes less readable:
  melt_diamond_summary <- summarise(group_by(melt_diamond, diag), 
                                    max = max(corr), count = length(corr))
  
  max_melt_diamond_summary <- melt_diamond_summary[which.max(melt_diamond_summary$max), ]
  
  ##Distance P-value
  dist_pval <- round((abs(max_corr_smooth_offset - max_melt_diamond_summary[1]) + 0.5) / (M + 0.5), 4)
  ##Threshold P-value
  thresh <- melt_diamond_summary$max[which(melt_diamond_summary$diag == max_corr_smooth_offset)]
  thresh_pval <- round((sum(melt_diamond_summary$max[-which(melt_diamond_summary$diag == max_corr_smooth_offset)] >= thresh) + 1) / (2 * M + 1), 4)
  
  ##Plot the maximized correlations within the diamond 
  p3 <- ggplot2::ggplot() + 
    xlab("Offset") + ylab(expression(gamma)) + ggtitle("Individual Correlation Function") +
    geom_line(aes(x = melt_diamond_summary$diag, y = melt_diamond_summary$max), size = 1) +
    geom_vline(xintercept = as.numeric(max_corr_smooth_offset), colour = I("black"), size = 1, lty = 2) +
    theme(title = element_text(size = 14),
          axis.title = element_text(size = 14),
          plot.title = element_text(hjust = 0.5))
  
  
  ##Plot the maximized correlatins within the diamond with the threshold depicted
  p4 <- ggplot2::ggplot() + xlab("Offset") + ylab(expression(gamma)) + 
    ggtitle("Individual Correlation Function") + 
    geom_line(aes(x = melt_diamond_summary$diag, y = melt_diamond_summary$max), size = 1) + 
    geom_vline(xintercept = as.numeric(max_corr_smooth_offset), colour = I("black"), size = 1, lty = 2) + 
    geom_hline(yintercept = thresh, colour = I("black"), size = 1, lty= 2) +
    theme(title = element_text(size = 14),
          axis.title = element_text(size = 14),
          plot.title = element_text(hjust = 0.5))
  
  
  return(list(max_corr = round(as.numeric(max_melt_diamond_summary[2]), 4), ##maximized indiviudal component correlation
              Smooth_offset = max_corr_smooth_offset, ##optimal Class offset
              Resid_offset = as.numeric(max_melt_diamond_summary[1]), ##optimal indiviudal offset
              dist_pval = as.numeric(dist_pval), ##distance p-value
              thresh_pval = thresh_pval, ##threshold p-value
              Above = sum(melt_diamond_summary$max[-which(melt_diamond_summary$diag == max_corr_smooth_offset)] >= thresh), ##Number of offsets with correlation bigger than threshold
              total_thresh = 2*M + 1,
              mark1_decomposition = d1,
              mark2_decomposition = d2, 
              class_correlations = p1,
              individual_correlations = p2,
              distance_plot = p3,
              threshold_plot = p4)) ##total offsets considered
}

