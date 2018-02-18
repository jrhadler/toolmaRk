library(reshape2)
library(ggplot2)
library(zoo)
library(plyr)
library(dplyr)
library(glmnet)
library(Matrix)
load('H:/MaxAngleStudy/Phd Thesis/marks_used_for_comaprison_distance_and_threshold_methods.RData')
source("H:/MaxAngleStudy/Phd Thesis/multiplot.r")

###Here we create a function that computes all possible correlations for windows of length 'window.size' between the class components
###Determine the location of the maximized correlation
###Given this location, create a diamond around it in the individual matrix of correlations (defined by M)
###For each offset in this diamond, compute the maximized correlation
###(1) Determine the distance between the offset for the class and indiviudal components
###(2) Compute the Threshold test statistics

fixed_width_with_modeling <- function(dat1, dat2, coarse = .25, fine = .01, window.size = .6, M = 500){
  ##dat1 is first toolmark
  ##dat2 is second toolmark
  ##coarse is class component smoothing parameter
  ##fine is individual component smoothing parameter
  ##window.size defines the size of the correlation windows (window.size*toolmarklength)
  ##M defines the size of the diamond shaped search region
  
  unity <- function(x) {x / sqrt(sum(x^2))} ##function to scale the matrices and give them unit vectors
  
  dat1 <- matrix(dat1[round(0.01*nrow(dat1)):round(0.99*nrow(dat1)),], ncol = 1)
  dat2 <- matrix(dat2[round(0.01*nrow(dat2)):round(0.99*nrow(dat2)),], ncol = 1)
  
  window <- round(min(nrow(dat1), nrow(dat2))*window.size)

  ##Smooth for 1-d Data
  y1 <- dat1 - lowess(y = dat1,  x = 1:nrow(dat1), f= coarse)$y
  y1smooth <- lowess(y = y1, x = 1:nrow(y1), f = fine)$y
  resid1 <- as.numeric(y1 - y1smooth)
  
  y2 <- dat2 - lowess(y = dat2,  x = 1:nrow(dat2), f= coarse)$y
  y2smooth <- lowess(y = y2, x = 1:nrow(y2), f = fine)$y
  resid2 <- as.numeric(y2 - y2smooth)
  
  ##################################################
  ##Correlations for the Class (smooth) components##
  ##################################################
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
  
  ##Rows in the following matrix are mark 2, columns are mark 1
  corr_mat_smooth <- t(smooth2_mat) %*% smooth1_mat
  
  ##Convert data to the long format with a column for row, column, and diagonal location 
  melt_corr_mat_smooth <- melt(corr_mat_smooth)
  names(melt_corr_mat_smooth) <- c("row", "col", "corr")
  melt_corr_mat_smooth$diag <- melt_corr_mat_smooth$row - melt_corr_mat_smooth$col
  
  melt_corr_mat_smooth_search_area <- dplyr::filter(melt_corr_mat_smooth, 
                                                    diag >= min(diag) + 2*M,
                                                    diag <= max(diag) - 2*M)
  ##Convert the columns to factors
  melt_corr_mat_smooth_search_area$row <- as.factor(melt_corr_mat_smooth_search_area$row)
  melt_corr_mat_smooth_search_area$col <- as.factor(melt_corr_mat_smooth_search_area$col)
  melt_corr_mat_smooth_search_area$diag <- as.factor(melt_corr_mat_smooth_search_area$diag)
  
  ##Subtract the overall mean from the data and fit a main effects model with row, column, and diagonal effects
  melt_corr_mat_smooth_search_area$centered <- as.numeric(scale(melt_corr_mat_smooth_search_area$corr, scale = FALSE))
  x <- cBind(as(matrix(melt_corr_mat_smooth_search_area$centered, ncol = 1), 'sparseMatrix'), sparse.model.matrix(centered ~ diag + row + col - 1, data = melt_corr_mat_smooth_search_area))
  m <- glmnet(x = x[,-1], y = x[,1], intercept = FALSE, standardize = FALSE, alpha = 1)
  #diag_smooth <- data.frame(diag = rownames(m$beta)[grep('diag', rownames(m$beta))], 
  #                          values = m$beta[grep('diag', rownames(m$beta)), ifelse(max(m$dev.ratio > 0.9), min(which(m$dev.ratio > 0.9)), which.max(m$dev.ratio))])
  diag_smooth <- data.frame(diag = rownames(m$beta)[grep('diag', rownames(m$beta))], 
                            values = m$beta[grep('diag', rownames(m$beta)), which.max(m$dev.ratio)])
  diag_smooth$diag <- as.integer(gsub('diag', '', diag_smooth$diag))
  max_diag_smooth <- as.numeric(diag_smooth[which.max(diag_smooth$values), 1])
  
  penalty <- m$lambda[which.max(m$dev.ratio)]
  
  ##Remove big space takers
  rm(x,m)
  
  ##Convert the columns of melt_corr_mat_smooth back to numeric
  melt_corr_mat_smooth_search_area$row <- as.numeric(as.character(melt_corr_mat_smooth_search_area$row))
  melt_corr_mat_smooth_search_area$col <- as.numeric(as.character(melt_corr_mat_smooth_search_area$col))
  melt_corr_mat_smooth_search_area$diag <- as.numeric(as.character(melt_corr_mat_smooth_search_area$diag))
  
  max_diag_locations <- dplyr::filter(melt_corr_mat_smooth_search_area, diag == max_diag_smooth)
  max_diag_locations_mid_point <- max_diag_locations[which(max_diag_locations$row == round(quantile(max_diag_locations$row, 0.5))),1:2]
    
  p1 <- ggplot() + 
    coord_fixed(ratio = 1) +
    xlab("s") + ylab("t") + 
    ggtitle("Class") +
    geom_raster(data = melt_corr_mat_smooth, aes(x = col, y = row, fill = corr)) +
    geom_segment(data = melt_corr_mat_smooth, aes(x = 1, y = max(row) - 2*M, xend = 2*M, yend = max(row)), size = 1) +
    geom_segment(data = melt_corr_mat_smooth, aes(x = max(col) - 2*M, y = 1, xend = max(col), yend = 2*M), size = 1) +
    geom_abline(intercept = max_diag_smooth, slope = 1, size = 1, lty = 2) + 
    geom_point(data = max_diag_locations_mid_point, aes(x = col, y = row), size = 2) +
    scale_fill_gradientn(name = "Correlation", colours = rainbow(16)) + 
    theme(axis.title = element_text(size = 13),
          title = element_text(size = 13))
  
  
  ##Corrdinates that create a diamond around the maximized location
  indices <- expand.grid(row = 1:nrow(corr_mat_smooth),
                         col = 1:ncol(corr_mat_smooth))
  ##Create the diamond search window
  diamond <- dplyr::filter(indices, 
                           abs(col - max_diag_locations_mid_point$col) <= M,
                           abs(row - max_diag_locations_mid_point$row) <= (M - abs(col - max_diag_locations_mid_point$col)))
  ##Add a data point to the bottom of each column on the left side of center
  diamond_left_of_center <- dplyr::filter(diamond, col - max_diag_locations_mid_point$col < 0)
  diamond_added_points <- ddply(diamond_left_of_center, .(col), summarise, row = min(row) - 1)
  diamond <- as.matrix(rbind(diamond, diamond_added_points))
  
  ###############################################################################
  ##Correlations for the residuals at the locations in the diamond just defined##
  ###############################################################################
  resid1_mat <- matrix(NA, ncol = length(1:(length(resid1) - (window - 1))), nrow = window)
  for(i in 1:(length(resid1) - (window - 1))){
    resid1_mat[,i] <- resid1[i:(i+(window - 1))]
  }
  resid2_mat <- matrix(NA, ncol = length(1:(length(resid2) - (window - 1))), nrow = window)
  for(i in 1:(length(resid2) - (window - 1))){
    resid2_mat[,i] <- resid2[i:(i+(window - 1))]
  }
  
  ##Rows in the following matrix are mark 2, columns are mark 1
  resid2_mat <- apply(scale(resid2_mat), 2, unity)
  resid1_mat <- apply(scale(resid1_mat), 2, unity)
  
  ##Convert the correlation matrix to the long format so it can be plotted and pull out just the elements in the diamond region
  corr_mat_resid <- t(resid2_mat) %*% resid1_mat
  melt_corr_mat_resid <- melt(corr_mat_resid)
  names(melt_corr_mat_resid) <- c('row', 'col', 'corr')
  melt_diamond <- merge(diamond, melt_corr_mat_resid, by = c('row', 'col'))
  
  ##Define the corners of the search diamond for easy plotting
  search_area <- data.frame(x = c(max_diag_locations_mid_point$col - M, max_diag_locations_mid_point$col, max_diag_locations_mid_point$col + M, max_diag_locations_mid_point$col, max_diag_locations_mid_point$col - M),
                            y = c(max_diag_locations_mid_point$row, max_diag_locations_mid_point$row + M, max_diag_locations_mid_point$row, max_diag_locations_mid_point$row - M, max_diag_locations_mid_point$row))
  
  ##Plot the correlations for the diamond region
  p2 <- ggplot() + 
    coord_fixed(ratio = 1) +
    xlab("s") + ylab("t") +    
    ggtitle(paste("Individual")) +
    geom_raster(data = melt_corr_mat_resid, aes(x = col, y = row, fill=corr)) +
    geom_point(data = max_diag_locations_mid_point, aes(x = col, y = row), size = 1.5) +
    geom_path(data = search_area, aes(x = x, y = y), size = 1) +
    scale_fill_gradientn(name = "Correlation", colours = rainbow(16)) + 
    theme(axis.title = element_text(size = 13),
          title = element_text(size = 13))
  #multiplot(p1,p2, cols = 2)
  
  ##Pull out the data corresponding to locations within the diamond
  melt_diamond$diag <- melt_diamond$row - melt_diamond$col

  ##Fit the model to the data within the diamond using row, column, and diagonal main effects
  melt_diamond$row <- as.factor(melt_diamond$row)
  melt_diamond$col <- as.factor(melt_diamond$col)
  melt_diamond$diag <- as.factor(melt_diamond$diag)
  
  ##Subtract the overall mean from the data and fit a main effects model with row, column, and diagonal effects
  melt_diamond$centered <- as.numeric(scale(melt_diamond$corr, scale = FALSE))
  x <- cBind(as(matrix(melt_diamond$centered, ncol = 1), 'sparseMatrix'), sparse.model.matrix(centered ~ diag + row + col - 1, data = melt_diamond))
  m <- glmnet(x = x[,-1], y = x[,1], intercept = FALSE, standardize = FALSE, alpha = 1)
  #diag_resid <- data.frame(diag = rownames(m$beta)[grep('diag', rownames(m$beta))], 
  #                         values = m$beta[grep('diag', rownames(m$beta)), ifelse(max(m$dev.ratio > 0.9), min(which(m$dev.ratio > 0.9)), which.max(m$dev.ratio))])
  diag_resid <- data.frame(diag = rownames(m$beta)[grep('diag', rownames(m$beta))], 
                           values = m$beta[grep('diag', rownames(m$beta)), which.max(m$dev.ratio)])
  diag_resid$diag <- as.integer(gsub('diag', '', diag_resid$diag))
  max_diag_resid <- as.numeric(diag_resid[which.max(diag_resid$values), 1])
  
  penalty <- m$lambda[which.max(m$dev.ratio)]
  
  ##Distance P-value
  dist_pval <- round((abs(max_diag_smooth - max_diag_resid) + 0.5) / (M + 0.5), 4)
  
  ##Threshold P-value
  thresh <- diag_resid$values[which(diag_resid$diag == max_diag_smooth)]
  thresh_pval <- round((sum(diag_resid$values[-which(diag_resid$diag == max_diag_smooth)] >= thresh) + 1) / (2 * M + 1), 4)
  
    p3 <- ggplot() + 
        xlab("Offset") + ylab(expression(gamma)) + ggtitle("Individual Correlation Function") +
        geom_line(aes(x = diag_resid$diag, y = diag_resid$values), colour = I("blue"), size = 1) +
        geom_vline(xintercept = as.numeric(max_diag_smooth), colour = I("black"), size = 1, lty = 2) +
        annotate("text", x = -2250, y = 0.20, label = paste("h(c)==", max_diag_smooth), parse = TRUE, size = 4) +
        annotate("text", x = -2250, y = 0.22, label = paste(" +                  "), size = 4) + 
        annotate("text", x = -1700, y = 0.20, label = paste("h(r)==", max_diag_resid), parse = TRUE, size = 4) +
        annotate("text", x = -1700, y = 0.22, label = paste(" +                  "), size = 4) + 
        theme(title = element_text(size = 14), 
                         axis.title = element_text(size = 14))
  
      p4 <- ggplot() + xlab("Offset") + ylab(expression(gamma)) + 
          ggtitle("Individual Correlation Function") + 
          geom_line(aes(x = diag_resid$diag, y = diag_resid$values), colour = I("blue"), size = 1) + 
          geom_vline(xintercept = as.numeric(max_diag_smooth), colour = I("black"), size = 1, lty = 2) + 
          geom_hline(yintercept = thresh, colour = I("black"), size = 1, lty= 2) +
          annotate("text", x = -1700, y = 0.25, label = paste("h(c) ==", max_diag_smooth), parse = TRUE, size = 4) +
          annotate("text", x = -1700, y = 0.27, label = paste(" +                  "), size = 4) +
          annotate("text", x = -1700, y = 0.20, label = paste("Threshold ==", round(thresh, 4)), parse = TRUE, size = 4) +
          theme(title = element_text(size = 14),
                           axis.title = element_text(size = 14))
  
  return(list(max_corr = round(diag_resid$values[which(diag_resid$diag == max_diag_resid)], 4),
              Smooth_offset = max_diag_smooth, 
              Resid_offset = max_diag_resid, 
              dist_pval = as.numeric(dist_pval), 
              total_dist = M + 0.5,
              thresh_pval = thresh_pval, 
              Above = sum(diag_resid$values[-which(diag_resid$diag == max_diag_smooth)] >= thresh), 
              total_thresh = 2*M + 1))
}
