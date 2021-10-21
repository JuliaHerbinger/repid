# Helper functions

# prepare grouped ICE curve dataframe from VINE in suitable format to plot
helper_VINEICEPrep <- function(df_name){
  df = read.csv2(df_name, sep=",", dec=".")
  df = df[,!colnames(df)=="points"]
  df2 = melt(df, id = c("X", "cluster_label"))
  df2$variable = as.character(df2$variable)
  df2$variable = str_replace(df2$variable,"X","")
  df2$variable = str_replace(df2$variable,fixed(".0."),"-0.")
  df2$variable = as.numeric(df2$variable)
  df2$X = as.factor(df2$X)
  df2$cluster_label = as.factor(df2$cluster_label)
  return(df2)
}

# write csv file with ICE curves df to use it in VINE
helper_VINEICEWrite <- function(featureEffects, df_name){
  ice_data = featureEffects$results[[1]]
  grid = subset(ice_data, .id ==1)$.borders
  ice_data = ice_data[,-c(3,5)]
  ice_data_transformed = reshape(ice_data, idvar = ".id", timevar = ".borders", direction = "wide")
  ice_data_transformed = ice_data_transformed[,-1]
  write.csv2(ice_data_transformed, df_name)
}

# Frechet distance measure (sum of squares)
# SS_fre = function(y, x, requires.x = FALSE) { # slow
#   # using only y-axis of curves is enough as x-axis is always the same for all curves
#   require(kmlShape)
#   center = colMeans(y)
#   grid.x = as.numeric(names(center))
#   pdp.y = unname(center)
#   dist = apply(y, 1, function(ice) distFrechet(grid.x, pdp.y, grid.x, ice, FrechetSumOrMax = "sum"))
#   sum(dist)
# }

library(dplyr)
library(reshape2)
library(stringr)
library(ggplot2)
library(tidyverse)
library(Rmalschains)
library(iml)
library(ranger)
library(kmlShape)
library(dtw)
library(tidyr)

# # Frechet distance FDA measure
# SS_fre = function(y, x, requires.x = FALSE, ...) { # slow
#   # using only y-axis of curves is enough as x-axis is always the same for all curves
#   require(kmlShape)
#   center = colMeans(y)
#   grid.x = as.numeric(names(center))
#   pdp.y = unname(center)
#   dist = apply(y, 1, function(ice) distFrechet(grid.x, pdp.y, grid.x, ice, FrechetSumOrMax = "sum"))
#   sum(dist)
# }
# 
# # Frechet distance measure - with filtered ice curves
# SS_fre_filtered = function(y, x, sub.number, requires.x = FALSE, feat, x.all, ...) {
#   require(kmlShape)
#   # use only ice curves that are available for the combination of the two features -> no extrapolation
#   indices = filter(feat, x.all, y, sub.number)
#   y.filtered = y[,indices, drop = FALSE]
#   center = colMeans(y.filtered)
#   grid.x = as.numeric(names(center))
#   pdp.y = unname(center)
#   dist = apply(y.filtered, 1, function(ice) distFrechet(grid.x, pdp.y, grid.x, ice, FrechetSumOrMax = "sum"))
#   sum(dist)*20/length(indices)
# }

SS_L2 = function(y, x, requires.x = FALSE, ...) { 
  #require(Rfast)
  ypred = colMeans(as.matrix(y)) 
  sum(t((t(y) - ypred)^2))
}

SS_fre = function(y, x, requires.x = FALSE, ...) { 
  #require(Rfast)
  ypred = apply(y, 2, median) #Rfast::colMedians(as.matrix(y))
  sum(t(abs(t(y) - ypred)))
}

# Frechet distance measure - with filtered ice curves
SS_fre_filtered = function(y, x, sub.number, requires.x = FALSE, feat, x.all, ...) {
  ycols = ncol(y)
  # use only ice curves that are available for the combination of the two features -> no extrapolation
  indices = filter(feat, x.all, y, sub.number)
  y.filtered = y[,indices, drop = FALSE]
  
  ypred = apply(y.filtered, 2, median) #Rfast::colMedians(as.matrix(y))
  dist = sum(t(abs(t(y.filtered) - ypred)))
  
  sum(dist)*ycols/length(indices)
}

# NEW:
# Filter function to use only ice curves within grid points to find best split point
# not yet included: categorical features (only numeric and one-hot-encoded)
# needs to be integrated in objective
filter = function(feat, x.all, Y, sub.number){
  values = unique(x.all[sub.number,feat])
  if (length(unique(x.all[,feat])) > 2) {
    grid.points = as.numeric(names(Y))
    break.points = grid.points[1:(length(grid.points) - 1)] + (grid.points[2:length(grid.points)] - grid.points[1:(length(grid.points) - 1)]) / 2
    range = cut(values, breaks = c(min(x.all[,feat]), break.points, max(x.all[,feat])), labels = c(names(Y)), include.lowest = TRUE, right = TRUE)
    return(which(names(Y) %in% unique(range)))
  }
  else if (length(unique(x.all[,feat])) == length(values)) {
    return(c(1:length(names(Y))))
  }
  else if (length(values) == 1 & length(unique(x.all[,feat])) == 2) {
    if (values < mean(unique(x.all[,feat]))) {
      return(c(1:round(ncol(Y)/2,0)))
    }
    else if (values > mean(unique(x.all[,feat]))) {
      return(c(round(ncol(Y)/2,0):ncol(Y)))
    }
  }
  
}

#------------------------------------------------------------------------------------------------------------
# functions for plotting


# get ice curves function for plotting
get_ice_curves <- function(Y, X, result, extrapol = TRUE){
  assert_data_table(result)
  # TODO: fix bug if more than one feature have the same best objective
  feature = unique(result$feature[result$best.split])
  split.points = unlist(result$split.points[result$best.split])
  split.points = sort.int(split.points)
  node.number = findInterval(x = X[,feature], split.points, rightmost.closed = TRUE) + 1
  
  # split y according to node.number
  y.list = split(Y, node.number)
  
  # ice curve feature
  feat = colnames(X)[which(!(colnames(X) %in% result$feature))]
  
  #filter ice curves in case extrapol = TRUE
  if (extrapol == TRUE) {
    y.list.filtered = lapply(seq_along(y.list), FUN = function(i) {
      ind = filter(feat, X, Y, which(node.number == i))
      y.list[[i]][,ind, drop = FALSE]
    })
  }
  else y.list.filtered = y.list
  
  return(y.list.filtered)
}


# prepare data for plotting
plot.prep.ind = function(i, x){
  x$.id = 1:nrow(x)
  x = gather(x, .borders, .value, colnames(x)[1]:colnames(x)[ncol(x) - 1], factor_key = TRUE)
  x$.split = i
  return(x)
}

plot.prep.full = function(data){
  data.prep = lapply(seq_along(data), FUN = function(i) {
    plot.prep.ind(i, data[[i]])
  })
  data.prep = rbindlist(data.prep)
}



# prepare data frame of child node for plot (distinguishes between mutli and binary split)
helper_prepChildPlot <- function(sp, index, plot.data, multi = TRUE){
  if(multi==TRUE){
    sp_multi_unlist = rbindlist(sp, idcol = "n.splits")
    n_split = sp_multi_unlist$n.splits[which(sp_multi_unlist$objective.value ==min(sp_multi_unlist$objective.value))[1]]
    child_node1 = generate_node_index(Y[index,], X[index,], result = sp[[n_split]])
  }
  else{
    child_node1 = generate_node_index(Y[index,], X[index,], result = sp)
  }
  
  # filter plot data on relevant indices and replace them by new child_node1 indices and split in new child classes
  plot.data.child1 = plot.data[which(plot.data$.id %in% index),]
  plot.data.child1$.id = rep(1:length(index), each = 20)
  plot.data.child1$.split = child_node1$class[plot.data.child1$.id]
  return(plot.data.child1)
}


