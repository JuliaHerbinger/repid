# Helper functions




extract_split_criteria = function(tree){
  list.split.criteria = lapply(tree, function(depth){
    lapply(depth, function(node){
      
      if(is.null(node$split.feature)){
        df = data.frame("depth" = "final", "id" = "final", 
                        "objective.value" = "final", 
                        "objective.value.parent" = "final",
                        "intImp" = "final", 
                        "split.feature" = "final", 
                        "split.value" = "final")
      } 
      else{
        df = data.frame("depth" = node$depth, "id" = node$id,
                        "objective.value" = node$objective.value,
                        "objective.value.parent" = node$objective.value.parent,
                        "intImp" = node$intImp,
                        "split.feature" = node$split.feature,
                        "split.value" = node$split.value)
      }
      df
    })
  })
  
  list.split.criteria = list.clean(list.split.criteria, function(x) length(x) == 0L, TRUE)
  df.split.criteria = unlist(list.split.criteria, recursive = FALSE)
  df.split.criteria = as.data.frame(do.call(rbind, df.split.criteria))
  n.final = length(which(df.split.criteria$depth == "final"))
  df.split.criteria$n.final = n.final
  df.split.criteria = df.split.criteria[df.split.criteria$depth!="final",]
  
  
  return(df.split.criteria)
}





SS_L2 = function(y, x, requires.x = FALSE, ...) {
  ypred = colMeans(as.matrix(y))
  sum(t((t(y) - ypred)^2))
} 

# helper functions for tree splitting

# performs one split
split_parent_node = function(Y, X, n.splits = 1, min.node.size = 10, optimizer,
                             objective, ...) {
  require(data.table)
  assert_data_frame(X)
  #assert_choice(target.col, choices = colnames(data))
  assert_integerish(n.splits)
  assert_integerish(min.node.size)
  assert_function(objective, args = c("y", "x", "requires.x"))
  assert_function(optimizer, args = c("xval", "y"))
  
  # find best split points per feature
  opt.feature = lapply(X, function(feat) {
    t1 = proc.time()
    res = optimizer(x = feat, y = Y, n.splits = n.splits, min.node.size = min.node.size,
                    objective = objective, ...)
    t2 = proc.time()
    res$runtime = (t2 - t1)[[3]]
    return(res)
  })
  
  result = data.table::rbindlist(lapply(opt.feature, as.data.frame), idcol = "feature")
  result = result[, .(split.points = list(split.points)), by = c("feature", "objective.value", "runtime"), with = TRUE]
  result$best.split = result$objective.value == min(result$objective.value)
  #result = result[, best.split := objective.value == min(objective.value)]
  return(result)
}

generate_node_index = function(Y, X, result) {
  assert_data_table(result)
  # TODO: fix bug if more than one feature have the same best objective
  feature = unique(result$feature[result$best.split])
  split.points = unlist(result$split.points[result$best.split])
  
  if (is.vector(X))
    xval = X else
      xval = X[, feature]
  
  cuts = c(min(xval), split.points, max(xval))
  sp = cut(xval, breaks = unique(cuts), include.lowest = TRUE)
  #levels(sp) = paste0(feature, " in ", levels(sp))
  
  return(list(class = sp, index = split(seq_along(xval), sp)))
}



# performs a binary split
find_best_binary_split = function(xval, y, n.splits = 1, min.node.size = 10,
                                  objective, ...) {
  assert_choice(n.splits, choices = 1)
  
  # use different split candidates to perform split
  q = generate_split_candidates(xval, n.quantiles = 100, min.node.size = min.node.size)
  splits = vapply(q, FUN = function(i) {
    perform_split(i, xval = xval, y = y, min.node.size = min.node.size,
                  objective = objective, ...)
  }, FUN.VALUE = NA_real_, USE.NAMES = FALSE)
  # select the split point yielding the minimal objective
  best = which.min(splits)
  
  return(list(split.points = q[best], objective.value = splits[best]))
}

generate_split_candidates = function(xval, n.quantiles = NULL, min.node.size = 10) {
  assert_integerish(min.node.size, upper = floor((length(xval) - 1)/2))
  xval = sort.int(xval)
  # try to ensure min.node.size between points (is not guaranteed)
  chunk.ind = seq.int(min.node.size + 1, length(xval) - min.node.size, by = min.node.size)
  #xadj = unique(quantile(xval, prob = chunk.ind/length(xval), type = 1))
  xadj = xval[chunk.ind]
  
  if (!is.null(n.quantiles)) {
    # to speedup we use only quantile values as possible split points
    # qprobs = seq(1/n.quantiles, (n.quantiles - 1)/n.quantiles, by = 1/n.quantiles)
    qprobs = seq(0, 1, by = 1/n.quantiles)
    q = unique(quantile(xadj, qprobs, type = 1))
  } else {
    q = unique(xadj)
  }
  
  # use a value between two subsequent points
  q = adjust_split_point(q, xval)
  
  return(q)
}

# Performs a single split and measures the objective
perform_split = function(split.points, xval, y, min.node.size, objective, ...) {
  
  split.points = sort.int(split.points)
  split.points = get_closest_point(split.points, xval, min.node.size)
  #cat(split.points, fill = TRUE)
  
  # assign intervalnr. according to split points
  node.number = findInterval(x = xval, split.points, rightmost.closed = TRUE) + 1
  # compute size of each childnode
  node.size = tabulate(node.number)
  # if minimum node size is violated, return Inf
  # TODO: instead of returning Inf try to avoid that this happens by fixing split points
  if (min(node.size) < min.node.size)
    return(Inf)
  # compute objective in each interval and sum it up
  y.list = split(y, node.number)
  # x.list only needed if this is used in the objective
  requires.x = formals(objective)[["requires.x"]]
  if (isTRUE(requires.x))
    x.list = split(xval, node.number) else
      x.list = NULL
  
  res = vapply(seq_along(y.list), FUN = function(i) {
    objective(y = y.list[[i]], x = x.list[[i]], sub.number = which(node.number == i), ...)
  }, FUN.VALUE = NA_real_, USE.NAMES = FALSE)
  sum(res)
}



# helper functions for splitting

adjust_nsplits = function(xval, n.splits) {
  # max. number of splits to be performed must be unique.x-1
  unique.x = length(unique(xval))
  if (n.splits >= unique.x)
    n.splits = unique.x - 1
  return(n.splits)
}



# replace split.points with closest value from xval taking into account min.node.size
get_closest_point = function(split.points, xval, min.node.size = 10) {
  xval = sort.int(xval)
  # try to ensure min.node.size between points (is not guaranteed if many duplicated values exist)
  chunk.ind = seq.int(min.node.size + 1, length(xval) - min.node.size, by = min.node.size)
  xadj = unique(xval[chunk.ind]) # unique(quantile(xval, prob = chunk.ind/length(xval), type = 1))
  # xval = xval[-c(1, length(xval))]
  split.adj = numeric(length(split.points))
  for (i in seq_along(split.adj)) {
    d = xadj - split.points[i]
    ind.closest = which.min(abs(d))
    split.adj[i] = xadj[ind.closest]
    xadj = xadj[-ind.closest] # remove already chosen value
  }
  
  return(sort.int(split.adj))
}

adjust_split_point = function(split.points, xval) {
  # use a value between two subsequent points
  q = split.points
  x.unique = sort.int(unique(xval))
  ind = which(x.unique %in% q)
  ind = ind[ind < length(x.unique)]
  if (length(ind) != length(q)) {
    eps = min(diff(x.unique))/2
  } else {
    eps = (x.unique[ind + 1] - x.unique[ind])/2
  }
  q = q + eps #+ c(diff(q)/2, 0)
  q[q < x.unique[2]] = mean(x.unique[1:2])
  q[q > x.unique[length(x.unique) - 1]] = mean(x.unique[(length(x.unique) - 1):length(x.unique)])
  #q = q[q <= max(xval) & q >= min(xval)]
  return(q)
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


