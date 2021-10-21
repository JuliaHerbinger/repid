# library(reshape2)
# library(stringr)
# library(ggplot2)
# library(tidyverse)
# library(Rmalschains)
# library(iml)
# library(ranger)
# library(kmlShape)
# library(dtw)
# library(egg)
# library(devtools)
# library(rlist)
# library(mgcv)


#source("batchtools/simulation_setting_definition.R")


get_sim_results = function(data, job, instance, feature, learner, n.split, impr.par, ...){
  
  data = instance
  X = data[, setdiff(colnames(data), "y")]
  testdata = create_sim_data(job, n = 100000, type = job$prob.pars$type, ...)
  
  if(learner == "lm"){
  
    mod =lm(formula = y ~ .^2, data = data)
    predict.function = function(model, newdata) predict.lm(model, newdata)
    model = Predictor$new(mod, data = X, predict.function = predict.function)
    pred = predict.function(mod, testdata)
    perf.test = measureMSE(testdata$y, pred)
    
  }
  
  else if(learner == "gam"){
    if(job$prob.pars$type == "friedman"){
      mod = gam(y~s(x1,x2)+s(x1)+s(x2)+s(x3)+s(x4)+s(x5)+s(x6)+s(x7)+s(x8)+s(x9)+s(x10),data=data, method="REML")
      predict.function = function(model, newdata) predict.gam(model, newdata)
      model = Predictor$new(mod, data = X, predict.function = predict.function)
      #perf.train = measureMSE(data$y, mod$fitted.values)
    }
    if(job$prob.pars$type == "nonlinear"){
      mod = gam(y~s(x1,x2,x3)+s(x4,x2)+s(x6,x2)+s(x8,x2)+
                  s(x1)+s(x2)+s(x3)+s(x4)+s(x5)+s(x6)+s(x7)+s(x8)+s(x9)+s(x10),data=data, method="REML")
      predict.function = function(model, newdata) predict.gam(model, newdata)
      model = Predictor$new(mod, data = X, predict.function = predict.function)
      #perf.train = measureMSE(data$y, mod$fitted.values)
    }
    pred = predict.function(mod, testdata)
    perf.test = measureMSE(testdata$y, pred)
    
  }
  
  else{
   
    task = mlr::makeRegrTask(data = data, target = "y")
    lrn = mlr::makeLearner(learner)
    #rdesc = makeResampleDesc("CV", iters = 5, predict = "both")
    #r = resample(lrn, task, rdesc)
    mod = mlr::train(lrn, task)
    model = Predictor$new(mod, data = X, y = data$y)
    
    # performance on training data - sanity check how good ML model adjusts to underlying function
    pred = predict(object = mod, newdata = testdata)
    perf.test = measureMSE(pred$data$truth, pred$data$response)
  }
  
  
  
  effect = FeatureEffect$new(model, method = "ice", grid.size = 20, feature = feature)
  
  # tree splitting and quantification
  tree = compute_tree(effect, X, objective = "SS_L2", n.split = n.split, impr.par = impr.par)
  tree = list.clean(tree, function(x) length(x) == 0L, TRUE)
 
  sim.result = extract_split_criteria(tree)
  #sim.result$iter = rep(iter, nrow(sim.result))
  sim.result$objective.value.root = sim.result$objective.value.parent[sim.result$id==0]
  
  # ranking according to first split
  result.first.split = first_binary_split_quantification(effect, X, SS_L2, 20)
  
  
  # ranking according to multiple splits with same feature
  result.multiple.splits = multiple_binary_split_quantification(effect, feature, X, "SS_L2", n.split, impr.par)
  
  
  # calculate hstatistic
  hstat = Interaction$new(model, feature = feature)$results
  hstat$interact.feature = setdiff(colnames(X),feature)
  
  # calculate greenwell interaction index
  # all_pairs <- combn(paste0("x", 1:10), m = 2)
  # all_pairs = all_pairs[, which(all_pairs[1,] == feature)]
  # res <- NULL
  # for (i in 1:ncol(all_pairs)) {
  #   interact <- vint(getLearnerModel(mod), feature_names = all_pairs[, i], train = data)
  #   res <- rbind(res, interact)
  # }
  
  return(list("result.tree" = as.data.frame(sim.result), "result.frist.split" = result.first.split, "result.multiple.splits" = result.multiple.splits, "hstatistic" = hstat, "tree" = tree, "effect" = effect, "perf.test" = perf.test))
}


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

# ranking according to first split
first_binary_split_quantification = function(effect, X, objective, min.node.size){
  input.data = compute_data_for_ice_splitting(effect, testdata = X)
  split = split_parent_node(Y = input.data$Y, X = input.data$X, objective = objective, optimizer = find_best_binary_split, min.node.size = min.node.size)
  split$obj.parent = objective(input.data$Y, input.data$X)
  return(split)
}

# ranking according to multiple splits with same feature
multiple_binary_split_quantification = function(effect, feature, X, objective, n.split, impr.par){
  split.features = setdiff(colnames(X),feature)
  #split.features = X[,..split.features]
  split.list = lapply(split.features, function(feat){
    #browser()
    tree = compute_tree(effect, X[,..feat, drop = FALSE], objective = objective, n.split = n.split, impr.par = impr.par)
    tree = list.clean(tree, function(x) length(x) == 0L, TRUE)
    sim.result = extract_split_criteria(tree)
    sim.result$objective.value.root = sim.result$objective.value.parent[sim.result$id==0]
    #if(nrow(sim.result)>0) sim.result$split.feature = feat
    sim.result
  })
  do.call("rbind", split.list)
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


# create_aggr_restults = function(n, n.sim, feat, learner, n.split, type, impr.par = 0.3, min.rel.imp = 0, min.hstat = 0.1){
#   #browser()
#   # calculate detailed simulation results
#   for(i in 1:n.sim){
#     data.sim = create_sim_data(n, n.sim, feat, n.split, i, type)
#     data = data.sim[[1]]
#     true.ranks = data.sim[[2]]
#     
#     results = get_sim_results(data, feat, learner, n.split, i, impr.par)
#     if(i == 1) {
#       df.results = results$sim.result
#       hstatistic = results$hstatistic
#     } 
#     else {
#       df.results = rbind(df.results, results$sim.result)
#       hstatistic = rbind(hstatistic, results$hstatistic)
#     } 
#     
#   }
#   # calculate loss reduction on relative importance per node
#   df.results$loss.reduction = as.numeric(df.results$objective.value.parent) - as.numeric(df.results$objective.value)
#   df.results$rel.importance = as.numeric(df.results$loss.reduction) / as.numeric(df.results$objective.value.root)
#   
#   # aggregating tree results to iteration / feature level
#   
#   # aggregating results on iteration and feature level
#   df.aggr.iter.feat = df.results %>% dplyr::group_by(iter, split.feature) %>% dplyr::summarise(loss = sum(loss.reduction), rel.imp = sum(rel.importance) ) 
#   
#   #browser()
#   # create new table with rank correlation, tpr and fpr per iteration
#   
#   features = colnames(data)[-which(colnames(data) %in% c("y",feat))]
#   table.ranks = data.frame("feature" = rep(features, n.sim), "iter" = rep(1:n.sim, each = length(features)), 
#                            "true.ranks" = rep(true.ranks, n.sim), "hstatistic.feat" = hstatistic$.feature,
#                            "hstatistic" = hstatistic$.interaction)
#   
#   table.ranks = merge(table.ranks, df.aggr.iter.feat[,c("split.feature", "iter", "rel.imp")], all.x = TRUE, by.x = c("feature", "iter"), by.y = c("split.feature", "iter"))
#   table.ranks$rel.imp[is.na(table.ranks$rel.imp)|table.ranks$rel.imp < min.rel.imp] = 0
#   table.ranks$hstatistic[table.ranks$hstatistic < min.hstat] = 0
#   
#   # calculate spearman, tpr and fpr for each iteration 
#   for(i in 1:n.sim){
#     # rel interaction imp
#     table.ranks$rank.corr[table.ranks$iter==i] = cor(table.ranks$true.ranks[which(table.ranks$iter == i)], table.ranks$rel.imp[which(table.ranks$iter == i)], method = "spearman")
#     table.ranks$tpr[table.ranks$iter==i] = nrow(table.ranks[table.ranks$iter==i & table.ranks$true.ranks != 0 & table.ranks$rel.imp != 0,]) / nrow(table.ranks[table.ranks$iter==i & table.ranks$true.ranks != 0 ,])
#     table.ranks$tnr[table.ranks$iter==i] = nrow(table.ranks[table.ranks$iter==i & table.ranks$true.ranks == 0 & table.ranks$rel.imp == 0,]) / nrow(table.ranks[table.ranks$iter==i & table.ranks$true.ranks == 0 ,])
#     
#     # hstat
#     table.ranks$rank.corr.hstat[table.ranks$iter==i] = cor(table.ranks$true.ranks[which(table.ranks$iter == i)], table.ranks$hstatistic[which(table.ranks$iter == i)], method = "spearman")
#     table.ranks$tpr.hstat[table.ranks$iter==i] = nrow(table.ranks[table.ranks$iter==i & table.ranks$true.ranks != 0 & table.ranks$hstatistic != 0,]) / nrow(table.ranks[table.ranks$iter==i & table.ranks$true.ranks != 0 ,])
#     table.ranks$tnr.hstat[table.ranks$iter==i] = nrow(table.ranks[table.ranks$iter==i & table.ranks$true.ranks == 0 & table.ranks$hstatistic == 0,]) / nrow(table.ranks[table.ranks$iter==i & table.ranks$true.ranks == 0 ,])
#   }
#   
#   # aggregate table on iteration level
#   table.ranks.aggr = table.ranks %>% dplyr::group_by(iter) %>% dplyr::summarise(rel.imp = sum(rel.imp), rank.corr = mean(rank.corr), tpr = mean(tpr), tnr = mean(tnr), rank.corr.hstat = mean(rank.corr.hstat), tpr.hstat = mean(tpr.hstat), tnr.hstat = mean(tnr.hstat) )
#   
#   
#   # total loss/variance reduction and number final nodes by iteration
#   df.aggr.iter = df.results %>% dplyr::group_by(iter) %>% dplyr::summarise(loss = sum(loss.reduction), rel.imp = sum(rel.importance), num.nodes = mean(n.final) ) 
#   
#   # average importance per feature
#   df.aggr.feature = df.aggr.iter.feat %>% dplyr::group_by(split.feature) %>% dplyr::summarise(avg.loss = mean(loss), avg.rel.imp = mean(rel.imp) ) 
#   
#   return(list("df.results" = df.results, "df.aggr.iter.feat" = df.aggr.iter.feat,"table.ranks" = table.ranks, "table.ranks.aggr" = table.ranks.aggr, "df.aggr.iter" = df.aggr.iter, "df.aggr.feat" = df.aggr.feature))
# }
# 
# 
