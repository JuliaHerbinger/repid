library(reshape2)
library(stringr)
library(ggplot2)
library(tidyverse)
library(Rmalschains)
library(iml)
library(ranger)
library(kmlShape)
library(dtw)
library(egg)
library(devtools)
library(rlist)






get_sim_results = function(data, feature, learner, n.split, iter, impr.par){
  #browser()
  X = data[, setdiff(colnames(data), "y")]
  
  if(learner == "lm"){
    set.seed(iter)
    mod =lm(formula = y ~ .^2, data = data)
    pred = predict.function = function(model, newdata) predict.lm(model, newdata)
    model = Predictor$new(mod, data = X, predict.function = pred)
  }
  
  else{
    set.seed(iter)
    task = mlr::makeRegrTask(data = data, target = "y")
    lrn = mlr::makeLearner(learner)
    mod = mlr::train(lrn, task)
    #mod = ranger(y ~ ., data = data, num.trees = 500)
    #pred = predict.function = function(model, newdata) predict(model, newdata)$predictions
    model = Predictor$new(mod, data = X, y = data$y)#, predict.function = pred)
  }
  
  
  
  effect = FeatureEffect$new(model, method = "ice", grid.size = 20, feature = feature)
  
  tree = compute_tree(effect, X, objective = "SS_L2", n.split = n.split, impr.par = impr.par)
  tree = list.clean(tree, function(x) length(x) == 0L, TRUE)
  #browser()
  sim.result = extract_split_criteria(tree)
  sim.result$iter = rep(iter, nrow(sim.result))
  sim.result$objective.value.root = sim.result$objective.value.parent[sim.result$id==0]
  
  # calculate hstatistic
  hstat = Interaction$new(model, feature = feature)$results
  hstat$iter = iter
  
  return(list("sim.result" = as.data.frame(sim.result), "hstatistic" = hstat))
}


extract_split_criteria = function(tree){
  list.split.criteria = lapply(tree, function(depth){
    lapply(depth, function(node){
      
      if(is.null(node$split.feature)){
        df = data.frame("depth" = "final", "id" = "final", 
                        "objective.value" = "final", 
                        "objective.value.parent" = "final",
                        "rsqrt" = "final", 
                        "split.feature" = "final", 
                        "split.value" = "final")
      } 
      else{
        df = data.frame("depth" = node$depth, "id" = node$id,
                        "objective.value" = node$objective.value,
                        "objective.value.parent" = node$objective.value.parent,
                        "rsqrt" = node$rsqrt,
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


create_aggr_restults = function(n, n.sim, feat, learner, n.split, type, impr.par = 0.3, min.rel.imp = 0, min.hstat = 0.1){
  #browser()
  # calculate detailed simulation results
  for(i in 1:n.sim){
    data.sim = create_sim_data(n, n.sim, feat, n.split, i, type)
    data = data.sim[[1]]
    true.ranks = data.sim[[2]]
    
    results = get_sim_results(data, feat, learner, n.split, i, impr.par)
    if(i == 1) {
      df.results = results$sim.result
      hstatistic = results$hstatistic
    } 
    else {
      df.results = rbind(df.results, results$sim.result)
      hstatistic = rbind(hstatistic, results$hstatistic)
    } 
    
  }
  # calculate loss reduction on relative importance per node
  df.results$loss.reduction = as.numeric(df.results$objective.value.parent) - as.numeric(df.results$objective.value)
  df.results$rel.importance = as.numeric(df.results$loss.reduction) / as.numeric(df.results$objective.value.root)
  
  # aggregating tree results to iteration / feature level
  
  # aggregating results on iteration and feature level
  df.aggr.iter.feat = df.results %>% dplyr::group_by(iter, split.feature) %>% dplyr::summarise(loss = sum(loss.reduction), rel.imp = sum(rel.importance) ) 
  
  #browser()
  # create new table with rank correlation, tpr and fpr per iteration
  
  features = colnames(data)[-which(colnames(data) %in% c("y",feat))]
  table.ranks = data.frame("feature" = rep(features, n.sim), "iter" = rep(1:n.sim, each = length(features)), 
                           "true.ranks" = rep(true.ranks, n.sim), "hstatistic.feat" = hstatistic$.feature,
                           "hstatistic" = hstatistic$.interaction)
  
  table.ranks = merge(table.ranks, df.aggr.iter.feat[,c("split.feature", "iter", "rel.imp")], all.x = TRUE, by.x = c("feature", "iter"), by.y = c("split.feature", "iter"))
  table.ranks$rel.imp[is.na(table.ranks$rel.imp)|table.ranks$rel.imp < min.rel.imp] = 0
  table.ranks$hstatistic[table.ranks$hstatistic < min.hstat] = 0
  
  # calculate spearman, tpr and fpr for each iteration 
  for(i in 1:n.sim){
    # rel interaction imp
    table.ranks$rank.corr[table.ranks$iter==i] = cor(table.ranks$true.ranks[which(table.ranks$iter == i)], table.ranks$rel.imp[which(table.ranks$iter == i)], method = "spearman")
    table.ranks$tpr[table.ranks$iter==i] = nrow(table.ranks[table.ranks$iter==i & table.ranks$true.ranks != 0 & table.ranks$rel.imp != 0,]) / nrow(table.ranks[table.ranks$iter==i & table.ranks$true.ranks != 0 ,])
    table.ranks$tnr[table.ranks$iter==i] = nrow(table.ranks[table.ranks$iter==i & table.ranks$true.ranks == 0 & table.ranks$rel.imp == 0,]) / nrow(table.ranks[table.ranks$iter==i & table.ranks$true.ranks == 0 ,])
    
    # hstat
    table.ranks$rank.corr.hstat[table.ranks$iter==i] = cor(table.ranks$true.ranks[which(table.ranks$iter == i)], table.ranks$hstatistic[which(table.ranks$iter == i)], method = "spearman")
    table.ranks$tpr.hstat[table.ranks$iter==i] = nrow(table.ranks[table.ranks$iter==i & table.ranks$true.ranks != 0 & table.ranks$hstatistic != 0,]) / nrow(table.ranks[table.ranks$iter==i & table.ranks$true.ranks != 0 ,])
    table.ranks$tnr.hstat[table.ranks$iter==i] = nrow(table.ranks[table.ranks$iter==i & table.ranks$true.ranks == 0 & table.ranks$hstatistic == 0,]) / nrow(table.ranks[table.ranks$iter==i & table.ranks$true.ranks == 0 ,])
  }
  
  # aggregate table on iteration level
  table.ranks.aggr = table.ranks %>% dplyr::group_by(iter) %>% dplyr::summarise(rel.imp = sum(rel.imp), rank.corr = mean(rank.corr), tpr = mean(tpr), tnr = mean(tnr), rank.corr.hstat = mean(rank.corr.hstat), tpr.hstat = mean(tpr.hstat), tnr.hstat = mean(tnr.hstat) )
  
  
  # total loss/variance reduction and number final nodes by iteration
  df.aggr.iter = df.results %>% dplyr::group_by(iter) %>% dplyr::summarise(loss = sum(loss.reduction), rel.imp = sum(rel.importance), num.nodes = mean(n.final) ) 
  
  # average importance per feature
  df.aggr.feature = df.aggr.iter.feat %>% dplyr::group_by(split.feature) %>% dplyr::summarise(avg.loss = mean(loss), avg.rel.imp = mean(rel.imp) ) 
  
  return(list("df.results" = df.results, "df.aggr.iter.feat" = df.aggr.iter.feat,"table.ranks" = table.ranks, "table.ranks.aggr" = table.ranks.aggr, "df.aggr.iter" = df.aggr.iter, "df.aggr.feat" = df.aggr.feature))
}


