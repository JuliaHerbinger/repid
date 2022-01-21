


source("R/simulations/batchtools/simulation_setting_definition.R")


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
      
    }
    if(job$prob.pars$type == "nonlinear"){
      mod = gam(y~s(x1,x2,x3)+s(x4,x2)+s(x6,x2)+s(x8,x2)+
                  s(x1)+s(x2)+s(x3)+s(x4)+s(x5)+s(x6)+s(x7)+s(x8)+s(x9)+s(x10),data=data, method="REML")
      predict.function = function(model, newdata) predict.gam(model, newdata)
      model = Predictor$new(mod, data = X, predict.function = predict.function)
      
    }
    pred = predict.function(mod, testdata)
    perf.test = measureMSE(testdata$y, pred)
    
  }
  
  else if(learner == "ranger_exact"){
    if(job$prob.pars$type == "numeric_linear"){
      interaction_constraint_list = "[[1,2],[1,3],[1,4]]"
      #formula = y~x1+x2+x3+x4+x5+x6+x7 + x2*x3 + x2*x4 + x2*x5
    }
    if(job$prob.pars$type == "friedman"){
      interaction_constraint_list = "[[0,2]]"
      #formula = y~x1*x2+x3+x4+x5+x6+x7+x8+x9+x10
    }
    if(job$prob.pars$type == "nonlinear"){
      interaction_constraint_list = "[[0,2,3],[2,4],[2,6],[2,8]]"
      #formula = y~x1*x2*x3 + x2*x4 + x6*x2 + x8*x2 +x3+x4+x5+x6+x7+x8+x9+x10
    }
    #mod = ranger(formula = formula,data=data)
    library(mlr3)
    task = TaskRegr$new(id = "test", backend = data, target = "y")
    lrn.xgb = lrn("regr.xgboost", objective = "reg:squarederror",
                  eta = 0.1,  nrounds = 1000,
                 # eta = 1,
                 # num_parallel_tree = 500,
                 # subsample = 0.63,
                 # colsample_bynode = floor(sqrt(10)) / 10,
                 # lambda = 0,
                 # max_depth = 20,
                 # min_child_weight = 2,
                 # nrounds = 1,
                 # verbose = 0,
                  interaction_constraints = interaction_constraint_list)
    
    mod = lrn.xgb$train(task)
    #predict.function = function(model, newdata) predict(model, newdata)
    model = Predictor$new(mod, data = X, y = data$y)
    #test_data = expand_dataset(formula, testdata)
    pred = lrn.xgb$predict_newdata(task = task,  newdata = testdata)
    perf.test = mlr3measures::mse(truth = pred$truth, response = pred$response)
    #perf.test = measureMSE(pred$data$truth, pred$data$response)
    
  }
  
  else{
   
    task = mlr::makeRegrTask(data = data, target = "y")
    lrn = mlr::makeLearner(learner)
    mod = mlr::train(lrn, task)
    model = Predictor$new(mod, data = X, y = data$y)
    
    # performance on training data - sanity check how good ML model adjusts to underlying function
    pred = predict(object = mod, newdata = testdata)
    perf.test = mlr::measureMSE(pred$data$truth, pred$data$response)
  }
  
  effect = FeatureEffect$new(model, method = "ice", grid.size = 20, feature = feature)
  
  # trees with different improvement
  tree.result = lapply(impr.par, function(impr){
    tree = compute_tree(effect, X, objective = "SS_L2", n.split = n.split, impr.par = impr)
    tree = list.clean(tree, function(x) length(x) == 0L, TRUE)
    
    sim.result = extract_split_criteria(tree)
    
    if(is.null(sim.result)) return()
    else if(nrow(sim.result) == 0) return()
    
    else{
      sim.result$objective.value.root = sim.result$objective.value.parent[sim.result$id==0]
      sim.result$impr.par = impr
      return(sim.result)
    }
    
  })
  tree.result = list.clean(tree.result)
  tree.result = do.call("rbind", tree.result)
  
  
  # calculate hstatistic
  hstat = Interaction$new(model, feature = feature)$results
  hstat$interact.feature = setdiff(colnames(X),feature)
  
  
  return(list("result.tree" = as.data.frame(tree.result),  "hstatistic" = hstat,  "effect" = effect, "perf.test" = perf.test))
}


# helper function for ranger
expand_dataset <- function(formula, data) {
  require(dplyr)
  
  form_split <- gsub(":", "*", as.character(formula)) 
  form_corr <- as.formula(paste(form_split[2], form_split[1], form_split[3]))
  
  response = form_split[[2]]
  
  model.matrix(form_corr, data) %>% 
    as_data_frame  %>% 
    bind_cols(data[c(response)])
}

# helper function to transform data for analysis
data_prep_sim_complex = function(data, impr.par = 0.15){
  dat.tree = data[[1]]
  dat.hstat = data[[2]]
  dat.perf = data[[3]]
  
  # calculate loss reduction on relative importance per node
  dat.tree$loss.reduction = as.numeric(dat.tree$objective.value.parent) - as.numeric(dat.tree$objective.value)
  dat.tree$rel.importance = as.numeric(dat.tree$loss.reduction) / as.numeric(dat.tree$objective.value.root)
  
  # aggregating results on iteration and feature level
  df.aggr.job.feat = dat.tree %>% dplyr::group_by(job.id, split.feature, impr.par) %>% dplyr::summarise(loss = sum(loss.reduction), rel.imp.tree = sum(rel.importance) )
  
  
  # add hstatistics
  df.interactions = merge(dat.hstat, df.aggr.job.feat[df.aggr.job.feat$impr.par==impr.par,c("job.id","split.feature","impr.par", "rel.imp.tree")], all.x = TRUE, by.x = c("job.id","interact.feature"), by.y = c("job.id","split.feature"))
  df.interactions$impr.par = impr.par
  
  # add performance
  df.interactions = merge(df.interactions, dat.perf[,c("job.id","perf")], all.x = TRUE, by.x = c("job.id"), by.y = c("job.id"))
  
  # set nas to 0
  df.interactions$rel.imp.tree[is.na(df.interactions$rel.imp.tree)] = 0
  
  
  # change columnnames
  colnames(df.interactions)[which(colnames(df.interactions) %in% c(".interaction", "rel.imp.tree"))] = c("H-Statistic", "REPID")
  
  data_long <- gather(df.interactions, method, interaction, c("H-Statistic", "REPID"), factor_key=TRUE)
  
}


# helper function for simulation example "weaknesses of other methods"


# copula approach with Gauss copula
# see: https://www.r-bloggers.com/2014/06/generating-and-visualising-multivariate-random-numbers-in-r/
# or: https://stackoverflow.com/questions/32365016/create-correlated-variables-following-various-distributions/32366655
marginals_copula = function(cor_matrix, list_distributions, L)
{
  n = length(list_distributions)
  # Correlated Gaussian variables
  Gauss = rmvnorm(n=L, mean = rep(0,n), sig=cor_matrix)
  # convert them to uniform distribution.
  Unif = pnorm(Gauss) 
  # Convert them to whatever I want
  vars = sapply(1:n, FUN = function(i) list_distributions[[i]](Unif[,i]))
  return(vars)
}


# switch model formula depending on setting
create_formula <- function(data, problem){
  attach(data)
  if(problem == "eq") formula = X1 + X2 + X3 + X4 + X2*X3 + X1*X2 + X1*X3 + X1*X2*X3
  else if(problem == "s_main") formula = 0.1*X1 + X2 + X3 + X4 + X2*X3 + X1*X2 + X1*X3 + X1*X2*X3
  else if(problem =="s_main_uneq") formula = 0.1*X1 + 0.1*X2 + 0.1*X3 + 0.1*X4 + X2*X3 + 2*X1*X2 + X1*X3 + X1*X2*X3
  return(formula)
}

# generate simulated data and calculate REPID, H-Statistic, SHAP and Greenwell Interaction Index
generate_data_sim = function(list_distributions, formula, cor_matrix, L, n, problem, grid = 20, n.sample = 100, n.repl = 20){
  
  set.seed(123)
  for(i in 1:n){
    
    vars = as.data.frame(marginals_copula(cor_matrix, list_distributions, L))
    colnames(vars) = paste0("X", 1:ncol(vars))
    
    
    remainder = create_formula(vars, problem)
    eps = rnorm(L, 0, sd(remainder)*0.1)
    y = target =  remainder + eps
    
    data = cbind(vars, y)
    
    
    mod =lm(formula = formula, data = data)
    predict.function = function(model, newdata) predict.lm(model, newdata)
    model = Predictor$new(mod, data = data, y = "y", predict.function = predict.function)
    
    
    feature = "X2"
    effect = FeatureEffect$new(model, method = "pdp+ice", grid.size = grid, feature = feature)
    
    # tree splitting and quantification
    tree = compute_tree(effect, vars, objective = "SS_L2", n.split = 6, impr.par = 0.15)
    tree = list.clean(tree, function(x) length(x) == 0L, TRUE)
    sim.result = extract_split_criteria(tree)
    
    if(nrow(sim.result)>0){
      sim.result$objective.value.root = sim.result$objective.value.parent[sim.result$id==0]
      # calculate loss reduction on relative importance per node
      dat.tree = sim.result
      dat.tree$loss.reduction = as.numeric(dat.tree$objective.value.parent) - as.numeric(dat.tree$objective.value)
      dat.tree$rel.importance = as.numeric(dat.tree$loss.reduction) / as.numeric(dat.tree$objective.value.root)
      dat.tree$iter = i
      if(!exists("data.tree")) data.tree = dat.tree
      else data.tree = rbind(data.tree, dat.tree)
    }
    
    
    # calculate hstatistic
    hstat = Interaction$new(model, feature = feature, grid.size = grid)$results
    hstat$interact.feature = setdiff(colnames(vars),feature)
    hstat$iter = i
    if(i == 1) data.hstat = hstat
    else data.hstat = rbind(data.hstat, hstat)
    
    # calculate Greenwell interaction index
    greenwell.interaction = lapply(setdiff(colnames(data), c(feature, "y")), function(feat.interact) {
      greenwell.interaction = vint(mod, c(feature, feat.interact), train = data, grid.resolution = grid)
      greenwell.interaction$feat.interact = feat.interact
      greenwell.interaction
    })
    greenwell = do.call("rbind", greenwell.interaction)
    greenwell$iter = i
    if(i == 1) data.greenwell = greenwell
    else data.greenwell = rbind(data.greenwell, greenwell)
    
    
    # calculate shapley interaction index
    shap = shap.interaction.global(data = data, object = mod, feat = feature, target = "y", n.sample = n.sample, n.repl = n.repl, predict.function = predict.function)
    shap$iter = i
    if(i == 1) data.shap = shap
    else data.shap = rbind(data.shap, shap)
    
    
  }
  
  df.aggr.job.feat = setDT(data.tree %>% dplyr::group_by(iter, split.feature) %>% dplyr::summarise(loss = sum(loss.reduction), rel.imp.tree = sum(rel.importance) ))
  df.aggr.job.feat$split.feature = unlist(df.aggr.job.feat$split.feature)
  df.interactions = data.table:::merge.data.table(data.hstat, df.aggr.job.feat[,c("iter","split.feature", "rel.imp.tree")], all.x = TRUE, by.x = c("iter","interact.feature"), by.y = c("iter","split.feature"))
  df.interactions = data.table:::merge.data.table(df.interactions, data.shap[,c("iter","feat.interact", "shap.interaction.rel")], all.x = TRUE, by.x = c("iter","interact.feature"), by.y = c("iter","feat.interact"))
  df.interactions = data.table:::merge.data.table(df.interactions, data.greenwell[,c("iter","feat.interact", "Interaction")], all.x = TRUE, by.x = c("iter","interact.feature"), by.y = c("iter","feat.interact"))
  # set nas to 0
  df.interactions$rel.imp.tree[is.na(df.interactions$rel.imp.tree)] = 0
  colnames(df.interactions)[which(colnames(df.interactions) %in% c("rel.imp.tree", ".interaction","shap.interaction.rel", "Interaction"))] = c("hstatistic","tree","shapley","greenwell")
  
  df.interactions.plot = df.interactions %>% gather(key = "method", value = "interaction", -c(iter, .feature, interact.feature))
  df.interactions.plot$method = factor(df.interactions.plot$method, levels = c("tree","hstatistic","greenwell","shapley"))
  levels(df.interactions.plot$method) = c("REPID","H-statistic","Greenwell","Shapley")
  
  return(df.interactions.plot)
  
}


# prep data for robustness analysis
data_prep_robust = function(data){
  data$id.node = NA
  for(i in unique(data$job.id)){
    for(mod in unique(data$learner)){
      for(d in 1:3){
        data[data$depth == d & data$job.id == i & data$learner==mod, "id.node"] = 1:nrow(data[data$depth == d & data$job.id == i & data$learner==mod, ])
      }
    }
    
  }
  data = data[!is.na(data$id.node),]  
}


# helper function for simulation example "VINE vs. REPID"

# prepare grouped ICE curve dataframe from VINE in suitable format to plot
helper_vine_ice <- function(df_name){
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
helper_vine_write <- function(featureEffects, df_name){
  ice_data = featureEffects$results[[1]]
  grid = subset(ice_data, .id ==1)$.borders
  ice_data = ice_data[,-c(3,5)]
  ice_data_transformed = reshape(ice_data, idvar = ".id", timevar = ".borders", direction = "wide")
  ice_data_transformed = ice_data_transformed[,-1]
  write.csv2(ice_data_transformed, df_name)
}
