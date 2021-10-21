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
library(mgcv)
library(mvtnorm)
library(BBmisc)
library(checkmate)

fun = function(cor_matrix, list_distributions, L)
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

L = 1000
cor_matrix =  matrix(c (1.00, 0.30, 0.3 ,0.1,0.85,0.3, 0.8, 0.8,0.8,0.8,
                        0.30, 1.00, 0.9, 0.15, 0.35,0.3,0.8, 0.8,0.8,0.8,
                        0.30, 0.9, 1.00, 0.1, 0.9, 0.3,0.8, 0.8,0.8,0.8,
                        0.1,  0.15, 0.1,  1.0, 0.15, 0.2,0.8, 0.8,0.8,0.8,
                        0.85, 0.35, 0.9,  0.15, 1.0, 0.3,0.8, 0.8,0.8,0.8,
                        0.3,  0.3, 0.3,  0.2,  0.3, 1.0, 0.8, 0.8,0.8,0.8,
                        0.8, 0.8,  0.8,  0.8,  0.8, 0.8, 1.0,  0.8, 0.8,0.8,
                        0.8, 0.8,  0.8,  0.8,  0.8, 0.8,  0.8, 1.0, 0.8,0.8,
                        0.8, 0.8,  0.8,  0.8,  0.8, 0.8, 0.8, 0.8,1.0, 0.8,
                        0.8, 0.8,  0.8,  0.8,  0.8, 0.8, 0.8, 0.8,0.8, 1.0), 
                     nrow=10,ncol=10,byrow=TRUE)

cor_matrix =  matrix(c (1.00, 0.30, 0.3 ,0.2,0.8,0.3, 
                        0.30, 1.00, 0.8, 0.15, 0.15,0.3,
                        0.30, 0.8, 1.00, 0.1, 0.8, 0.3,
                        0.2,  0.15, 0.1,  1.0, 0.15, 0.2,
                        0.8, 0.15, 0.8,  0.15, 1.0, 0.2,
                        0.3,  0.3, 0.3,  0.2,  0.2, 1.0), 
                     nrow=6,ncol=6,byrow=TRUE)

f_unif = function(L) qunif(L,-1,1)
f_norm1 = function(L) qnorm(L, 0, 1)
f_norm = function(L) qnorm(L, 0, 2)
f_t = function(L) qt(L, 5)
#list_distributions = list( f_t, f_t, f_t,  f_t, f_t, f_t)
list_distributions = list( f_norm, f_norm, f_norm,  f_norm, f_norm, f_norm)
#list_distributions = list( f_norm1, f_norm1, f_norm1,  f_norm1, f_norm1, f_norm1)
#list_distributions = list( f_t, f_t, f_norm,  f_norm, f_norm, f_norm)
#list_distributions = list( f_norm1, f_norm1, f_norm,  f_norm, f_norm, f_norm)

set.seed(123)
for(i in 1:100){
  vars = as.data.frame(fun(cor_matrix, list_distributions, L))
  cor(vars)
  #plot(as.data.frame(vars))
  
  # testdata = as.data.frame(fun(cor_matrix, list_distributions, 100000))
  # formula = testdata$V1 + 4*testdata$V2 + 3*testdata$V2*testdata$V3 + 5*testdata$V2*testdata$V4 + 7*testdata$V2*testdata$V5
  # eps = rnorm(100000, 0, sd(formula)*0.1)
  # testdata$y =  formula + eps
  
  
  library(iml)
  library(mlr)
  formula = 0.2*vars$V1 + 4*vars$V2 + 4*vars$V2*vars$V3 + 6*vars$V2*vars$V4 + 8*vars$V2*vars$V5 + 10*vars$V2*vars$V6
  #formula = 0.2*vars$V1 + 4*vars$V2 + ifelse(vars$V3 < mean(vars$V3), vars$V2, 0) + ifelse(vars$V4 < mean(vars$V4), 2*vars$V2, 0) + ifelse(vars$V5 < mean(vars$V5), 3*vars$V2, 0)
  eps = rnorm(n, 0, sd(formula)*0.1)
  y =  formula + eps
  
  data = cbind(vars, y)
  
  #mod =lm(formula = y ~ V1 + V2 + I((V3 < mean(V3))*V2) + I((V4 < mean(V4))*V2) + I((V5 < mean(V5))*V2), data = data)
  mod =lm(formula = y ~ V1 + V2 + V2:V3 + V2:V4 + V2:V5 + V2:V6, data = data)
  predict.function = function(model, newdata) predict.lm(model, newdata)
  model = Predictor$new(mod, data = data, y = "y", predict.function = predict.function)
  
  
  feature = "V2"
  effect = FeatureEffect$new(model, method = "ice", grid.size = 20, feature = feature)
  
  # tree splitting and quantification
  library(data.table)
  tree = compute_tree(effect, vars, objective = "SS_L2", n.split = 6, impr.par = 0.15)
  tree = list.clean(tree, function(x) length(x) == 0L, TRUE)
  extract_split_criteria(tree)
  
  sim.result = extract_split_criteria(tree)
  #sim.result$iter = rep(iter, nrow(sim.result))
  sim.result$objective.value.root = sim.result$objective.value.parent[sim.result$id==0]
  
  # calculate loss reduction on relative importance per node
  dat.tree = sim.result
  dat.tree$loss.reduction = as.numeric(dat.tree$objective.value.parent) - as.numeric(dat.tree$objective.value)
  dat.tree$rel.importance = as.numeric(dat.tree$loss.reduction) / as.numeric(dat.tree$objective.value.root)
  dat.tree$iter = i
  if(i == 1) data.tree = dat.tree
  else data.tree = rbind(data.tree, dat.tree)
  # aggregating results on iteration and feature level
  #df.aggr.job.feat = dat.tree %>% dplyr::group_by(split.feature) %>% dplyr::summarise(loss = sum(loss.reduction), rel.imp.tree = sum(rel.importance) )
  
  
  # # ranking according to first split
  # result.first.split = first_binary_split_quantification(effect, vars, SS_L2, 20)
  # result.first.split$loss = result.first.split$obj.parent - result.first.split$objective.value
  # result.first.split$rel.imp = result.first.split$loss / result.first.split$obj.parent
  # 
  # 
  # # ranking according to multiple splits with same feature
  # result.multiple.splits = multiple_binary_split_quantification(effect, feature, vars, "SS_L2", 5, 0.2)
  # 
  # # calculate loss reduction on relative importance per node
  # dat.multiple = result.multiple.splits
  # dat.multiple$loss.reduction = as.numeric(dat.multiple$objective.value.parent) - as.numeric(dat.multiple$objective.value)
  # dat.multiple$rel.importance = as.numeric(dat.multiple$loss.reduction) / as.numeric(dat.multiple$objective.value.root)
  # 
  # # aggregating results on iteration and feature level
  # df.multiple.job.feat = dat.multiple %>% dplyr::group_by(split.feature) %>% dplyr::summarise(loss = sum(loss.reduction), rel.imp.multiple = sum(rel.importance) )
  # data.frame(df.multiple.job.feat)
  
  # calculate hstatistic
  hstat = Interaction$new(model, feature = feature)$results
  hstat$interact.feature = setdiff(colnames(vars),feature)
  hstat$iter = i
  if(i == 1) data.hstat = hstat
  else data.hstat = rbind(data.hstat, hstat)
  
  
}

df.aggr.job.feat = setDT(data.tree %>% dplyr::group_by(iter, split.feature) %>% dplyr::summarise(loss = sum(loss.reduction), rel.imp.tree = sum(rel.importance) ))
df.aggr.job.feat$split.feature = unlist(df.aggr.job.feat$split.feature)
df.interactions = data.table:::merge.data.table(data.hstat, df.aggr.job.feat[,c("iter","split.feature", "rel.imp.tree")], all.x = TRUE, by.x = c("iter","interact.feature"), by.y = c("iter","split.feature"))
# set nas to 0
df.interactions$rel.imp.tree[is.na(df.interactions$rel.imp.tree)] = 0

df.interactions$rel.imp.tree[df.interactions$rel.imp.tree < 0.01] = 0
df.interactions$.interaction[df.interactions$.interaction < 0.01] = 0


df.interactions.plot = df.interactions %>% gather(key = "method", value = "interaction", -c(iter, .feature, interact.feature))
ggplot(df.interactions.plot, aes(x = interact.feature, y = interaction)) + geom_boxplot(aes(fill = method)) +
  ggtitle(expression(paste("y = 0.2",x[1]," + 4",x[2], " + 4",x[2],x[3], " + 6",x[2],x[4], " + 8",x[2],x[5], " + 10",x[2],x[6])))
corrplot::corrplot(cor(vars))


df.interactions$true.ranks = rep(c(0,4,6,8,10),100)

cor.tree = c()
cor.hstat = c()
for(i in 1:20){
  
  cor.tree[i] = cor(x = df.interactions$rel.imp.tree[df.interactions$iter==i], y = df.interactions$true.ranks[df.interactions$iter==i], method = "kendall")
  cor.hstat[i] = cor(x = df.interactions$.interaction[df.interactions$iter==i], y = df.interactions$true.ranks[df.interactions$iter==i], method = "kendall")
}

df = data.frame(iter = rep(1:20,2), method = c(rep("tree",20),rep("hstat",20)), "rank.cor" = c(cor.tree,cor.hstat))
ggplot(df, aes(x = method, y = rank.cor)) + geom_boxplot()
