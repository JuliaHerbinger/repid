
# path to folder, with folders for dataset results
path = "results/batchtools/1_result_tables/"
datasets = list.files(path)
type = c( "numeric_linear","linear_mixed", "nonlinear")

# create list with one list containing one dataframe per dataset
data.all = lapply(type, function(typ){
  
  #browser()
  df.final = NULL
  methods = c("tree", "hstat", "perf")
  # trees = which(endsWith(methods, "", trim = TRUE))
  # if(length(trees) > 0) objectives = objectives[-trees]
  #
  lapply(methods, function(method){
    #browser()
    res.tree = readRDS(file.path(path, paste0("eval_", typ, "_", method, ".rds")))
    for(i in 1:nrow(res.tree)){
      #browser()
      print(i)
      df_sub = as.data.frame(res.tree[i,])
      
      if(method == "perf") df_sub_res = data.frame(perf = df_sub$result[[1]])
      else df_sub_res = df_sub$result[[1]]
      if((is.null(df_sub_res)) ) next
      else if( (nrow(df_sub_res) == 0)) next
      df_sub = cbind(df_sub[,which(colnames(df_sub) != "result")], df_sub_res)
      
      if(i == 1) df = df_sub
      else df = rbind(df, df_sub)
      
    }
    df
    
    
  })
  
  
  
  
})
names(data.all) = type

data = data.all$nonlinear

library(dplyr)

dat.tree = data[[1]]
#dat.first = data[[2]]
#colnames(dat.first)[10] = "split.feature"
#dat.first$rel.imp.first = (dat.first$obj.parent-dat.first$objective.value)/dat.first$obj.parent
#dat.multiple = data[[3]]
dat.hstat = data[[2]]
dat.perf = data[[3]]

# calculate loss reduction on relative importance per node
dat.tree$loss.reduction = as.numeric(dat.tree$objective.value.parent) - as.numeric(dat.tree$objective.value)
dat.tree$rel.importance = as.numeric(dat.tree$loss.reduction) / as.numeric(dat.tree$objective.value.root)

# aggregating results on iteration and feature level
df.aggr.job.feat = dat.tree %>% dplyr::group_by(job.id, split.feature, impr.par) %>% dplyr::summarise(loss = sum(loss.reduction), rel.imp.tree = sum(rel.importance) )

#browser()
# create new table with rank correlation, tpr and fpr per iteration

# features = colnames(data)[-which(colnames(data) %in% c("y",feat))]
# table.ranks = data.frame("feature" = rep(features, n.sim), "iter" = rep(1:n.sim, each = length(features)),
#                          "true.ranks" = rep(true.ranks, n.sim), "hstatistic.feat" = hstatistic$.feature,
#                          "hstatistic" = hstatistic$.interaction)


#df.interactions = merge(dat.first, df.aggr.job.feat[,c("job.id","split.feature", "rel.imp.tree")], all.x = TRUE, by.x = c("job.id","split.feature"), by.y = c("job.id","split.feature"))


# multiple splits with same feature

# calculate loss reduction on relative importance per node
#dat.multiple$loss.reduction = as.numeric(dat.multiple$objective.value.parent) - as.numeric(dat.multiple$objective.value)
#dat.multiple$rel.importance = as.numeric(dat.multiple$loss.reduction) / as.numeric(dat.multiple$objective.value.root)

# aggregating results on iteration and feature level
#df.multiple.job.feat = dat.multiple %>% dplyr::group_by(job.id, split.feature) %>% dplyr::summarise(loss = sum(loss.reduction), rel.imp.multiple = sum(rel.importance) )

#df.interactions = merge(df.interactions, df.multiple.job.feat[,c("job.id","split.feature", "rel.imp.multiple")], all.x = TRUE, by.x = c("job.id","split.feature"), by.y = c("job.id","split.feature"))


# add hstatistics
impr.par = 0.15
df.interactions = merge(dat.hstat, df.aggr.job.feat[df.aggr.job.feat$impr.par==impr.par,c("job.id","split.feature","impr.par", "rel.imp.tree")], all.x = TRUE, by.x = c("job.id","interact.feature"), by.y = c("job.id","split.feature"))
df.interactions$impr.par = impr.par

# add performance
df.interactions = merge(df.interactions, dat.perf[,c("job.id","perf")], all.x = TRUE, by.x = c("job.id"), by.y = c("job.id"))

# set nas to 0
df.interactions$rel.imp.tree[is.na(df.interactions$rel.imp.tree)] = 0
#df.interactions$rel.imp.multiple[is.na(df.interactions$rel.imp.multiple)] = 0


# change columnnames
colnames(df.interactions)[which(colnames(df.interactions) %in% c(".interaction", "rel.imp.tree"))] = c("H-Statistic", "REPID")

library(tidyr)

data_long <- gather(df.interactions, method, interaction, c("H-Statistic", "REPID"), factor_key=TRUE)


#visualization
type = "nonlinear"
feature = "x2"
features = unique(data_long$interact.feature)
data_long$interact.feature = factor(data_long$interact.feature, levels = c("x1", "x2",  "x3",  "x4",  "x5" , "x6" , "x7" , "x8" , "x9","x10"   ))
data_long$method = factor(data_long$method, levels = c("REPID", "H-Statistic"   ))
n = 2000
data_long_x = data_long[data_long$feature==feature & data_long$n ==n,]

p1 = ggplot(data_long_x, aes(x = interact.feature, y = interaction, fill = learner)) + facet_grid(vars(method)) + geom_boxplot(aes(fill = learner)) +
  ylab("Interaction Strength") + xlab("Feature") + theme_bw() +
  theme(legend.position="top", legend.text=element_text(size=16), text = element_text(size = 16)) +
  #scale_color_discrete(name = "Region", labels = c("Sex = F & Fare > 26", "Sex = M & Pclass = 1", "Sex = F & Fare < 26 & Pclass = {1,2}", "Sex = F & Fare < 26 & Pclass = 3", "Sex = M & Pclass = {2,3} & Embarked = C", "Sex = M & Pclass = {2,3} & Embarked = {Q, S}"))+
  scale_fill_manual(values = c(brewer.pal(3,"Dark2")),name = "", 
                    labels = c("GAM", "SVM", "RF"))

ggsave("sim_nonlinear_x2.pdf", p1, width = 8, height = 4.8)

# average performance
data_perf = data_long_x %>% group_by(job.id, learner) %>% dplyr::summarise(mean = mean(perf))
data_perf %>% group_by(learner) %>% dplyr::summarise(mu = mean(mean),sd= sd(mean) )

