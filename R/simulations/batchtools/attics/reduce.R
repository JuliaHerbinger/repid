reduce_trees = function(reg, problems = NULL, savedir) {

	tab = summarizeExperiments(
		by = c("job.id", "algorithm", "problem", "n", "type", "feature", "impr.par", "learner"), reg = reg)

	if (is.null(problems))
		problems = unique(tab$problem)


	for (typ in unique(tab$type)) {

		for (prob in unique(tab$prob)) {

		    toreduce = tab[problem %in% prob & type == typ, ]
		    toreduce = ijoin(toreduce, findDone())

		    if (nrow(toreduce) > 0) {

		    	# We only store the evaluation metrics
				  res.tree = reduceResultsDataTable(toreduce, function(x) x$result.tree)
			    res.tree = ijoin(tab, res.tree) 
			    #res.first = reduceResultsDataTable(toreduce, function(x) setDT(x$result.frist.split))
			    #res.first = ijoin(tab, res.first)
			    #res.multiple = reduceResultsDataTable(toreduce, function(x) x$result.multiple.splits)
			    #res.multiple = ijoin(tab, res.multiple)
			    res.hstat = reduceResultsDataTable(toreduce, function(x) x$hstatistic)
			    res.hstat = ijoin(tab, res.hstat)
			    res.perf = reduceResultsDataTable(toreduce, function(x) x$perf.test)
			    res.perf = ijoin(tab, res.perf)

				path = file.path(savedir, prob, "1_result_tables")

				if (!dir.exists(path)) 
				  dir.create(path, recursive = TRUE)
	      
	      		savepath = file.path(path, paste0("eval_", typ, "_", "tree.rds"))
	       		saveRDS(res.tree, savepath)  
	       		
	       		#savepath = file.path(path, paste0("eval_", typ, "_", "first.rds"))
	       		#saveRDS(res.first, savepath)  
	       		
	       		#savepath = file.path(path, paste0("eval_", typ, "_", "multiple.rds"))
	       		#saveRDS(res.multiple, savepath)  
	       		
	       		savepath = file.path(path, paste0("eval_", typ, "_", "hstat.rds"))
	       		saveRDS(res.hstat, savepath)  
	       		
	       		savepath = file.path(path, paste0("eval_", typ, "_", "perf.rds"))
	       		saveRDS(res.perf, savepath)  
			}
		}
	}
}

reg = loadRegistry("R/Simulation_Examples/batchtools/test")
reduce_trees(reg = reg, savedir = "batchtools/experiments")


# path to folder, with folders for dataset results
path = "R/Simulation_Examples/batchtools/experiments/interaction_det_ind/1_result_tables/"
datasets = list.files(path)
type = c("categorical_linear", "numeric_linear","linear_mixed", "nonlinear",  "friedman")

# create list with one list containing one dataframe per dataset
data.all = lapply(type, function(typ){

  #browser()
  df.final = NULL
  methods = c("tree", "first", "multiple", "hstat", "perf")
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
    #df_sub = res$result[[1]]$eval
    #df_sub$objective = res$objective
    # if(is.null(df.final)){
    #   df.final = df
    #   df.final$method = method
    # }
    # else{
    #   df.final = rbind(df.final, cbind(df, "method" = method))
    # }

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
dat.hstat = data[[4]]
dat.perf = data[[5]]

# calculate loss reduction on relative importance per node
dat.tree$loss.reduction = as.numeric(dat.tree$objective.value.parent) - as.numeric(dat.tree$objective.value)
dat.tree$rel.importance = as.numeric(dat.tree$loss.reduction) / as.numeric(dat.tree$objective.value.root)

# aggregating results on iteration and feature level
df.aggr.job.feat = dat.tree %>% dplyr::group_by(job.id, split.feature) %>% dplyr::summarise(loss = sum(loss.reduction), rel.imp.tree = sum(rel.importance) )

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
df.interactions = merge(df.interactions, dat.hstat[,c("job.id","interact.feature", ".interaction")], all.y = TRUE, by.x = c("job.id","split.feature"), by.y = c("job.id","interact.feature"))


# add performance
df.interactions = merge(df.interactions, dat.perf[,c("job.id","perf")], all.x = TRUE, by.x = c("job.id"), by.y = c("job.id"))

# set nas to 0
df.interactions$rel.imp.tree[is.na(df.interactions$rel.imp.tree)] = 0
#df.interactions$rel.imp.multiple[is.na(df.interactions$rel.imp.multiple)] = 0

library(tidyr)
data_long <- gather(df.interactions, method, interaction, rel.imp.tree:.interaction, factor_key=TRUE)


#visualization
type = "nonlinear"
feature = "x2"
n = 5000
data_long_x = data_long[data_long$feature==feature & data_long$n ==n,]
ggplot(data_long_x, aes(x = split.feature, y = interaction, fill = method)) + facet_grid(vars(impr.par), vars(learner)) + geom_boxplot(aes(fill = method)) +
  ggtitle(paste0("type =",type, ", n = ", n, ", feature = ", feature))
