# extract result tables 

reduce_trees = function(ades, pdes, n.repl, problems = NULL, savedir) {

	tab = data.frame(job.id = 1:(nrow(ades)*n.repl))
	tab$n = rep(pdes$n, n.repl)
	tab$type = as.character(rep(pdes$type, each = n.repl))
	tab$feature = as.character(rep(ades$feature, each = n.repl))
	tab$learner = as.character(rep(ades$learner, each = n.repl))

	for (typ in unique(tab$type)) {


		    toreduce = tab[tab$type == typ, ]
		    toreduce = ijoin(toreduce, findDone())

		    if (nrow(toreduce) > 0) {

		    	# We only store the evaluation metrics
				  res.tree = reduceResultsDataTable(toreduce, function(x) x$result.tree)
			    res.tree = ijoin(tab, res.tree) 
			    res.hstat = reduceResultsDataTable(toreduce, function(x) x$hstatistic)
			    res.hstat = ijoin(tab, res.hstat)
			    res.perf = reduceResultsDataTable(toreduce, function(x) x$perf.test)
			    res.perf = ijoin(tab, res.perf)

				path = file.path(savedir, "1_result_tables")

				if (!dir.exists(path)) 
				  dir.create(path, recursive = TRUE)
	      
	      		savepath = file.path(path, paste0("eval_", typ, "_", "tree.rds"))
	       		saveRDS(res.tree, savepath)  
	       		
	       		savepath = file.path(path, paste0("eval_", typ, "_", "hstat.rds"))
	       		saveRDS(res.hstat, savepath)  
	       		
	       		savepath = file.path(path, paste0("eval_", typ, "_", "perf.rds"))
	       		saveRDS(res.perf, savepath)  
	
		}
	}
}


reg = loadRegistry("data/batchtools/interaction_detection")
pdes = expand.grid(n = c(2000, 5000), type = rep(c("numeric_linear", "linear_mixed", "nonlinear","nonlinear"), each = 3))
ades = data.table(impr.par = list(list(0.1, 0.15, 0.2,0.25)), n.split = 7, stringsAsFactors = FALSE)
ades = cbind(ades, feature = c(rep(("x2"),18),rep(("x10"),6)))
ades = cbind(ades, learner = c(rep(rep(c("lm","regr.ranger","regr.ksvm"),each = 2),2),rep(rep(c("gam","regr.ranger","regr.ksvm"),each = 2),2)))
ades = ades[,-1]
reduce_trees(ades = ades, pdes = pdes,n.repl = 30, savedir = "data/batchtools/")




