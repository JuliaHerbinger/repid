
#library(devtools)
#load_all("customtrees")
#source("customtrees/R/tree_splitting.R") # add in package
#source("R/Simulation_Examples/batchtools/helper_simulation.R")



library(batchtools)
#library(data.table)

source("batchtools/config.R")
lapply(packages, require, character.only = TRUE)

# --- 1. SETUP REGISTRY ---

# unlink("batchtools/interaction_detection_test", recursive = TRUE)
reg = makeExperimentRegistry(file.dir = "batchtools/interaction_detection_test", packages = packages,
                             source = c("batchtools/simulation_setting_definition.R","batchtools/tree_splitting.R","batchtools/config.R"), seed = 123)




# --- 2. ADD PROBLEMS, ALGORITHMS, EXPERIMENTS ---

source("batchtools/simulation_setting_definition.R")

# add problems and setting definitions
addProblem(name = "interaction_det_ind", fun = create_sim_data, reg = reg)
pdes = expand.grid(n = c(1000, 5000), type = rep(c("categorical_linear", "numeric_linear", "linear_mixed", "nonlinear","friedman"), each = 18))
pdes = list("interaction_det_ind" = pdes)

# add aglorithms

# define configurations
ades = data.table(impr.par = rep(rep(c(0.2, 0.3, 0.4), each = 4),15), n.split = 6, stringsAsFactors = FALSE)
#grid = expand.grid(seq(1, nrow(ades)), learner = c("lm","regr.ranger","regr.ksvm"),  stringsAsFactors = FALSE)
#ades = cbind(ades[grid$Var1, ], learner = grid$learner, impr.par = grid$impr.par)
ades = cbind(ades, feature = c(rep(rep(c("x2","x6"), each = 2),18),rep(rep(c("x2","x10"), each = 2),27)))
ades = cbind(ades, learner = c(rep(rep(c("lm","regr.ranger","regr.ksvm"),each = 12),3),rep(rep(c("gam","regr.ranger","regr.ksvm"),each = 12),2)))

ALGORITHMS = list(
  get_sim_results = list(fun = get_sim_results, ades = ades
  ))

ades = lapply(ALGORITHMS, function(x) x$ades)

# add all algorithms
for (i in 1:length(ALGORITHMS)) {
  addAlgorithm(name = names(ALGORITHMS)[i], reg = reg, fun = ALGORITHMS[[i]]$fun)  
}


# add experiments
addExperiments(
  reg = reg, 
  prob.designs = pdes,
  algo.designs = ades, 
  combine = "bind",
  repls = 20L)



# --- 3. SUBMIT JOBS
# Sys.time()
# submitJobs(ids = reg$defs$def.id)
# Sys.time()


