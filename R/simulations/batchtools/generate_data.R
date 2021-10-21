# Generate experiments for different settings 


library(batchtools)
#library(data.table)

source("R/simulations/batchtools/config.R")
lapply(packages, require, character.only = TRUE)



# --- 1. SETUP REGISTRY ---
if (!dir.exists("data/batchtools")) dir.create("data/batchtools", recursive = TRUE)

# unlink("data/batchtools/interaction_detection", recursive = TRUE)
reg = makeExperimentRegistry(file.dir = "data/batchtools/interaction_detection", packages = packages,
                             source = c("R/simulations/batchtools/simulation_setting_definition.R","R/tree_splitting.R","R/simulations/batchtools/config.R"), seed = 123)




# --- 2. ADD PROBLEMS, ALGORITHMS, EXPERIMENTS ---

source("R/simulations/batchtools/simulation_setting_definition.R")

# add problems and setting definitions
addProblem(name = "interaction_det_ind", fun = create_sim_data, reg = reg)
pdes = expand.grid(n = c(2000, 5000), type = rep(c("numeric_linear", "linear_mixed", "nonlinear","nonlinear"), each = 3))
pdes = list("interaction_det_ind" = pdes)

# add aglorithms

# define configurations
ades = data.table(impr.par = list(list(0.1, 0.15, 0.2,0.25)), n.split = 7, stringsAsFactors = FALSE)
ades = cbind(ades, feature = c(rep(("x2"),18),rep(("x10"),6)))
ades = cbind(ades, learner = c(rep(rep(c("lm","regr.ranger","regr.ksvm"),each = 2),2),rep(rep(c("gam","regr.ranger","regr.ksvm"),each = 2),2)))

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
  repls = 30L)



# --- 3. SUBMIT JOBS
Sys.time()
submitJobs(ids = reg$defs$def.id)
Sys.time()


