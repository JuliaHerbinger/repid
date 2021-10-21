
library(devtools)
load_all("../customtrees")
source("R/Simulation_Examples/batchtools/tree_splitting.R") # add in package
source("R/Simulation_Examples/batchtools/helper_simulation.r")
source("R/Simulation_Examples/batchtools/simulation_setting_definition.R")


library(batchtools)
library(data.table)




# --- 1. SETUP REGISTRY ---

# unlink("R/Simulation_Examples/batchtools/t", recursive = TRUE)
reg = makeExperimentRegistry(file.dir = "R/Simulation_Examples/batchtools/t", seed = 123)



# --- 2. ADD PROBLEMS, ALGORITHMS, EXPERIMENTS ---

# add problems and setting definitions
addProblem(name = "all_1000", fun = create_sim_data, reg = reg)
# addProblem(name = "categorical_linear", fun = create_sim_data, reg = reg)
# addProblem(name = "numeric_linear", fun = create_sim_data, reg = reg)
# addProblem(name = "linear_mixed", fun = create_sim_data, reg = reg)
# addProblem(name = "nonlinear", fun = create_sim_data, reg = reg)
# addProblem(name = "friedman", fun = create_sim_data, reg = reg)
pdes = list()
# pdes$categorical_linear = data.table(n = rep(c(1000, 5000),18), type = "categorical_linear")
# pdes$numeric_linear = data.table(n = rep(c(1000, 5000),18), type = "numeric_linear")
# pdes$linear_mixed = data.table(n = rep(c(1000, 5000),18), type = "linear_mixed")
# pdes$nonlinear = data.table(n = rep(c(1000, 5000),18), type = "nonlinear")
# pdes$friedman = data.table(n = rep(c(1000, 5000),18), type = "friedman")
tab = as.data.frame(expand.grid(n = c(1000, 5000), type = rep(c("categorical_linear", "numeric_linear", "linear_mixed", "nonlinear","friedman"), each = 18),stringsAsFactors = FALSE))
pdes$all_1000 = tab

tab = data.table(n = c(1000), type = rep(c("categorical_linear", "numeric_linear", "linear_mixed", "nonlinear","friedman"), each = 18),stringsAsFactors = FALSE)
pdes$all_1000 = tab
#prob.designs$rexp = data.table(n = 100, lambda = 1:5)
#algo.designs$average = data.table(method = c("mean", "median"))
#algo.designs$deviation = data.table()
#pdes = list("test" = pdes)

# add aglorithms

# define configurations
ades = data.table(impr.par = rep(rep(c(0.2, 0.3, 0.4), each = 4),15), n.split = 6, stringsAsFactors = FALSE)
#grid = expand.grid(seq(1, nrow(ades)), learner = c("lm","regr.ranger","regr.ksvm"),  stringsAsFactors = FALSE)
#ades = cbind(ades[grid$Var1, ], learner = grid$learner, impr.par = grid$impr.par)
ades = cbind(ades, feature = c(rep(rep(c("x2","x6"), each = 2),18),rep(rep(c("x2","x10"), each = 2),27)))
ades = cbind(ades, learner = c(rep(rep(c("lm","regr.ranger","regr.ksvm"),each = 12),3),rep(rep(c("gam","regr.ranger","regr.ksvm"),each = 12),2)))


###
ades = data.table(impr.par = rep(rep(c(0.2, 0.3, 0.4), each = 2),15), n.split = 6, stringsAsFactors = FALSE)
#grid = expand.grid(seq(1, nrow(ades)), learner = c("lm","regr.ranger","regr.ksvm"),  stringsAsFactors = FALSE)
#ades = cbind(ades[grid$Var1, ], learner = grid$learner, impr.par = grid$impr.par)
ades = cbind(ades, feature = c(rep(rep(c("x2","x6"), each = 1),18),rep(rep(c("x2","x10"), each = 1),27)))
ades = cbind(ades, learner = c(rep(rep(c("lm","regr.ranger","regr.ksvm"),each = 6),3),rep(rep(c("gam","regr.ranger","regr.ksvm"),each = 6),2)))

###

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
  repls = 4L)



# --- 3. SUBMIT JOBS
Sys.time()
submitJobs()
Sys.time()


