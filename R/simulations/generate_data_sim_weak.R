source("R/tree_splitting.R")
source("R/simulations/helper_simulation.R")
source("R/helper_general.R")
source("R/load_packages.R")
source("R/shap_interaction_index.R")

#---------------------------------------------------------------------------------------------------------------

# GENERATE DATA FOR SIMULATION STUDY WEAKNESSES OF OTHER METHODS

#---------------------------------------------------------------------------------------------------------------


# DEFINE DATA SETTINGS

L = 1000 # number of observations

# correlation matrices
cor_matrix1 = matrix(c(1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1), nrow = 4, ncol = 4, byrow = T)
cor_matrix2 = matrix(c(1,0.9,0,0,0.9,1,0,0,0,0,1,0,0,0,0,1), nrow = 4, ncol = 4, byrow = T)

# marginal distributions to generate
f_unif = function(L) qunif(L,-1,1)
list_distributions = list(f_unif, f_unif, f_unif, f_unif)

# lm formula
formula1 = as.formula("y ~ X1 + X2 + X3 + X4 + X1:X2 + X2:X3 + X1:X3 + X1:X2:X3")

# grid size
grid = 20



# GENERATE DATA

# Initial Setting
# formula = V1 + V2 + V3 + V4 + V2*V3 + 2*V1*V2 + V1*V3 + V1*V2*V3
p1 = generate_data_sim(list_distributions, formula1, cor_matrix1, L, 30, "eq", grid)

# small main effect
# formula = 0.1*V1 + V2 + V3 + V4 + V2*V3 + V1*V2 + V1*V3 + V1*V2*V3
p2 = generate_data_sim(list_distributions, formula1, cor_matrix1, L, 30, "s_main", grid)

# all main effects small 
# formula = 0.1*V1 + 0.1*V2 + 0.1*V3 + 0.1*V4 + V2*V3 + V1*V2 + V1*V3 + V1*V2*V3
p3 = generate_data_sim(list_distributions, formula1, cor_matrix1, L, 30, "s_main_uneq", grid)

# correlation rho_12 = 0.9
# formula = V1 + V2 + V3 + V4 + V2*V3 + 2*V1*V2 + V1*V3 + V1*V2*V3
p4 = generate_data_sim(list_distributions, formula1, cor_matrix2, L, 30, "eq", grid)


if (!dir.exists("data/sim_weak")) dir.create("data/sim_weak", recursive = TRUE)
save(p1,p2,p3,p4, file = "data/sim_weak/sim_sensi.RData")



