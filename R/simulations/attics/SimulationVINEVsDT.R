library(reshape2)
library(stringr)
library(ggplot2)
library(tidyverse)
library(Rmalschains)
library(iml)
library(ranger)
library(kmlShape)
library(dtw)
library(customtrees)
library(egg)

source("R/Simulation_Examples/helper_functions.R")
source("R/Simulation_Examples/dice_function.R")

#---------------------------------------------------------------------------------------------------------------

# Simulation study 1: Interactions between categorical and numeric features 

#---------------------------------------------------------------------------------------------------------------

# Simulate Data
set.seed(1234)
n = 500
x1 = runif(n, -1, 1)
x2 = runif(n, -1, 1)
x3 = sample(c(0, 1), size = n, replace = TRUE, prob = c(0.5, 0.5))
x4 = sample(c(0, 1), size = n, replace = TRUE, prob = c(0.7, 0.3))
eps = rnorm(n, 0, 1)

# noisy vars
x5 = sample(c(0, 1), size = n, replace = TRUE, prob = c(0.5, 0.5))
x6 = rnorm(n, mean = 1, sd = 5)

y = 0.2*x1 - 8*x2 + ifelse(x3 == 0, I(16*x2),0) + ifelse(x1 > mean(x1), I(8*x2),0) + eps

dat = data.frame(x1, x2, x3, x4, x5, x6, y)
X = dat[, setdiff(colnames(dat), "y")]

# Fit model and compute ICE for x2
set.seed(1234)
mod = ranger(y ~ ., data = dat, num.trees = 500)
#mod = rpart(y ~., data = dat, control = rpart.control(cp = 0))
pred = predict.function = function(model, newdata) predict(model, newdata)$predictions
model = Predictor$new(mod, data = X, y = dat$y, predict.function = pred)
effect = FeatureEffects$new(model, method = "ice", grid.size = 20, features = "x2")

# create ice curves dataframe for VINE
#helper_VINEICEWrite(featureEffects = effect, df_name = "data/ice_data_Sim1.csv")


#---------------------------------------------------------------------------------------------------------------

# OUR SOLUTION VIA DECISION TREE

#---------------------------------------------------------------------------------------------------------------
# Parent node (ice curves and pdp)

# Plot ICE curves: WE WANT TO FIND SUBGROUPS SUCH THAT ICE KURVES ARE HOMOGENOUS
pdp = effect$results$x2 %>% dplyr::group_by(.borders) %>% dplyr::summarise(pdp = mean(.value)) 
p_parent = ggplot(effect$results$x2, aes(x = .borders, y = .value)) + 
  geom_line(aes(group = .id), alpha = 0.2) + 
  geom_line(data = pdp, aes(y = pdp), color = "blue", lwd = 2) + theme_bw() + 
  xlab(expression(x[2])) + ylab(expression(hat(f)[2]^{PD}))

# dice curves
ice_and_pdp = FeatureEffects$new(model, method = "pdp+ice", grid.size = 20, features = "x2")

dice_curves = dice(ice_and_pdp, smooth = TRUE)

p_dice = ggplot(dice_curves$dice_curves, aes(x = .borders, y = .value, group = .id)) + geom_line(alpha = 0.2) +
  geom_line(data = dice_curves$dpdp, size = 1.5, color = "blue") + theme_bw() + 
  xlab(expression(x[2])) + ylab(expression(paste("Derivative of   ", hat(f)[2]^{PD})))

p_sd = ggplot(dice_curves$sd_deriv, aes(x = gridpts, y = .sd)) + geom_line(size = 1.5, color = "orange") + theme_bw() + 
  xlab(expression(x[2])) + ylab("sd(deriv)")

# arrange plots
cowplot::plot_grid(p_dice, p_sd, align = "v", ncol = 1, rel_heights = c(0.75, 0.25))
p_dice_sd = egg::ggarrange(p_dice, p_sd, heights = c(0.75, 0.25))



# First Split: Parent Node

# Get ICE values and arrange them in a horizontal matrix
Y = spread(effect$results$x2, .borders, .value)
Y = Y[, setdiff(colnames(Y), c(".type", ".id", ".feature"))]

# center ICE curves by their mean
for(i in 1:nrow(Y)){
  Y[i,] = as.numeric(unname(Y[i,])) - mean(as.numeric(unname(Y[i,])))
}

# first split 
sp_L2 = split_parent_node(Y = Y, X = X, objective = SS_L2,
                               n.splits = 1, optimizer = find_best_binary_split)
SS_L2(Y,X) # before split
sp_L2 # after split


# Visualize results
node_index = generate_node_index(Y, X, result = sp_L2)
plot_data = effect$results$x2
plot_data$.split = node_index$class[plot_data$.id]
pdp_split1 = plot_data %>% dplyr::group_by(.split, .borders) %>% dplyr::summarise(pdp = mean(.value))

split_lab = c("x3 = 0", "x3 = 1")
names(split_lab) = levels(plot_data$.split)
p_split1 = ggplot(plot_data, aes(x = .borders, y = .value)) + 
  geom_line(aes(group = .id), alpha = 0.2) + 
  geom_line(data = pdp_split1, aes(y = pdp, group = .split), color = "blue", lwd = 2) + facet_grid(~ .split, labeller = labeller(.split = split_lab)) +
  theme_bw() + theme( text = element_text(size = 16)) +
  xlab(expression(x[2])) + ylab(expression(hat(f)[2]^{PD}))



#---------------------------------------------------------------------------------------------------------------
# Second Splits: Child nodes

# child node 1:

# L2 distance for child node and node_index
index_c1 = node_index$index$`[0,0.5]`
sp_L2_21 = split_parent_node(Y = Y[index_c1,], X = X[index_c1,], objective = SS_L2,
                                  n.splits = 1, optimizer = find_best_binary_split)

# create plot.data for child node
plot_data_child1 = helper_prepChildPlot(sp = sp_L2_21, index = index_c1, plot.data = plot_data, multi = FALSE)
pdp_split2_1 = plot_data_child1 %>% dplyr::group_by(.split, .borders) %>% dplyr::summarise(pdp = mean(.value))

# plot ICE curves of child node splitted by x1: 
# ggplot(plot.data.child1, aes(x = .borders, y = .value)) + 
#   geom_line(aes(group = .id)) + facet_grid(~ .split)

split.lab = c("x1 <= 0", "x1 > 0")
names(split.lab) = levels(pdp_split2_1$.split)
p_split2_1 = ggplot(plot_data_child1, aes(x = .borders, y = .value)) + 
  geom_line(aes(group = .id), alpha = 0.2) + 
  geom_line(data = pdp_split2_1, aes(y = pdp, group = .split), color = "blue", lwd = 2) + facet_grid(~ .split, labeller = labeller(.split = split.lab)) +
  theme_bw() +
    theme( text = element_text(size = 16)) +
  xlab(expression(x[2])) + ylab(expression(hat(f)[2]^{PD}))



# child node 2:

# L2 distance for child node and node_index
index_c2 = node_index$index$`(0.5,1]`
sp_L2_22 = split_parent_node(Y = Y[index_c2,], X = X[index_c2,], objective = SS_L2,
                                  n.splits = 1, optimizer = find_best_binary_split)

# create plot.data for child node
plot_data_child2 = helper_prepChildPlot(sp = sp_L2_22, index = index_c2, plot.data = plot_data, multi = FALSE)
pdp_split2_2 = plot_data_child2 %>% dplyr::group_by(.split, .borders) %>% dplyr::summarise(pdp = mean(.value))

# plot ICE curves of child node splitted by x1: 
# ggplot(plot.data.child2, aes(x = .borders, y = .value)) + 
# geom_line(aes(group = .id)) + facet_grid(~ .split)

split_lab = c("x1 <= 0", "x1 > 0")
names(split_lab) = levels(plot_data_child2$.split)
p_split2_2 = ggplot(plot_data_child2, aes(x = .borders, y = .value)) + 
  geom_line(aes(group = .id), alpha = 0.2) + 
  geom_line(data = pdp_split2_2, aes(y = pdp, group = .split), color = "blue", lwd = 2) + facet_grid(~ .split, labeller = labeller(.split = split_lab)) +
  theme_bw() + theme( text = element_text(size = 16)) +
  xlab(expression(x[2])) + ylab(expression(hat(f)[2]^{PD})) + ylim(-12,12)





#---------------------------------------------------------------------------------------------------------------

# SOLUTION WITH VINE

#---------------------------------------------------------------------------------------------------------------


# Example 1: originally 5 Clusters - reduced to 4 Clusters
#df_VINE = helper_VINEICEPrep("data/groupedICECurves5Clusters.csv")   
df_VINE = helper_VINEICEPrep("data/result5Clusters.csv")   

# 'clusters': [{'accuracy': 98,
#   'precision': 95,
#   'recall': 100,
#   'split_feature': 'x1',
#   'split_val': 0.02,
#   'split_direction': '<=',
#   
#   'accuracy': 65,
#   'precision': 28,
#   'recall': 100,
#   'split_feature': 'x3',
#   'split_val': 0.5,
#   'split_direction': '<=',
#   
#   'accuracy': 90,
#   'precision': 79,
#   'recall': 100,
#   'split_feature': 'x1',
#   'split_val': 0.04,
#   'split_direction': '>',


#Create a custom color scale
library(RColorBrewer)
myColors <- brewer.pal(5,"Set1")[3:5]
names(myColors) <- levels(df_VINE$cluster_label)
colScale <- scale_colour_manual(name = "cluster",values = myColors)

p_vine5 = ggplot(df_VINE, aes(x = variable, y = value)) + 
  geom_line(aes(group = X, colour = cluster_label), alpha = 0.2) + colScale

# Cluster 0: x1 <=0.02
# Cluster 1: x3 <=0.5
# Cluster 3: x1 > 0.04

pdps = df_VINE %>% dplyr::group_by(cluster_label, variable) %>% dplyr::summarise(pdp = mean(value)) 

p_vine5 = p_vine5 + geom_line(data = pdps, aes(x = variable, y = pdp, color = cluster_label), lwd = 2, lty = 2) + theme_bw() +
  xlab(expression(x[2])) + ylab(expression(hat(f)[2]^{PD}))

# Example 2: originally 2 Clusters - not merged                      
df_VINE = helper_VINEICEPrep("data/result2Clusters.csv")   

myColors <- brewer.pal(5,"Set1")[3:4]
names(myColors) <- levels(df_VINE$cluster_label)
colScale <- scale_colour_manual(name = "cluster",values = myColors)

p_vine2 = ggplot(df_VINE, aes(x = variable, y = value)) + 
  geom_line(aes(group = X, colour = cluster_label), alpha = 0.2) + colScale

# Cluster 0: x1 <=0.02
# Cluster 1: x3 <=0.5
# Cluster 3: x1 > 0.04

pdps = df_VINE %>% dplyr::group_by(cluster_label, variable) %>% dplyr::summarise(pdp = mean(value)) 

p_vine2 = p_vine2 + geom_line(data = pdps, aes(x = variable, y = pdp, color = cluster_label), lwd = 2, lty = 2) + theme_bw() + 
  xlab(expression(x[2])) + ylab(expression(hat(f)[2]^{PD}))

# Cluster 0: x3 <=0.5
# Cluster 1: x3 >0.5


# save plots
ggsave("figures/sim1_allcurves.png", p_parent, width = 4, height = 3.5)
ggsave("figures/sim1_dice.png", p_dice_sd, width = 4, height = 3.5)
ggsave("figures/sim1_vine2clusters.png", p_vine2, width = 4, height = 3)
ggsave("figures/sim1_vine5clusters.png", p_vine5, width = 4, height = 3)

ggsave("figures/sim1_dt_split1.png", p_split1, width = 5, height = 2.8)
ggsave("figures/sim1_dt_split2_1.png", p_split2_1, width = 5, height = 2.8)
ggsave("figures/sim1_dt_split2_2.png", p_split2_2, width = 5, height = 2.8)


################################################################################################################
################################################################################################################

#---------------------------------------------------------------------------------------------------------------

# Simulation Study 2: Continuous interaction

#---------------------------------------------------------------------------------------------------------------



# # new y_hat with continuous interactions
# y = 0.2*x1 - 8*x2^2 + 5*cos(x2*5)*x6 + ifelse(x3 == 0, I(8*x2),0) + eps
# dat = data.frame(x1, x2, x3, x4, x5, x6, y)
# X = dat[, setdiff(colnames(dat), "y")]
# 
# 
# # Fit model and compute ICE for x2
# mod = ranger(y ~ ., data = dat, num.trees = 1000)
# pred = predict.function = function(model, newdata) predict(model, newdata)$predictions
# model = Predictor$new(mod, data = X, y = dat$y, predict.function = pred)
# effect = FeatureEffects$new(model, method = "ice", grid.size = 20, features = "x2")
# 
# # create ice curves dataframe for VINE
# #helper_VINEICEWrite(featureEffects = effect, df_name = "data/ice_data_Sim2.csv")
# 
# 
# #---------------------------------------------------------------------------------------------------------------
# 
# # OUR SOLUTION VIA DECISION TREE
# 
# #---------------------------------------------------------------------------------------------------------------
# 
# # First Split: Parent Node - Multisplits
# 
# # Plot ICE curves: WE WANT TO FIND SUBGROUPS SUCH THAT ICE KURVES ARE HOMOGENOUS
# ggplot(effect$results$x2, aes(x = .borders, y = .value)) + 
#   geom_line(aes(group = .id))
# 
# # Get ICE values and arrange them in a horizontal matrix
# Y = spread(effect$results$x2, .borders, .value)
# Y = Y[, setdiff(colnames(Y), c(".type", ".id", ".feature"))]
# 
# # find best multiway split for parend node (38766)
# sp_multi = lapply(1:3, function(i) {
#   split_parent_node(Y = Y, X = X, objective = SS_fre, 
#                     n.splits = i, optimizer = find_best_multiway_split2, min.node.size = 10)
# })
# 
# sp_multi_unlist = rbindlist(sp_multi, idcol = "n.splits")
# n_split = sp_multi_unlist$n.splits[which(sp_multi_unlist$objective.value ==min(sp_multi_unlist$objective.value))]
# # best one for all combinations is x6 --> minimum is reached with five splits
# # ab wann hört man auf? prozentualer threshold oder sobald ansteigt?
# 
# 
# # Plot ice curves clustered in resulting nodes
# node_index_multiway_frechet = generate_node_index(Y, X, result = sp_multi[[n_split]])
# 
# plot.data = effect$results$x2
# plot.data$.split = node_index_multiway_frechet$class[plot.data$.id]
# 
# ggplot(plot.data, aes(x = .borders, y = .value)) + 
#   geom_line(aes(group = .id)) + facet_grid(~ .split)
# 
# # plot 2dim pdp plot for x2 and x6
# eff <- FeatureEffect$new(model, method = "pdp", feature = c("x2", "x6"))
# plot(eff)
# # it shows very well the interaction between those two for the different intervals shown in the ice plots
# 
# 
# # comparison to centered ICE curves
# for(i in 1:nrow(Y)){
#   Y[i,] = as.numeric(unname(Y[i,])) - mean(as.numeric(unname(Y[i,])))
# }
# 
# 
# #---------------------------------------------------------------------------------------------------------------
# # Second Splits: Child nodes
# 
# 
# 
# # 1. weitere binäre splits getestet. zweimal x3 mit abstand am besten, bei anderen x6 oder x2 aber nicht so 
# # enormer unterschied zu anderen --> wann splitte ich, wieviel differenz zu anderen, um nicht unendlich viele 
# # Splits zu haben?
# 
# 
# # frechet distance for child node and node_index
# index_c1 = node_index_multiway_frechet$index$`(-8.81,-3.3]`
# sp_frechet_21 = split_parent_node(Y = Y[index_c1,], X = X[index_c1,], objective = SS_fre,
#                                   n.splits = 1, optimizer = find_best_binary_split, min.node.size = 10)
# 
# # create plot.data for child node
# plot.data.child1 = helper_prepChildPlot(sp = sp_frechet_21, index = index_c1, plot_data = plot.data, multi = FALSE)
# 
# # plot ICE curves of child node splitted by x1: 
# ggplot(plot.data.child1, aes(x = .borders, y = .value)) + 
#   geom_line(aes(group = .id)) + facet_grid(~ .split)
# 
# 
# # 2. multiway splits testen:
# 
# # frechet distance for child node and node_index
# index_c1 = node_index_multiway_frechet$index$`(-3.3,1.91]`
# 
# # find best multiway split for parent node
# sp_multi_21 = lapply(1:5, function(i) {
#   split_parent_node(Y = Y[index_c1,], X = X[index_c1,], objective = SS_fre, 
#                     n.splits = i, optimizer = find_best_multiway_split2, min.node.size = 10)
# })
# 
# # create plot.data for child node
# plot.data.child1 = helper_prepChildPlot(sp = sp_multi_21, index = index_c1, plot_data = plot.data, multi = TRUE)
# 
# # plot ICE curves of child node splitted by x1: 
# ggplot(plot.data.child1, aes(x = .borders, y = .value)) + 
#   geom_line(aes(group = .id)) + facet_grid(~ .split)
# 
# 
# # Results:
# 
# # despite multiway split, x3 is still best split criteria for interval 2: (4600)
# # [[5]]
# # feature objective.value                                                split.points best.split
# # 1:      x1        4426.103      -0.3040786,-0.1479441, 0.1382904, 0.4225452, 0.5838667      FALSE
# # 2:      x2        4438.694 -0.40472146, 0.07768402, 0.17576518, 0.39259544, 0.65204970      FALSE
# # 3:      x3        3695.099                                                         0.5       TRUE
# # 4:      x4        4430.134                                                         0.5      FALSE
# # 5:      x5        4593.496                                                         0.5      FALSE
# # 6:      x6        4066.151 -2.93432373,-1.97107517,-1.26743679, 0.09623105, 0.38722219      FALSE
# 
# 
# # same for interval 3: (4460)
# # [[1]]
# # feature objective.value split.points best.split
# # 1:      x1        4350.271   -0.8963868      FALSE
# # 2:      x2        4396.216    0.2575906      FALSE
# # 3:      x3        3088.571          0.5       TRUE
# # 4:      x4        4275.589          0.5      FALSE
# # 5:      x5        4428.542          0.5      FALSE
# # 6:      x6        4276.331     2.210518      FALSE
# 
# 
# # but not for interval 5: (5028) --> binary split it is x3 then x2: when to split?
# # [[1]]
# # feature objective.value split.points best.split
# # 1:      x1        4890.007    0.4723322      FALSE
# # 2:      x2        4509.946    0.3424933      FALSE
# # 3:      x3        4223.410          0.5       TRUE
# # 4:      x4        4722.806          0.5      FALSE
# # 5:      x5        5031.059          0.5      FALSE
# # 6:      x6        4378.663     9.364002      FALSE
# # 
# # [[2]]
# # feature objective.value          split.points best.split
# # 1:      x1        4625.710   0.1737772,0.4739254      FALSE
# # 2:      x2        3624.315 -0.2002253, 0.2212658       TRUE
# # 3:      x3        4223.410                   0.5      FALSE
# # 4:      x4        4722.806                   0.5      FALSE
# # 5:      x5        5031.059                   0.5      FALSE
# # 6:      x6        4160.908     7.723611,9.355219      FALSE
# # 
# # [[3]]
# # feature objective.value                           split.points best.split
# # 1:      x1        4660.581 -0.457567229,-0.005074572, 0.231841058      FALSE
# # 2:      x2        3669.188       -0.1877496, 0.1409008, 0.4964286       TRUE
# # 3:      x3        4223.410                                    0.5      FALSE
# # 4:      x4        4722.806                                    0.5      FALSE
# # 5:      x5        5031.059                                    0.5      FALSE
# # 6:      x6        4113.682           8.138894, 9.009494,10.259465      FALSE
# 
# 
# # first intervall: (6137)
# # binary split would choose x6, but for more splits it is definately x2:
# # [[1]]
# # feature objective.value split.points best.split
# # 1:      x1        5690.180   -0.3665886      FALSE
# # 2:      x2        5641.785    0.3679569      FALSE
# # 3:      x3        5529.060          0.5      FALSE
# # 4:      x4        6089.885          0.5      FALSE
# # 5:      x5        5824.488          0.5      FALSE
# # 6:      x6        5385.969     -8.75305       TRUE
# # 
# # [[2]]
# # feature objective.value          split.points best.split
# # 1:      x1        5213.500 -0.3608856, 0.3319700      FALSE
# # 2:      x2        4078.811 -0.2780358, 0.2244429       TRUE
# # 3:      x3        5529.060                   0.5      FALSE
# # 4:      x4        6089.885                   0.5      FALSE
# # 5:      x5        5824.488                   0.5      FALSE
# # 6:      x6        5203.491   -8.769317,-5.808904      FALSE
# # 
# # [[3]]
# # feature objective.value                     split.points best.split
# # 1:      x1        5173.326 -0.8525032,-0.3652667, 0.2974587      FALSE
# # 2:      x2        3925.254 -0.8116910,-0.2990416, 0.2711271       TRUE
# # 3:      x3        5529.060                              0.5      FALSE
# # 4:      x4        6089.885                              0.5      FALSE
# # 5:      x5        5824.488                              0.5      FALSE
# # 6:      x6        5148.990    -9.061001,-7.974370,-5.801737      FALSE
# 
# # but is this reasonable?
# 
# 
# # same question for interval 4: changes between x2 and x6 and x3 is quite close --> split reasonable? (2394)
# # [[1]]
# # feature objective.value split.points best.split
# # 1:      x1        2308.801    0.5855359      FALSE
# # 2:      x2        2252.236    0.3269685      FALSE
# # 3:      x3        2175.992          0.5      FALSE
# # 4:      x4        2239.925          0.5      FALSE
# # 5:      x5        2400.904          0.5      FALSE
# # 6:      x6        2136.647     5.752303       TRUE
# # 
# # [[2]]
# # feature objective.value          split.points best.split
# # 1:      x1        2246.470 0.09703509,0.61769736      FALSE
# # 2:      x2        2027.426 -0.2037971, 0.3203221      FALSE
# # 3:      x3        2175.992                   0.5      FALSE
# # 4:      x4        2239.925                   0.5      FALSE
# # 5:      x5        2400.904                   0.5      FALSE
# # 6:      x6        1997.435     5.220918,6.159762       TRUE
# # 
# # [[3]]
# # feature objective.value                     split.points best.split
# # 1:      x1        2194.175 -0.3751111, 0.1307950, 0.5841682      FALSE
# # 2:      x2        1960.306 -0.1570971, 0.3140077, 0.8086073       TRUE
# # 3:      x3        2175.992                              0.5      FALSE
# # 4:      x4        2239.925                              0.5      FALSE
# # 5:      x5        2400.904                              0.5      FALSE
# # 6:      x6        2005.127       5.212499,6.127613,6.497318      FALSE
# 
# 
# 
# #---------------------------------------------------------------------------------------------------------------
# # test interacting features for other features than x2:
# 
# var = "x5"
# effect = FeatureEffects$new(model, method = "ice", grid.size = 20, features = var)
# 
# # Plot ICE curves: WE WANT TO FIND SUBGROUPS SUCH THAT ICE KURVES ARE HOMOGENOUS
# ggplot(effect$results$x5, aes(x = .borders, y = .value)) + 
#   geom_line(aes(group = .id))
# 
# # Get ICE values and arrange them in a horizontal matrix
# Y = spread(effect$results$x5, .borders, .value)
# Y = Y[, setdiff(colnames(Y), c(".type", ".id", ".feature"))]
# 
# # find best multiway split for parent node (38766)
# sp_multi = lapply(1:3, function(i) {
#   split_parent_node(Y = Y, X = X, objective = SS_fre, 
#                     n.splits = i, optimizer = find_best_multiway_split2, min.node.size = 10)
# })
# 
# SS_fre(Y,X)
# sp_multi
# 
# # Visualize results
# sp_multi_unlist = rbindlist(sp_multi, idcol = "n.splits")
# n_split = sp_multi_unlist$n.splits[which(sp_multi_unlist$objective.value ==min(sp_multi_unlist$objective.value))]
# node_index_frechet = generate_node_index(Y, X, result = sp_multi[[n_split]])
# plot.data = effect$results$x1
# plot.data$.split = node_index_frechet$class[plot.data$.id]
# 
# ggplot(plot.data, aes(x = .borders, y = .value)) + 
#   geom_line(aes(group = .id)) + facet_grid(~ .split)
# 
# 
# # x1 is not interacting with any other variable (not in real data, maybe in model?) - best multiway split
# # reduces original objective by around 16% - splitted by x6, but when to stop splitting? some intervals with a couple of curves
# 
# # x4 not included at all: shows x2 then x6 as best split criterion. With 3 Splits it is an improvement of 13,7%
# # for x5 it is x2 with 4 splits and an improvement of 15% --> how to decide on real interactions?
# 
# # for all multiway splits x2 is the best for x3 and x6. Improvement of objective is way higher for x6 (45%),
# # than for x3 (only 17,7%)
# 
# 
# # comparison to centered ICE curves
# for(i in 1:nrow(Y)){
#   Y[i,] = as.numeric(unname(Y[i,])) - mean(as.numeric(unname(Y[i,])))
# }
# 
# 
# #---------------------------------------------------------------------------------------------------------------
# 
# # SOLUTION WITH VINE
# 
# #---------------------------------------------------------------------------------------------------------------
# 
# 
# # Example 1: originally 5 Clusters merged to 3 for x2
# df_VINE = helper_VINEICEPrep("data/Sim25ClustersR.csv")
# 
# ggplot(df_VINE, aes(x = variable, y = value)) + 
#   geom_line(aes(group = X, colour = cluster_label))
# 
# # 3 Clusters:
# # Cluster0: x6 <= -1.55
# # Cluster1: x6 >  2.99
# # Cluster2: x6 > - 1.01
# 
# # Optimcally Cluster look good, but explanation for continuous interactions is not that intuitive, since
# # no intervals are regarded and explanations are overlapping.
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
