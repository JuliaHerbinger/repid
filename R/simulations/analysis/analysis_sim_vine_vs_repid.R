
source("R/load_packages.R")
source("R/helper_general.R")
source("R/simulations/helper_simulation.R")
source("R/dice.R")

#---------------------------------------------------------------------------------------------------------------

# SIMULATION STUDY VINE VS REPID 

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
pred = predict.function = function(model, newdata) predict(model, newdata)$predictions
model = Predictor$new(mod, data = X, y = dat$y, predict.function = pred)
effect = FeatureEffects$new(model, method = "ice", grid.size = 20, features = "x2")

# create ice curves dataframe for VINE
if (!dir.exists("data/sim_vine_vs_repid")) dir.create("data/sim_vine_vs_repid", recursive = TRUE)
if(!file.exists("data/sim_vine_vs_repid/ice_data_Sim1.csv")) helper_vine_write(featureEffects = effect, df_name = "data/sim_vine_vs_repid/ice_data_Sim1.csv")
if(!file.exists("data/sim_vine_vs_repid/data.csv")) write.csv(file = "data/sim_vine_vs_repid/data.csv", x = X, row.names = FALSE) 

#---------------------------------------------------------------------------------------------------------------

# OUR SOLUTION VIA REPID

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


# Example 1: originally 5 Clusters - reduced to 3 Clusters
df_VINE = helper_vine_ice("data/sim_vine_vs_repid/result5Clusters.csv")   

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
df_VINE = helper_vine_ice("data/sim_vine_vs_repid/result2Clusters.csv")   

myColors <- brewer.pal(5,"Set1")[3:4]
names(myColors) <- levels(df_VINE$cluster_label)
colScale <- scale_colour_manual(name = "cluster",values = myColors)

p_vine2 = ggplot(df_VINE, aes(x = variable, y = value)) + 
  geom_line(aes(group = X, colour = cluster_label), alpha = 0.2) + colScale

pdps = df_VINE %>% dplyr::group_by(cluster_label, variable) %>% dplyr::summarise(pdp = mean(value)) 

p_vine2 = p_vine2 + geom_line(data = pdps, aes(x = variable, y = pdp, color = cluster_label), lwd = 2, lty = 2) + theme_bw() + 
  xlab(expression(x[2])) + ylab(expression(hat(f)[2]^{PD}))

# Cluster 0: x3 <=0.5
# Cluster 1: x3 >0.5



# save plots
if (!dir.exists("figures")) dir.create("figures", recursive = TRUE)

# Figure 1
ggsave("figures/sim1_allcurves.png", p_parent, width = 4, height = 3.5)
ggsave("figures/sim1_dice.png", p_dice_sd, width = 4, height = 3.5)

# Figure 2
ggsave("figures/sim1_vine2clusters.png", p_vine2, width = 4, height = 3)
ggsave("figures/sim1_vine5clusters.png", p_vine5, width = 4, height = 3)

# Figure 3
ggsave("figures/sim1_dt_split1.png", p_split1, width = 5, height = 2.8)
ggsave("figures/sim1_dt_split2_1.png", p_split2_1, width = 5, height = 2.8)
ggsave("figures/sim1_dt_split2_2.png", p_split2_2, width = 5, height = 2.8)


