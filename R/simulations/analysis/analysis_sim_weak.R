library(ggplot2)
library(ggpubr)

#---------------------------------------------------------------------------------------------------------------

# SIMULATION STUDY WEAKNESSES OF OTHER METHODS

#---------------------------------------------------------------------------------------------------------------



if (file.exists("data/sim_weak/sim_sensi.RData")) { load("data/sim_weak/sim_sensi.RData")
} else source("R/simulations/generate_data_sim_weak.R")

# initial setup
# formula = V1 + V2 + V3 + V4 + V2*V3 + 2*V1*V2 + V1*V3 + V1*V2*V3
plot1 = ggplot(p1, aes(x = interact.feature, y = interaction)) + geom_boxplot(aes(fill = method)) +
  ylab("Interaction Strength") + xlab("Feature")  + theme_bw() + theme(plot.title = element_text(size = 8))+
  ggtitle(expression(paste(hat(f), "(x) = ", x[1], " + ", x[2], " + ", x[3], " + ", x[4], " + ", x[1], x[2], " + ", x[2], x[3], " + ", x[1], x[3], " + ", x[1], x[2], x[3]))) + 
  ylim(0,1.13) + scale_fill_discrete(name="")


# small main effect
# formula = 0.1*V1 + V2 + V3 + V4 + V2*V3 + V1*V2 + V1*V3 + V1*V2*V3
plot2 = ggplot(p2, aes(x = interact.feature, y = interaction)) + geom_boxplot(aes(fill = method)) +
  ylab("") + xlab("Feature") + theme_bw() + theme(plot.title = element_text(size = 8))+
  ggtitle(expression(paste(hat(f), "(x) = 0.1", x[1], " + ", x[2], " + ", x[3], " + ", x[4], " + ", x[1], x[2], " + ", x[2], x[3], " + ", x[1], x[3], " + ", x[1], x[2], x[3]))) + 
  ylim(0,1.13) + scale_fill_discrete(name="")


# all main effects small 
# formula = 0.1*V1 + 0.1*V2 + 0.1*V3 + 0.1*V4 + V2*V3 + V1*V2 + V1*V3 + V1*V2*V3
plot3 = ggplot(p3, aes(x = interact.feature, y = interaction)) + geom_boxplot(aes(fill = method)) +
  ylab("") + xlab("Feature") + theme_bw() + theme(plot.title = element_text(size = 8))+
  ggtitle(expression(paste(hat(f), "(x) = 0.1*(", x[1], " + ", x[2], " + ", x[3], " + ", x[4], ") + 2*", x[1], x[2], " + ", x[2], x[3], " + ", x[1], x[3], " + ", x[1], x[2], x[3]))) + 
  ylim(0,1.13) + scale_fill_discrete(name="")

# correlation rho_12 = 0.9
# formula = V1 + V2 + V3 + V4 + V2*V3 + 2*V1*V2 + V1*V3 + V1*V2*V3
plot4 = ggplot(p4, aes(x = interact.feature, y = interaction)) + geom_boxplot(aes(fill = method)) +
  ylab("Interaction Strength") + xlab("Feature") + theme_bw() + theme(plot.title = element_text(size = 8))+
  ggtitle(expression(paste(hat(f), "(x) = ", x[1], " + ", x[2], " + ", x[3], " + ", x[4], " + ", x[1], x[2], " + ", x[2], x[3], " + ", x[1], x[3], " + ", x[1], x[2], x[3], ",  ", rho[12], " = 0.9"))) + 
  ylim(0,1.13) + scale_fill_discrete(name="")


# Use a common legend for multiple plots
if (!dir.exists("figures")) dir.create("figures", recursive = TRUE)
ggpubr::ggarrange(plot1,plot2,plot3,plot4,  common.legend = TRUE) %>%
  ggpubr::ggexport(filename = "figures/sim_sensi_linear.pdf", width = 7.1, height = 5)



