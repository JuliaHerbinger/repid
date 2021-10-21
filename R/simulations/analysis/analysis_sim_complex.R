library(xtable)
source("R/helper_general.R")
source("R/load_packages.R")
source("R/simulations/helper_simulation.R")

#---------------------------------------------------------------------------------------------------------------

# SIMULATION STUDY SECTION 4.2 (COMPLEX SETTINGS) AND A.4.1 (FURTHER EXPERIMENTS) AND A.4.2 (ROBUSTNESS ANALYSIS)

#---------------------------------------------------------------------------------------------------------------

# Before this file can be run, the data needs to be generated (R/simulations/batchtools/generate_data.R)
# and results need to be reduced (R/simulations/batchtools/reduce_experiments.R)

#---------------------------------------------------------------------------------------------------------------

# path to folder, with folders for dataset results
path = "data/batchtools/1_result_tables/"
datasets = list.files(path)
type = c( "numeric_linear","linear_mixed", "nonlinear")

# create list with one list containing one dataframe per dataset
data_all = lapply(type, function(typ){
  
  df.final = NULL
  methods = c("tree", "hstat", "perf")
  
  lapply(methods, function(method){
    res.tree = readRDS(file.path(path, paste0("eval_", typ, "_", method, ".rds")))
    for(i in 1:nrow(res.tree)){
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
names(data_all) = type



# ANALYSIS OF NONLINEAR SETTING
data_nonlinear = data_all$nonlinear
data_plot = data_prep_sim_complex(data_nonlinear)


# create Figure 5
type = "nonlinear"
features = unique(data_plot$interact.feature)
data_plot$interact.feature = factor(data_plot$interact.feature, levels = c("x1", "x2",  "x3",  "x4",  "x5" , "x6" , "x7" , "x8" , "x9","x10"   ))
data_plot$method = factor(data_plot$method, levels = c("REPID", "H-Statistic" ))

feature = "x2"
n = 2000
data_plot_x2 = data_plot[data_plot$feature==feature & data_plot$n ==n,]

p1 = ggplot(data_plot_x2, aes(x = interact.feature, y = interaction, fill = learner)) + 
  facet_grid(vars(method)) + 
  geom_boxplot(aes(fill = learner)) +
  ylab("Interaction Strength") + xlab("Feature") + theme_bw() +
  theme(legend.position="top", legend.text=element_text(size=16), text = element_text(size = 16)) +
  scale_fill_manual(values = c(brewer.pal(3,"Dark2")),name = "", labels = c("GAM", "SVM", "RF"))

ggsave("figures/sim_nonlinear_x2.pdf", p1, width = 8, height = 4.8)

# average performance
data_perf = data_plot_x2 %>% dplyr::group_by(job.id, learner) %>% dplyr::summarise(mean = mean(perf))
data_perf %>% group_by(learner) %>% dplyr::summarise(mu = mean(mean),sd= sd(mean) )



# compare different values of the improvement parameter gamma
data_plot2 = rbind(data_prep_sim_complex(data_nonlinear, impr.par = 0.1),data_prep_sim_complex(data_nonlinear, impr.par = 0.15),data_prep_sim_complex(data_nonlinear, impr.par = 0.2))
data_plot2$interact.feature = factor(data_plot2$interact.feature, levels = c("x1", "x2",  "x3",  "x4",  "x5" , "x6" , "x7" , "x8" , "x9","x10"   ))
data_plot2$method = factor(data_plot2$method, levels = c("REPID", "H-Statistic" ))


data_plot2_x2 = data_plot2[data_plot2$feature==feature & data_plot2$n ==n & data_plot2$method == "REPID",]

p2 = ggplot(data_plot2_x2, aes(x = interact.feature, y = interaction, fill = learner)) + 
  facet_grid(vars(impr.par)) + 
  geom_boxplot(aes(fill = learner)) +
  ylab("Interaction Strength") + xlab("Feature") + theme_bw() +
  theme(legend.position="top", legend.text=element_text(size=16), text = element_text(size = 16)) +
  scale_fill_manual(values = c(brewer.pal(3,"Dark2")),name = "", 
                    labels = c("GAM", "SVM", "RF"))

ggsave("figures/app_nonlinear_x2_imprpar.pdf", p2, width = 8, height = 6)


# compare REPID and H-Statistic for feature x10 (non-influencial)
feature = "x10"
data_plot_x10 = data_plot[data_plot$feature==feature & data_plot$n ==n,]

p3 = ggplot(data_plot_x10, aes(x = interact.feature, y = interaction, fill = learner)) + 
  facet_grid(vars(method)) + 
  geom_boxplot(aes(fill = learner)) +
  ylab("Interaction Strength") + xlab("Feature") + theme_bw() +
  theme(legend.position="top", legend.text=element_text(size=16), text = element_text(size = 16)) +
  scale_fill_manual(values = c(brewer.pal(3,"Dark2")),name = "", 
                    labels = c("GAM", "SVM", "RF"))

ggsave("figures/app_nonlinear_x10.pdf", p3, width = 8, height = 4.8)



# ANALYSIS OF LINEAR SETTING
data_linear = data_all$numeric_linear
data_plot = data_prep_sim_complex(data_linear)


# create Figure for Appendix B
type = "numeric_linear"
features = unique(data_plot$interact.feature)
data_plot$interact.feature = factor(data_plot$interact.feature, levels = c("x1", "x2",  "x3",  "x4",  "x5" , "x6" , "x7" ))
data_plot$method = factor(data_plot$method, levels = c("REPID", "H-Statistic" ))

feature = "x2"
n = 2000
data_plot_x2 = data_plot[data_plot$feature==feature & data_plot$n ==n,]

p4 = ggplot(data_plot_x2, aes(x = interact.feature, y = interaction, fill = learner)) + 
  facet_grid(vars(method)) + 
  geom_boxplot(aes(fill = learner)) +
  ylab("Interaction Strength") + xlab("Feature") + theme_bw() +
  theme(legend.position="top", legend.text=element_text(size=16), text = element_text(size = 16)) +
  scale_fill_manual(values = c(brewer.pal(3,"Dark2")),name = "", 
                    labels = c("GAM", "SVM", "RF"))

ggsave("figures/app_linear_x2.pdf", p4, width = 8, height = 4.8)


# compare different values of the improvement parameter gamma
data_plot2 = rbind(data_prep_sim_complex(data_linear, impr.par = 0.1),data_prep_sim_complex(data_linear, impr.par = 0.15),data_prep_sim_complex(data_linear, impr.par = 0.2))
data_plot2$interact.feature = factor(data_plot2$interact.feature, levels = c("x1", "x2",  "x3",  "x4",  "x5" , "x6" , "x7" , "x8" , "x9","x10"   ))
data_plot2$method = factor(data_plot2$method, levels = c("REPID", "H-Statistic" ))


data_plot2_x2 = data_plot2[data_plot2$feature==feature & data_plot2$n ==n & data_plot2$method == "REPID",]

p5 = ggplot(data_plot2_x2, aes(x = interact.feature, y = interaction, fill = learner)) + 
  facet_grid(vars(impr.par)) + 
  geom_boxplot(aes(fill = learner)) +
  ylab("Interaction Strength") + xlab("Feature") + theme_bw() +
  theme(legend.position="top", legend.text=element_text(size=16), text = element_text(size = 16)) +
  scale_fill_manual(values = c(brewer.pal(3,"Dark2")),name = "", 
                    labels = c("GAM", "SVM", "RF"))


# save figure
if (!dir.exists("figures")) dir.create("figures", recursive = TRUE)
ggsave("figures/app_linear_x2_imprpar.pdf", p5, width = 8, height = 6)




# ROBUSTNESS ANALYSIS
data = data_nonlinear[[1]]
data = data[which(data$n == 2000 & data$impr.par==0.1 & data$feature == "x2"),]
data = data_prep_robust(data)
data$split.value = as.numeric(data$split.value)

# aggregate data for Table 3
data_aggr = data %>% dplyr::group_by(learner, depth, id.node, split.feature) %>% dplyr::summarise(count.rel = round(n()/30,2), split.mean = round(mean(split.value),2), split.sd = round(sd(split.value),2))
print(xtable(data_aggr[,2:7] ), include.rownames = FALSE)
