source("R/load_packages.R")
source("R/tree_splitting.R")
source("R/helper_general.R")

# load prepared dataset
library(MASS)
data = Pima.te


# Train model
set.seed(123)
task = makeClassifTask(id = "pima", data = data, target = "type")
lrn = makeLearner("classif.ksvm", predict.type = "prob")
mlr::resample(lrn, task, cv5, measures = bac)
rf = mlr::train(task = task, learner = lrn)

# calculate ice curves 
mod <- Predictor$new(rf, data = data, type = "prob", class = "Yes")
eff <- FeatureEffect$new(mod, feature = "skin", grid.size = 20, method = "pdp+ice")

# compute tree and extract split criteria
X = data[,-which(colnames(data)=="type")]
tree = compute_tree(eff, X, n.split = 3, impr.par = 0.2, min.split = 30)
res = extract_split_criteria(tree)
res

# define regional pdps
pdps = lapply(tree, function(depth) {
  pdp = lapply(1:length(depth), function(node){
    if(is.null(depth[[node]]$split.feature) & !is.null(depth[[node]]$subset.idx)){
      pdp = cbind(aggregate(formula = .value~skin, data = eff$results[eff$results$.id %in% depth[[node]]$subset.idx,], FUN = mean), depth = depth[[node]]$depth, id = node) 
      pdp
    }
  })
  do.call("rbind", pdp)
})

pdps = list.clean(pdps)
pdps = do.call("rbind",pdps)
pdps$group = paste0(pdps$depth, pdps$id)


# create plots for Figure 6
p1 = ggplot(eff$results[is.na(eff$results$.id),], aes(x = skin, y = .value)) + 
  geom_line(data = eff$results[!is.na(eff$results$.id),], aes(group = .id), alpha = 0.3) + geom_line(lwd = 2, col = "blue") + theme_bw() + ylim(0,1) +
  ylab("Predicted probability for diabetes") + ggtitle("Global PD Plot") + xlab("Skin")

p2 = ggplot(pdps, aes(x = skin, y = .value, group = group)) + geom_line(aes(col = group), lwd = 1.5) + theme_bw()+ ylim(0,1) +
  ylab("") + guides(color=guide_legend(nrow=2,byrow=TRUE)) +
  theme(legend.position="bottom", legend.text=element_text(size=9)) + xlab("Skin") +
  scale_color_manual(values = c(brewer.pal(9,"Oranges")[c(3)],brewer.pal(9,"Greens")[c(4,7)],brewer.pal(9,"Oranges")[c(5,7)]),name = "", 
                     labels = c("Glu <= 133 & Npreg > 5", "Glu > 133 & Age <= 27", "Glu > 133 & Age > 27", "Glu <= 133 & Npreg <= 5 & Bmi <= 30", "Glu <= 133 & Npreg <= 5 & Bmi > 30")) + ggtitle("Regional PD Plots")


# save Figure
if (!dir.exists("figures")) dir.create("figures", recursive = TRUE)
ggpubr::ggarrange(p1,p2, common.legend = TRUE, legend = "bottom") %>%
  ggpubr::ggexport(filename = "figures/pima_skin.pdf", width = 7, height = 3.8)
