source("R/load_packages.R")
source("R/tree_splitting.R")
source("R/helper_general.R")

# load prepared dataset and convert features
data = read.csv("data/california_housing/housing.csv")
data$ocean_proximity = factor(data$ocean_proximity, levels = c("INLAND","<1H OCEAN", "NEAR BAY", "NEAR OCEAN", "ISLAND"))
data$ocean_proximity = as.integer(data$ocean_proximity)
data$total_bedrooms[is.na(data$total_bedrooms)] = median(data$total_bedrooms, na.rm = TRUE)
data$median_house_value = log10(data$median_house_value)
data$median_income = log(data$median_income)
data$total_rooms = log(data$total_rooms)
data$total_bedrooms = log(data$total_bedrooms)
data$population = log(data$population)
data$households = log(data$households)
summary(data)

# tune neuralnet
set.seed(123)
task = makeRegrTask(data = data, target = "median_house_value")
ps = makeParamSet(
  makeDiscreteParam("decay", values = c(0.5, 0.1, 1e-2, 1e-3, 1e-4, 1e-5)),
  makeDiscreteParam("size", values = c(3, 5, 10, 20, 30))
)
ctrl = makeTuneControlGrid()
rdesc = makeResampleDesc("CV", iters = 5L)
res = tuneParams(makeLearner("regr.nnet",  maxit = 1000), task = task, resampling = rdesc,
                 par.set = ps, control = ctrl,  measures = list(mlr::mse, mlr::mae, mlr::rsq))



# set hyperparameters
set.seed(123)
#mlr::resample(lrn, task, cv5, measures = list(mlr::mse, mlr::mae, mlr::rsq))
# lrn = setHyperPars(makeLearner("regr.nnet",  maxit = 1000),  size = res$x$size,
#                    decay = res$x$decay, trace = F)
lrn = makeLearner("regr.nnet",  maxit = 1000,  size = 20, decay = 0.1, trace = F)
model = mlr::train(task = task, learner = lrn)

# either use all data or a subset
data_sample = data
#data_sample = data[sample(x = 1:nrow(data), size = 2000),]

# calculate ice curves for age
mod <- Predictor$new(model, data = data_sample[which(names(data_sample)!="median_house_value")], y = data_sample$median_house_value)

# calculate feature importance
#fi = FeatureImp$new(mod, loss = "mse")

# calculate effects for feature longitude
eff <- FeatureEffect$new(mod, feature = "longitude", grid.size = 20, method = "pdp+ice")
eff$plot()

# compute tree and extract split criteria
X = data_sample[,-which(colnames(data_sample)=="median_house_value")]
tree = compute_tree(eff, X, n.split = 3, impr.par = 0.25, min.split = 30)
res = extract_split_criteria(tree)
res

# define regional pdps
pdps = lapply(tree, function(depth) {
  pdp = lapply(1:length(depth), function(node){
    if(is.null(depth[[node]]$split.feature) & !is.null(depth[[node]]$subset.idx)){
      pdp = cbind(aggregate(formula = .value~longitude, data = eff$results[eff$results$.id %in% depth[[node]]$subset.idx,], FUN = mean), depth = depth[[node]]$depth, id = node) 
      pdp
    }
  })
  do.call("rbind", pdp)
})

pdps = list.clean(pdps)
pdps = do.call("rbind",pdps)
pdps$group = paste0(pdps$depth, pdps$id)


# create plots for Figure 6
scientific_10 <- function(x) {
  parse(text=gsub("e", "%*% 10^", scales::scientific_format()(10^x)))
}
p1 = ggplot(eff$results[is.na(eff$results$.id),], aes(x = longitude, y = .value)) + 
  geom_line(data = eff$results[!is.na(eff$results$.id),], aes(group = .id), alpha = 0.1) + geom_line(lwd = 2, col = "blue") + theme_bw() + 
  scale_y_continuous(label=scientific_10, limits = c(4.2, 6))+ xlab("Longitude") +
  ylab("Predicted median house value") + ggtitle("Global PD Plot")

p2 = ggplot(pdps, aes(x = longitude, y = .value, group = group)) + geom_line(aes(col = group), lwd = 1.5) + theme_bw()+ #ylim(3.5,7.5) +
  ylab("") + 
  theme(legend.position="bottom", legend.text=element_text(size=8)) +
  guides(color=guide_legend(nrow=2,byrow=FALSE)) + ggtitle("Regional PD Plots") +
  scale_y_continuous(label=scientific_10, limits = c(4.2, 6)) + xlab("Longitude") +
scale_color_manual(values = c(brewer.pal(8,"Oranges")[c(3,5,7,8)]),name = "", 
                   labels = c("34.5 < Latitude", "Latitude <= 34.5 & Ocean prox. = {Inland}", "Latitude <= 33.5 & Ocean prox. = {<1H Ocean, Near bay, Near ocean, Island}", "33.5 < Latitude <= 34.5 & Ocean prox. = {<1H Ocean, Near bay, Near ocean, Island}")) 


# save Figure
if (!dir.exists("figures")) dir.create("figures", recursive = TRUE)
ggpubr::ggarrange(p1,p2, common.legend = TRUE, legend = "bottom") %>%
  ggpubr::ggexport(filename = "figures/california_housing_longitude.pdf", width = 7, height = 3.8)
