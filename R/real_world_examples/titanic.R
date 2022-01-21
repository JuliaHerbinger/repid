

source("R/load_packages.R")
source("R/tree_splitting.R")
source("R/helper_general.R")

# load prepared dataset and convert features
train1 <- read_csv("data/titanic/titanic.csv")[,-1]
train1$Sex = as.factor(train1$Sex) 
train1$Sex = as.numeric(train1$Sex)
train1$Title = factor(train1$Title, levels = c( "Miss" ,"Mrs",  "Rare Title", "Mr","Master" ))
train1$Title = as.numeric(train1$Title)
train1$FsizeD = factor(train1$FsizeD, levels = c("singleton","small","large"))
train1$FsizeD = as.numeric(train1$FsizeD)
train1$Embarked = as.numeric(as.factor(train1$Embarked))
train1$Survived = as.factor(train1$Survived)


# Train a random forest
set.seed(123)
task = makeClassifTask(id = "titanic", data = train1, target = "Survived")
lrn = makeLearner("classif.ranger", predict.type = "prob")
mlr::resample(lrn, task, cv5, measures = bac)
rf = mlr::train(task = task, learner = lrn)

# calculate ice curves for age
mod <- Predictor$new(rf, data = train1, type = "prob", class = 2)
eff <- FeatureEffect$new(mod, feature = "Age", grid.size = 20, method = "pdp+ice")

# compute tree and extract split criteria
X = train1[,-which(colnames(train1)=="Survived")]
tree = compute_tree(eff, X, n.split = 3, impr.par = 0.2, min.split = 30)
res = extract_split_criteria(tree)

# define regional pdps
pdps = lapply(tree, function(depth) {
  pdp = lapply(1:length(depth), function(node){
    if(is.null(depth[[node]]$split.feature) & !is.null(depth[[node]]$subset.idx)){
      pdp = cbind(aggregate(formula = .value~Age, data = eff$results[eff$results$.id %in% depth[[node]]$subset.idx,], FUN = mean), depth = depth[[node]]$depth, id = node) 
     pdp
    }
  })
  do.call("rbind", pdp)
})

pdps = list.clean(pdps)
pdps = do.call("rbind",pdps)
pdps$group = paste0(pdps$depth, pdps$id)


# create plots for Figure 6
p1 = ggplot(eff$results[is.na(eff$results$.id),], aes(x = Age, y = .value)) + 
  geom_line(data = eff$results[!is.na(eff$results$.id),], aes(group = .id), alpha = 0.3) + geom_line(lwd = 2, col = "blue") + theme_bw() + ylim(0,1) +
  ylab("Predicted survival probability") + ggtitle("Global PD Plot")

p2 = ggplot(pdps, aes(x = Age, y = .value, group = group)) + geom_line(aes(col = group), lwd = 1.5) + theme_bw()+ ylim(0,1) +
  ylab("") + 
  theme(legend.position="bottom", legend.text=element_text(size=9)) +
  scale_color_manual(values = c(brewer.pal(9,"Greens")[c(4)],brewer.pal(9,"Oranges")[c(3)],brewer.pal(9,"Greens")[c(7,8)],brewer.pal(9,"Oranges")[c(5,7)]),name = "", 
                     labels = c("Sex = F & Fare > 26", "Sex = M & Pclass = 1", "Sex = F & Fare < 26 & Pclass = {1,2}", "Sex = F & Fare < 26 & Pclass = 3", "Sex = M & Pclass = {2,3} & Embarked = C", "Sex = M & Pclass = {2,3} & Embarked = {Q, S}")) + ggtitle("Regional PD Plots")


# save Figure
if (!dir.exists("figures")) dir.create("figures", recursive = TRUE)
ggpubr::ggarrange(p1,p2, common.legend = TRUE, legend = "bottom") %>%
  ggpubr::ggexport(filename = "figures/titanic_age.pdf", width = 7, height = 3.8)

# Regions:
# 32: Sex = Female & Fare > 26
# 41: Sex = Female & Fare < 26 & passenger class = {1,2} 
# 42: Sex = Female & Fare < 26 & passenger class = 3
# 33: Sex = Male   & passenger class = 1
# 47: Sex = Male   & passenger class = {2,3} & Embarked = Cherbourg
# 48: Sex = Male   & passenger class = {2,3} & Embarked = {Queenstown; S = Southampton}


