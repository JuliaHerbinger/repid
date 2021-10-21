 # needs to be extracted somehow of simulation setting
source("R/Simulation_Examples/helper_functions.R")
source("R/Simulation_Examples/dice_function.R")
load_all("customtrees")
source("customtrees/R/tree_splitting.R") # add in package
source("R/Simulation_Examples/quantitative_evaluation.R")
source("R/Simulation_Examples/simulation_setting_definition.R")

results.list = create_aggr_restults(n = 1000, n.sim = 1, feat = "x1", learner = "regr.ranger", n.split = 4, type = "categorical_linear", impr.par = 0.3, min.rel.imp = 0, min.hstat = 0.05 )
tab.ranks = as.data.frame(results.list$table.ranks.aggr)
tab.ranks = tab.ranks %>% tidyr::gather(value = "value", key = "method", -iter)


ggplot(tab.ranks) + geom_boxplot(aes(x = method, y = value))
df.aggr.feat = as.data.frame(results.list$df.aggr.feat)
ggplot(as.data.frame(results.list$df.aggr.feat), aes(x = as.character(split.feature), y = avg.rel.imp)) + geom_bar(stat="identity")




n = 500
set.seed(1)
x1 = runif(n, -1, 1)
x2 = runif(n, -1, 1)
x3 = runif(n, -1, 1)
x4 = runif(n, -1, 1)
x5 = runif(n, -1, 1)
x6 = rnorm(n, 0, 2)
x7 = rnorm(n, 2, 3)

true.ranks = c(0,3,5,7,0,0)
formula = x1 + 4*x2 + 3*x2*x3 + 5*x2*x4 + 7*x2*x5
eps = rnorm(n, 0, sd(formula)*0.1)
y =  formula + eps

data = data.frame(x1, x2, x3, x4, x5, x6, x7, y)

set.seed(1)
learner = "regr.cvglmnet"
task = mlr::makeRegrTask(data = data, target = "y")
lrn = mlr::makeLearner(learner)
mod = mlr::train(lrn, task)
X = data[, setdiff(colnames(data), "y")]
model = Predictor$new(mod, data = X, y = data$y)
effect = FeatureEffect$new(model, method = "ice", grid.size = 20, feature = "x2")
data.split = compute_data_for_ice_splitting(effect, X)
res.onesplit = split_parent_node(X = data.split$X, Y = data.split$Y, n.splits = 1, objective = SS_L2, optimizer = find_best_binary_split)
res.onesplit$impr = 1-res.onesplit$objective.value/SS_L2(data.split$Y, data.split$X)

library(iml)
int = Interaction$new(model, feature = "x2")






# alpha=1 means lasso regression. 
lasso <- glmnet(scale(varmtx), response, alpha=1)

# Cross validation to find the optimal lambda penalization
cv.lasso <- cv.glmnet(varmtx, response, alpha=1)


library(glmnet)
## Loading required package: Matrix
## Loaded glmnet 4.0-2
# Prepare glmnet input as matrix of predictors and response var as vector
varmtx <- model.matrix(y~.^2-1, data=data)
response <- data$y

plot(lasso, xvar = "lambda", label=T)
lbs_fun(ridge, offset_x = -2)
abline(v=cv.lasso$lambda.min, col = "red", lty=2)
abline(v=cv.lasso$lambda.1se, col="blue", lty=2)
coef(cv.lasso)
f <- as.formula(y~.^2-1, data)

pred = predict.function = function(model, newdata) predict(model, newdata)
model = Predictor$new(cv.lasso, data = X, predict.function = pred)
