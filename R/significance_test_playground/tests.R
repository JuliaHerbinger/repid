library(iml)
library(ranger)
library(data.table)
library(tidyverse)
library(customtrees)

# Simulate Data
set.seed(1)
n = 2500
x1 = round(runif(n, -1, 1), 1)
x2 = round(runif(n, -1, 1), 3)
x3 = sample(c(0, 1), size = n, replace = TRUE, prob = c(0.5, 0.5))
x4 = sample(c(0, 1), size = n, replace = TRUE, prob = c(0.7, 0.3))

# noisy vars
x5 = sample(c(0, 1), size = n, replace = TRUE, prob = c(0.5, 0.5))
x6 = rnorm(n, mean = 0, sd = 1)

#y = 0.2*x1 - 8*x2 + ifelse(x1 < quantile(x1, 0.25), 8*x2, ifelse(x1 > quantile(x1, 0.75), 16*x2, 0))
y = 0.2*x1 - 8*x2 + x6 + 8*x1*x2 + 0.01*x1*x6
eps = rnorm(n, 0, 0.1*sd(y))
y = y + eps

dat = data.frame(x1, x2, x3, x4, x5, x6, y)
X = dat[, setdiff(colnames(dat), "y")]

# Fit model and compute ICE for x2
library(MASS)
library(mgcv)
#mod = stepAIC(lm(y~.+.:x6,data=dat))

mod = gam(y~s(x1,x2,x6)+s(x1,x2)+
    s(x1)+s(x2)+s(x6)+x3+x4+x5,data=dat, method="REML")

#mod = ranger(y ~ ., data = dat, num.trees = 500)
#pred = function(model, newdata) predict(model, newdata)$predictions
pred = function(model, newdata) predict.gam(model, newdata)
model = Predictor$new(mod, data = X, y = dat$y, predict.function = pred)
effect = FeatureEffect$new(model, method = "ice", grid.size = 20, feature = "x6")
eff = as.data.table(effect$results)

# Center ICE curves
eff = as.data.frame(eff[, .value := (.value - mean(.value)), by = c(".type", ".id")])

# Plot ICE curves: WE WANT TO FIND SUBGROUPS SUCH THAT ICE KURVES ARE HOMOGENOUS
ggplot(eff, aes(x = x6, y = .value)) + 
  geom_line(aes(group = .id))

# point-wise L2 distance
SS_L2 = function(y, x, requires.x = FALSE, ...) {
  ypred = colMeans(y)
  sum(t((t(y) - ypred)^2))
}

# Get ICE values and arrange them in a horizontal matrix
Y = spread(eff, x6, .value)
Y = Y[, setdiff(colnames(Y), c(".type", ".id"))]

str(X) # contains our feature values
str(Y) # contains ICE values for each grid point

# find best split
sp = split_parent_node(Y = Y, X = X, objective = SS_L2,
  n.splits = 1, optimizer = find_best_binary_split)
sp

# show optimal split
node_index = generate_node_index(Y, X, result = sp)
str(node_index)
plot.data = effect$results
plot.data$.split = node_index$class[plot.data$.id]
ggplot(plot.data, aes(x = x6, y = .value)) + 
  geom_line(aes(group = .id)) + facet_grid(~ .split)


########################
# Compute Significance
########################
obj.root = SS_L2(Y, X)
obj.root

# split data according to best split
feat = sp$feature[sp$best.split]
split.point = unlist(sp$split.points[sp$best.split])
#split.point = sample(X$x2, 1) # use a random point as split point

feat = "x1"
split.point = unlist(sp$split.points[sp$feature == feat])

idx = X[,feat] < split.point

X.left = X[idx, ]
X.right = X[!idx, ]
Y.left = Y[idx, ]
Y.right = Y[!idx, ]

obj.split = SS_L2(Y.left, X.left) + SS_L2(Y.right, X.right)

(t.stat = obj.split)

# compute null distribution of t.stat values after permutation without running optimizer again
null.no.optim = sapply(1:1000, function(i) {
  # permute ice curves to break association between feature of interest S and C
  ind = sample(1:nrow(Y))
  #Y = Y[ind,]
  X[, feat] = X[ind, feat]
  
  idx = X[,feat] < split.point
  
  X.left = X[idx, ]
  X.right = X[!idx, ]
  
  Y.left = Y[idx, ]
  Y.right = Y[!idx, ]
  
  obj.split = SS_L2(Y.left, X.left) + SS_L2(Y.right, X.right)
  return(obj.split)
})
plot(density(null.no.optim)) # null distribution has very small values
(p.value = mean(null.no.optim < t.stat)) # many points are often significant...


# compute null distribution of t.stat values after permutation and running optimizer
null.optim = sapply(1:100, function(i) {
  # permute ice curves to break association between feature of interest S and C
  ind = sample(1:nrow(Y))
  #Y = Y[ind,]
  X[, feat] = X[ind, feat]
  
  sp = split_parent_node(Y = Y, X = X[, feat, drop=FALSE], objective = SS_L2,
    n.splits = 1, optimizer = find_best_binary_split)
  #feat = sp$feature[sp$best.split]
  split.point = unlist(sp$split.points[sp$best.split])
  
  idx = X[,feat] < split.point
  X.left = X[idx, ]
  X.right = X[!idx, ]
  
  Y.left = Y[idx, ]
  Y.right = Y[!idx, ]
  
  obj.split = SS_L2(Y.left, X.left) + SS_L2(Y.right, X.right)
  return(obj.split)
})
plot(density(null.optim))
abline(v = t.stat)
(p.value = mean(null.optim < t.stat))





# compute null distribution of t.stat values after permutation without running optimizer again
null.random.sp = sapply(1:100, function(i) {
  # permute ice curves to break association between feature of interest S and C
  #ind = sample(1:nrow(Y))
  #Y = Y[ind,]
  #X = X[ind, ]
  
  idx = X[,feat] < sample(X[,feat], 1)
  
  X.left = X[idx, ]
  X.right = X[!idx, ]
  
  Y.left = Y[idx, ]
  Y.right = Y[!idx, ]
  
  obj.split = SS_L2(Y.left, X.left) + SS_L2(Y.right, X.right)
  return(1 - obj.split/obj.root)
})
plot(density(null.random.sp)) # null distribution has very small values
(p.value = mean(null.random.sp > t.stat)) # many points are often significant...



# 
# SS_L2_random = function(y, x, requires.x = FALSE, ...) {
#   ypred = colMeans(y)
#   y = y[, sample(1:ncol(y))]
#   sum(t((t(y) - ypred)^2))
# }
# # compute null distribution of t.stat values after permutation without running optimizer again
# null.random = sapply(1:100, function(i) {
#   # permute ice curves to break association between feature of interest S and C
#   #ind = sample(1:nrow(Y))
#   #Y = Y[ind,]
#   #X = X[ind, ]
#   
#   idx = X[,feat] < split.point
#   
#   X.left = X[idx, ]
#   X.right = X[!idx, ]
#   
#   Y.left = Y[idx, ]
#   Y.right = Y[!idx, ]
#   
#   
#   obj.split = SS_L2_random(Y.left, X.left) + SS_L2_random(Y.right, X.right)
#   return(1 - obj.split/obj.root)
# })
# plot(density(null.random)) # null distribution has very small values
# (p.value = mean(null.random > t.stat)) # many points are often significant...




# compute null distribution of t.stat values after permutation without running optimizer again
null.no.optim = sapply(1:100, function(i) {
  # permute ice curves to break association between feature of interest S and C
  ind = sample(1:nrow(Y))
  Y = Y[ind,]
  #X = X[ind, ]
  
  idx = X[,feat] < split.point
  
  X.left = X[idx, ]
  X.right = X[!idx, ]
  
  Y.left = Y[idx, ]
  Y.left = Y.left[sample(1:nrow(Y.left)), ]
  Y.right = Y[!idx, ]
  Y.right = Y.right[sample(1:nrow(Y.right)), ]
  
  obj.split = SS_L2(Y.left, X.left) + SS_L2(Y.right, X.right)
  return(1 - obj.split/obj.root)
})
plot(density(null.no.optim)) # null distribution has very small values
(p.value = mean(null.no.optim > t.stat)) # many points are often significant...






# # compute null distribution of t.stat values after permutation without running optimizer again
# null2 = sapply(1:100, function(i) {
#   # permute ice curves to break association between feature of interest S and C
#   #ind = sample(1:nrow(Y))
#   #Y = Y[ind,]
#   #X = X[ind, ]
#   
#   idx = sample(X[,feat] < split.point)
#   X.left = X[idx, ]
#   X.right = X[!idx, ]
#   
#   Y.left = Y[idx, ]
#   Y.right = Y[!idx, ]
#   
#   obj.split = SS_L2(Y.left, X.left) + SS_L2(Y.right, X.right)
#   return(1 - obj.split/obj.root)
# })
# plot(density(null2)) # null distribution has very small values
# (p.value = mean(null2 > t.stat)) # many points are often significant...

pdp.left = colMeans(Y.left)
pdp.right = colMeans(Y.right)

(t.stat = sum((pdp.left - pdp.right)^2))

# compute null distribution of t.stat values after permutation without running optimizer again
null.pdp.diff = sapply(1:1000, function(i) {
  # permute ice curves to break association between feature of interest S and C
  #ind = sample(1:nrow(Y))
  #Y = Y[ind,]
  #X = X[ind, ]
  
  idx = sample(X[,feat] < split.point)
  
  Y.left = Y[idx, ]
  Y.right = Y[!idx, ]
  
  pdp.left = colMeans(Y.left)
  pdp.right = colMeans(Y.right)
  
  pdp.diff = sum((pdp.left - pdp.right)^2)
  return(pdp.diff)
})
plot(density(null.pdp.diff)) # null distribution has very small values
(p.value = mean(null.pdp.diff > t.stat)) # many points are often significant...






