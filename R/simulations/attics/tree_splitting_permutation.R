library(R6)
library(data.table)
library(BBmisc)

#' @title Performs a single tree based on ICE curves
#'
#' @description
#' Uses functions in customtrees.r for splitting effect curves according to defined objective.
#'
#' @param effect effect object of IML method FeatureEffect$new()
#' @param testdata dataset to use for splitting (data.frame with features in columns)
#' @param objective character string with objective function to use (so far: 'SS_L1', 'SS_L2', 'SS_area', 'SS_sd' and 'var_gp' are defined)
#' @param n.split number of splits to be performed


# Definition of Class Node
Node <- R6Class("Node", list(
  id = NULL,
  
  # on which depth is the node
  depth = NULL,
  
  # ids of the instances of data that are in this node
  subset.idx = NULL,
  objective.value = NULL, # objective value in a node
  objective.value.parent = NULL,
  
  # Parent information
  id.parent = NULL, 
  child.type = NULL, # left or right type
  
  # Split information (if splitting has already taken place)
  split.feature = NULL,
  split.value = NULL,
  
  # Append the children of this node
  children = list(),
  
  stop.criterion.met = FALSE,
  improvement.met = NULL,
  
  improvement = NULL,
  rsqrt = NULL,
  
  
  
  initialize = function(id, depth = NULL, subset.idx, id.parent = NULL, child.type = NULL, objective.value.parent = NULL, objective.value = NULL, improvement.met, rsqrt, improvement) {
    
    assert_numeric(id, len = 1)
    assert_numeric(depth, len = 1, null.ok = TRUE)
    
    assert_numeric(subset.idx, min.len = 1)
    assert_numeric(id.parent, len = 1, null.ok = TRUE)
    assert_character(child.type, null.ok = TRUE)
    
    self$id = id
    self$depth = depth
    self$subset.idx = subset.idx
    self$id.parent = id.parent
    self$child.type = child.type
    self$improvement = improvement
    self$rsqrt = rsqrt
    self$objective.value.parent = objective.value.parent
    self$objective.value = objective.value
    
    self$stop.criterion.met = FALSE
    self$improvement.met = improvement.met
  },
  
  computeSplit = function(X, Y, objective, impr.par, optimizer, n.perm, alpha, min.split = 10) {
    browser()
    
    if (length(self$subset.idx) < min.split | self$improvement.met == TRUE) {
      self$stop.criterion.met = TRUE
    } else {
      
      self$objective.value.parent = objective(y = Y, x = X)
      self$objective.value = objective(y = Y[self$subset.idx, ], x = X[self$subset.idx, ])
      
      
      
      
      tryCatch({
        split = split_parent_node(Y = Y[self$subset.idx, ], X = X[self$subset.idx, ], objective = objective, optimizer = find_best_binary_split, min.node.size = min.split)
        #idx.left = which(X[self$subset.idx, split$feature[split$best.split][1], with = FALSE] <= unlist(split$split.points[split$best.split])[1])
        #idx.right = which(X[self$subset.idx, split$feature[split$best.split][1], with = FALSE] > unlist(split$split.points[split$best.split])[1])
        n.left=(length(which(X[self$subset.idx, split$feature[split$best.split][1], with = FALSE] <= unlist(split$split.points[split$best.split])[1])))
        n.right = length(which(X[self$subset.idx, split$feature[split$best.split][1], with = FALSE] > unlist(split$split.points[split$best.split])[1]))
        
        idx.left = sample(1:length(self$subset.idx), n.left)
        idx.right = setdiff(1:length(self$subset.idx), idx.left)
        #n.perm = min(n.perm, (length(self$subset.idx)-1))
        #perm = permute(data.frame(id = self$subset.idx), n.perm)
        perm = permute(data.frame(id = self$subset.idx), n.perm)
        set.seed(123)
        loss.perm = lapply(perm$perm, function(x){
          idx.left = sample(1:length(self$subset.idx), n.left)
          idx.right = setdiff(1:length(self$subset.idx), idx.left)
          mean_difference = colMeans(Y[self$subset.idx[idx.left], ])-colMeans(Y[self$subset.idx[idx.right], ])
          standard_dev = abs(sqrt(1/length(self$subset.idx[idx.left])^2*objective(y = Y[self$subset.idx[idx.left], ], x = X[self$subset.idx[idx.left],]) + 1/length(self$subset.idx[idx.right])^2*objective(y = Y[self$subset.idx[idx.right], ], x = X[self$subset.idx[idx.right],])))
          Z = 1/sqrt(2*20)*sum((mean_difference/standard_dev)^2 - 1)
          Z
          #(length(self$subset.idx)-2)*(self$objective.value/20 - objective(y = Y[self$subset.idx[idx.left], ], x = X[self$subset.idx[idx.left],])/20  - objective(y = Y[self$subset.idx[idx.right], ], x = X[self$subset.idx[idx.right],])/20)/(objective(y = Y[self$subset.idx[idx.left], ], x = X[self$subset.idx[idx.left],])/20  + objective(y = Y[self$subset.idx[idx.right], ], x = X[self$subset.idx[idx.right],])/20)
          #self$objective.value - objective(y = Y[self$subset.idx[x$idx][idx.left], ], x = X[self$subset.idx[idx.left],])  - objective(y = Y[self$subset.idx[x$idx][idx.right], ], x = X[self$subset.idx[idx.right],])
          #split.perm = split_parent_node(Y = Y[self$subset.idx[x$idx], ], X = X[self$subset.idx, ], objective = objective, optimizer = find_best_binary_split, min.node.size = min.split)
          #self$objective.value - split.perm$objective.value[split$best.split][1] 
          #
        })
        loss.perm = do.call("c", loss.perm)
        loss.perm = sort(loss.perm)
        loss.perm.quantile = loss.perm[round((1-alpha)*n.perm, 0)]
        
        if(is.null(self$rsqrt)) {
          self$rsqrt = 0 
          self$improvement = 0
        }
        rsqrt = (self$objective.value.parent - split$objective.value[split$best.split][1]) / self$objective.value.parent
        
        # if(self$improvement == 0){
        #   if ( rsqrt - self$rsqrt < impr.par){
        #     self$improvement.met = TRUE
        #   }
        #   else{
        #     self$split.feature = split$feature[split$best.split][1]
        #     self$split.value = unlist(split$split.points[split$best.split])[1]
        #     self$improvement = rsqrt - self$rsqrt
        #     self$rsqrt = rsqrt
        #     self$objective.value.parent = objective(y = Y[self$subset.idx, ], x = X[self$subset.idx, ])
        #     self$objective.value = split$objective.value[split$best.split][1]
        #     
        #   }
        # }
        #else{
          # if ( rsqrt - self$rsqrt < min(self$improvement*impr.par, impr.par^2)){
          #   self$improvement.met = TRUE
          # }
        
        # if(self$improvement == 0){
        #   if ( rsqrt - self$rsqrt < impr.par){
        #     self$improvement.met = TRUE
        #   }
        #   else{
        #     self$split.feature = split$feature[split$best.split][1]
        #     self$split.value = unlist(split$split.points[split$best.split])[1]
        #     self$improvement = rsqrt - self$rsqrt
        #     self$rsqrt = rsqrt
        #     self$objective.value.parent = objective(y = Y[self$subset.idx, ], x = X[self$subset.idx, ])
        #     self$objective.value = split$objective.value[split$best.split][1]
        #     
        #   }
        # }
        #else{
        # if ( rsqrt - self$rsqrt < min(self$improvement*impr.par, impr.par^2)){
        #   self$improvement.met = TRUE
        # }
        
        improvement = self$objective.value - split$objective.value[split$best.split][1]
        if ( improvement - self$improvement*impr.par*self$objective.value.parent < loss.perm.quantile){
          self$improvement.met = TRUE
        }
          else{
            self$split.feature = split$feature[split$best.split][1]
            self$split.value = unlist(split$split.points[split$best.split])[1]
            self$improvement = rsqrt - self$rsqrt
            self$rsqrt = rsqrt
            self$objective.value.parent = objective(y = Y[self$subset.idx, ], x = X[self$subset.idx, ])
            self$objective.value = split$objective.value[split$best.split][1]
            
          }
        #}
        
        
      }, 
      error = function(cond) {
        # message(paste0("Min.node.size is reached in node ", self$id))
        self$stop.criterion.met = TRUE
      })
      
      
    }
  },
  
  computeChildren = function(X, Y, objective) {
    #browser()
    if (self$stop.criterion.met|self$improvement.met) {
      # no further split is performed
      self$children = list("left.child" = NULL, "right.child" = NULL)
    } else {
      if(is.null(self$split.feature))
        stop("Please compute the split first via computeSplit().")
      
      
      idx.left = which(X[self$subset.idx, self$split.feature, with = FALSE] <= self$split.value)
      idx.right = which(X[self$subset.idx, self$split.feature, with = FALSE] > self$split.value)
      
      idx.left = self$subset.idx[idx.left]
      if(length(idx.left)==0) idx.left = 0
      idx.right = self$subset.idx[idx.right]
      if(length(idx.right)==0) idx.right = 0
      
      obj.left = objective(y = Y[idx.left, ], x = X[idx.left, ])
      obj.right = objective(y = Y[idx.right, ], x = X[idx.right, ])
      obj.parent = objective(y = Y[self$subset.idx, ], x = X[self$subset.idx, ])
      
      left.child = Node$new(id = 1, depth = self$depth + 1, subset.idx = idx.left, id.parent = self$id, child.type = "<=",  improvement.met = self$improvement.met, rsqrt = self$rsqrt, improvement = self$improvement)
      right.child = Node$new(id = 2, depth = self$depth + 1, subset.idx = idx.right, id.parent = self$id, child.type = ">",  improvement.met = self$improvement.met, rsqrt = self$rsqrt, improvement = self$improvement)
      
      self$children = list("left.child" = left.child, "right.child" = right.child)
    }
  }
)
)



# compute single tree based on Class 'Node' 
compute_tree = function(effect, testdata, n.perm, objective = "SS_L2", n.split, alpha = 0.05, impr.par = 0.01) {
  #browser()
  if (objective == "SS_L1") {
    
    split.objective = function(y, x, requires.x = FALSE, ...) {
      require(Rfast)
      ypred = colMeans(as.matrix(y))
      min(t((t(y) - ypred)^2))    
    } 
    
    input.data = compute_data_for_ice_splitting(effect, testdata = testdata)
  } 
  
  else if (objective == "SS_L2") {
    
    split.objective = function(y, x, requires.x = FALSE, ...) {
      ypred = colMeans(as.matrix(y))
      sum(t((t(y) - ypred)^2))
    } 
    
    input.data = compute_data_for_ice_splitting(effect, testdata = testdata)
  }
  
  else if (objective == "SS_area") {
    
    split.objective = function(y, x, requires.x = FALSE, ...) {
      row_means = rowMeans(y) # area of individual ice curves
      ypred = mean(row_means) # area of pdp
      sum((row_means - ypred)^2)
    } 
    
    input.data = compute_data_for_ice_splitting(effect, testdata = testdata)
    
  }
  
  
  else if (objective == "SS_sd") {
    
    pdp.feat = effect$feature.name
    split.feats = setdiff(names(testdata), pdp.feat)
    
    # The ys are the predictions (in this case, the standard deviation)
    X = setDT(testdata)
    Y = setDT(effect$predictor$predict(X))
    
    split.objective = function(y, x, requires.x = FALSE, ...) {
      y = y$pred
      sum((y - mean(y))^2)
    } 
    
    split.feats = setdiff(names(testdata), pdp.feat)
    input.data = list(X = X[, ..split.feats, drop = FALSE], Y = Y)
  }
  
  else {
    stop(paste("Objective", objective, "is not supported."))
  } 
  
  # calculate permutations
  if(is.null(n.perm)) n.perm = length(nrow(input.data$X)-1)
  
  
  
  # Initialize the parent node of the tree
  parent = Node$new(id = 0, depth = 1, subset.idx = seq_len(nrow(input.data$X)), improvement.met = FALSE, rsqrt = 0, improvement = 0)
  
  # Perform splitting for the parent
  tree = list(list(parent))
  #browser()
  for (depth in seq_len(n.split)) {
    
    leaves = tree[[depth]]
    
    tree[[depth + 1]] = list()
    
    for (node.idx in seq_along(leaves)) {
      
      node.to.split = leaves[[node.idx]]
      
      if (!is.null(node.to.split)) {
        node.to.split$computeSplit(X = input.data$X, Y = input.data$Y, objective = split.objective, impr.par = impr.par, optimizer = find_best_binary_split, n.perm = n.perm,alpha = alpha, min.split = 10)
        
        node.to.split$computeChildren(input.data$X, input.data$Y, split.objective)
        
        
        tree[[depth + 1]] = c(tree[[depth + 1]], node.to.split$children)        
      } else {
        tree[[depth + 1]] = c(tree[[depth + 1]], list(NULL,NULL))                
      }
    }
  }
  
  return(tree)
}




compute_data_for_ice_splitting = function(effect, testdata) {
  
  # effect: effect object of IML method FeatureEffect
  
  # Output: A data.frame where each row corresponds to a ice curve 
  #print(testdata)
  df = setDT(testdata)
  df$.id = seq_row(df)
  
  ice.feat = effect$feature.name
  features = names(testdata)
  
  # Features we consider splitting 
  split.feats = setdiff(features, ice.feat)
  df.sub = df[, c(".id", split.feats), with = FALSE]  
  
  effectdata = effect$results
  effectdata = effectdata[effectdata$.type=="ice",]
  
  Y = tidyr::spread(effectdata, ice.feat, .value)
  #Y = setDT(Y)[, setdiff(colnames(Y), c(".type", ".id")), with = FALSE]
  Y = Y[, setdiff(colnames(Y), c(".type", ".id"))]
  # center ICE curves by their mean
  #browser()
  #Y = apply(Y, 1, function(y) y - mean(y))
  Y = Y - rowMeans(Y)
  Y = setDT(Y)
  # for(i in 1:nrow(Y)){
  #   Y[i,] = as.numeric(unname(Y[i,])) - mean(as.numeric(unname(Y[i,])))
  # }
  
  X = df[, split.feats, with = FALSE]
  
  return(list(X = X, Y = Y))
}
