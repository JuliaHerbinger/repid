

create_sim_data = function(job, n, type, ...){
  
  if(type == "numeric_linear"){
    # Simulate Data
    #set.seed(seed)
    x1 = runif(n, -1, 1)
    x2 = runif(n, -1, 1)
    x3 = runif(n, -1, 1)
    x4 = runif(n, -1, 1)
    x5 = runif(n, -1, 1)
    x6 = rnorm(n, 0, 2)
    x7 = rnorm(n, 2, 3)
    
    formula = x1 + 4*x2 + 3*x2*x3 + 5*x2*x4 + 7*x2*x5
    eps = rnorm(n, 0, sd(formula)*0.1)
    y =  formula + eps
    
    data = data.frame(x1, x2, x3, x4, x5, x6, x7, y)
    
  }
  
  if(type == "numeric_linear_large"){
    # Simulate Data
    #set.seed(seed)
    for(i in 1:20){
      x = runif(n, -1, 1)
      assign(paste0("x",i), x)
    }
    
    
    formula = 4*x2 + 4*x4 + 4*x6 + 4*x8 + 4*x10 + 4*x12 + 3*x2*x1 + 3.5*x2*x3 + 4*x2*x4 + 6*x2*x5 + 8*x2*x6 + 8.5*x2*x7 + 3*x1*x3 + 3*x8*x10 + 3*x7*x9
    eps = rnorm(n, 0, sd(formula)*0.1)
    y =  formula + eps
    
    data = data.frame(mget(paste0("x",1:20)), y)
    
  }
  
  if(type == "linear_mixed"){
    # Simulate Data
    #set.seed(seed)
    x1 = runif(n, -1, 1)
    x2 = runif(n, -1, 1)
    x3 = sample(c(0, 1), size = n, replace = TRUE, prob = c(0.5, 0.5))
    x4 = sample(c(0, 1), size = n, replace = TRUE, prob = c(0.7, 0.3))
    x5 = sample(c(0, 1), size = n, replace = TRUE, prob = c(0.5, 0.5))
    for(i in 6:20){
      x = runif(n, -1, 1)
      assign(paste0("x",i), x)
    }
    
    
    formula = 4*x2 + 2*x4 + 2*x6 + 2*x8 + 4*x2*x1 + ifelse(x3 == 0, I(8*x2),0) + 
      ifelse(x5 == 1, I(10*x2),0)*x6 + 8*x2*x7 + 3*x1*x3 + 3*x8*x10 + 3*x7*x9
    eps = rnorm(n, 0, sd(formula)*0.1)
    y =  formula + eps
    
    data = data.frame(mget(paste0("x",1:20)), y)
    
  }
  
  if(type == "categorical_linear"){
    # Simulate Data
    #set.seed(seed)
    x1 = runif(n, -1, 1)
    x2 = runif(n, -1, 1)
    x3 = sample(c(0, 1), size = n, replace = TRUE, prob = c(0.5, 0.5))
    x4 = sample(c(0, 1), size = n, replace = TRUE, prob = c(0.7, 0.3))
    x5 = sample(c(0, 1), size = n, replace = TRUE, prob = c(0.5, 0.5))
    x6 = rnorm(n, mean = 1, sd = 5)
    
    formula = 0.2*x1 - 8*x2 + ifelse(x3 == 0, I(16*x2),0) + ifelse(x1 > mean(x1), I(8*x2),0) 
    eps = rnorm(n, 0, sd(formula)*0.1)
    y =  formula + eps
    
    data = data.frame(x1, x2, x3, x4, x5, x6, y)
  }
  
  if(type == "mixed_large"){
    # Simulate Data
    #set.seed(seed)
    # numeric
    for(i in 1:10){
      x = runif(n, -1, 1)
      assign(paste0("x",i), x)
    }
    # binary
    for(i in 11:15){
      x = sample(c(0, 1), size = n, replace = TRUE, prob = c(0.5, 0.5))
      assign(paste0("x",i), x)
    }
    # multiple categories
    x16 = sample(c(1:3), size = n, replace = TRUE, prob = c(0.2, 0.4, 0.3))
    x17 = sample(c(1:4), size = n, replace = TRUE, prob = c(0.2, 0.2, 0.3, 0.3))
    x18 = sample(c(1:4), size = n, replace = TRUE, prob = c(0.1, 0.1, 0.1, 0.7))
    x19 = sample(c(1:8), size = n, replace = TRUE, prob = c(rep(0.1,6), 0.2, 0.2))
    x20 = sample(c(1:16), size = n, replace = TRUE, prob = seq(0,1, length.out = 16))
    

    formula = 4*x2 - 4*x4 + 4*x6 - 4*x8 + 4*x10 + 3*x2*x1 + 5*x2*x5 + 7*x2*x8 + ifelse(x13 == 0, I(8*x2),0) + 
      ifelse(x16 == 1, I(4*x2),0) + ifelse(x16 == 2, I(6*x2),0) + ifelse(x19 %in% c(1,2,3), I(4*x2),0) + ifelse(x19 %in% c(4:6), I(6*x2),0) + 
      3*x1*x3 + 3*x8*x10 + 3*x7*x9 + ifelse(x20 == 16, I(4*x5),0) + ifelse(x20 %in% c(1,3,5,7), I(6*x5),0)
    eps = rnorm(n, 0, sd(formula)*0.1)
    y =  formula + eps
    
    data = data.frame(mget(paste0("x",1:20)), y)
    
  }
  
  if(type == "nonlinear"){ # adjusted Wells fargo example
    # Simulate Data
    #set.seed(seed)
    # numeric
    for(i in 1:10){
      x = runif(n, -1, 1)
      assign(paste0("x",i), x)
    }
    # binary
    # for(i in 16:20){
    #   x = sample(c(0, 1), size = n, replace = TRUE, prob = c(0.5, 0.5))
    #   assign(paste0("x",i), x)
    # }
   

    # formula = 4*cos(x2) - 4*x4 - 5*sin(x6) + 3*sin(3*x1*x2) + 10*x2*x3*x7 + 10*x2*(x4)^2 + 3*x2*exp(2*x5) + 2*abs(x2)^(x6) + ifelse(x17 == 0, I(5*x2),I(10*x2^2)) + 4*abs(x2*x8) +
    #   ifelse(x19 == 1, I(16*x2^3),0) + 1/(x2+x9) + 
    #   3*x1*x3 + ifelse(x10 == 1, I(4*x5),0)
    formula = 3*x1 + x2^2 - (pi)^(x3) + exp(-2*(x4)^2) + 1/(2+abs(x5)) + x6*log(abs(x6)) + 
      2*ifelse(x1 > 0, 1,0)*ifelse(x2 > 0, 1,0)*x3 + 2*ifelse(x4 > 0, 1,0)*x2 + 4*(x2*ifelse(x2 > 0, 1,0))^(abs(x6)) + abs(x2 + x8)
    eps = rnorm(n, 0, 0.5)
    y =  formula + eps
    
    data = data.frame(mget(paste0("x",1:10)), y)
    
  }
  
  if(type == "friedman"){
    # Simulate Data
    #set.seed(seed)
    # numeric
    for(i in 1:10){
      x = runif(n, 0, 1)
      assign(paste0("x",i), x)
    }
    

    formula = 10*sin(pi*x1*x2) + 20*(x3-0.5)^2 + 10*x4 + 5*x5
    eps = rnorm(n, 0, 1)
    y =  formula + eps
    
    data = data.frame(mget(paste0("x",1:10)), y)
    
  }
  
  #data = instance
  X = data[, setdiff(colnames(data), "y")]
  testdata = create_sim_data(job, n = 100000, type = job$prob.pars$type, ...)
  
  if(learner == "lm"){
    
    mod =lm(formula = y ~ .^2, data = data)
    predict.function = function(model, newdata) predict.lm(model, newdata)
    model = Predictor$new(mod, data = X, predict.function = predict.function)
    pred = predict.function(mod, testdata)
    perf.test = measureMSE(testdata$y, pred)
    
  }
  
  else if(learner == "gam"){
    if(job$prob.pars$type == "friedman"){
      mod = gam(y~s(x1,x2)+s(x1)+s(x2)+s(x3)+s(x4)+s(x5)+s(x6)+s(x7)+s(x8)+s(x9)+s(x10),data=data, method="REML")
      predict.function = function(model, newdata) predict.gam(model, newdata)
      model = Predictor$new(mod, data = X, predict.function = predict.function)
      #perf.train = measureMSE(data$y, mod$fitted.values)
    }
    if(job$prob.pars$type == "nonlinear"){
      mod = gam(y~s(x1,x2,x3)+s(x4,x2)+s(x6,x2)+s(x8,x2)+
                  s(x1)+s(x2)+s(x3)+s(x4)+s(x5)+s(x6)+s(x7)+s(x8)+s(x9)+s(x10),data=data, method="REML")
      predict.function = function(model, newdata) predict.gam(model, newdata)
      model = Predictor$new(mod, data = X, predict.function = predict.function)
      #perf.train = measureMSE(data$y, mod$fitted.values)
    }
    pred = predict.function(mod, testdata)
    perf.test = measureMSE(testdata$y, pred)
    
  }
  
  else{
    
    task = mlr::makeRegrTask(data = data, target = "y")
    lrn = mlr::makeLearner(learner)
    #rdesc = makeResampleDesc("CV", iters = 5, predict = "both")
    #r = resample(lrn, task, rdesc)
    mod = mlr::train(lrn, task)
    model = Predictor$new(mod, data = X, y = data$y)
    
    # performance on training data - sanity check how good ML model adjusts to underlying function
    pred = predict(object = mod, newdata = testdata)
    perf.test = measureMSE(pred$data$truth, pred$data$response)
  }
  
  return(list(mod = mod, model = model, perf.test = perf.test))
}
