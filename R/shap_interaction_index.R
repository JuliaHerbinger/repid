# SHAP INTERACTION INDEX FUNCTIONS


calculateValueFunctionPrediction = function(features, object, x.interest, data, target = NULL,
                                             repl.vec, measures = NULL, predict.fun = NULL, local = FALSE) {
  assertCharacter(features)
  assertString(target)
  
  all.feats = setdiff(colnames(data), target)
  
  # shuffle all features except the ones for which we want to compute the value function
  shuffle.features = setdiff(all.feats, features)
  # compute the value function
  ret = lapply(repl.vec, function(i) {
    z.interest = data[i, shuffle.features]
    x.interest.shuffled = x.interest
    x.interest.shuffled[, shuffle.features] = z.interest
    if (is.null(predict.fun)) p = predict(object, newdata = x.interest.shuffled)$data$response
    else p = predict.fun(object, newdata = x.interest.shuffled)
    as.data.frame(cbind(features = stri_paste(features, collapse = ","), pred = p))
  })
  ret = rbindlist(ret)
  ret$pred = as.numeric(ret$pred)
  
  
  return(ret[, lapply(.SD, mean), by = "features"])
}


getMarginalContributionValues = function(mc, vf, with.f) {
  f = vf$features
  vf = vf[, -"features"]
  mc.vals = lapply(mc, function(m) {
    # make string to match with f
    if(with.f==TRUE) with.f = stri_paste(m$with.f, collapse = ",")
    else with.f = stri_paste(m$without.f, collapse = ",")
    
    #without.f = stri_paste(m$without.f, collapse = ",")
    # value function with feature f
    v.with.f = vf[charmatch(with.f, f), ] #vf[f %in% with.f,]
    # value function without feature f
    #v.without.f = vf[charmatch(without.f, f), ] #vf[f %in% without.f,]
    list(
      with.f = with.f,
      #without.f = without.f,
      v.with.f = v.with.f#
      #v.without.f = v.without.f
    )
  })
  
  # extract string
  with.f = vcapply(mc.vals, function(x) x$with.f)
 # without.f = vcapply(mc.vals, function(x) x$without.f)
  
  # extract value functions
  v.with.f = rbindlist(lapply(mc.vals, function(x) x$v.with.f))
  #v.without.f = rbindlist(lapply(mc.vals, function(x) x$v.without.f))
  
  # compute marginal contribution value which is the difference of value functions:
  ret = v.with.f #- v.without.f
  return(data.table(features.with.f = with.f,  ret))
}



 
  
shap.interaction = function(data, object, feat, feat.interact, target, n.repl, predict.function){
  
  features = setdiff(colnames(data), target)
  pos.feat.interact = which(features == feat.interact)
  
  # generate permutations with all features - interacting features (not in included in S)
  perm = featureImportance:::generatePermutations(features[-pos.feat.interact], n.shapley.perm = 120)
  
  # generate marginal contribution for feature of interest
  mc.feat = featureImportance:::generateMarginalContribution(feat, perm)
  
  # generate marginal contribution for interacting feature
  mc.feat.interact = lapply(mc.feat, function(x) lapply(x, function(y) gsub(feat, feat.interact, y)))
  
  # generate marginal contribution for both, feature of interest and interacting feature
  mc.both = lapply(mc.feat, function(x) lapply(x, function(y) {
    if(feat %in% y) append(y, c(feat.interact), which(y==c(feat)))
    else y
    
  }))
  
  # create list for all marginal contributions to be calculated
  mc = append(append(mc.feat, mc.feat.interact), mc.both)
  
  # get unique elements 
  values = unique(unname(unlist(mc, recursive = FALSE)))
  
  # loop over all samples to calculate interaction value for all samples
  for(i in 1:nrow(data)){
    
    length.data = setdiff(1:nrow(data), i)
    repl.vec = sample(length.data, n.repl) # create random samples for replacements
  
    # loop over all all feature combinations to create value function
    value.function = lapply(values, function(f) {
      calculateValueFunctionPrediction(object = object, data = data, x.interest = data[i,features], target = "y", measures = getDefaultMeasure(task),
                                                         repl.vec = repl.vec, features = f, predict.fun = predict.function)
    })
  
  vf = rbindlist(value.function)
  vf$features = stri_paste_list(values, ",")
  
  # calculate predictions for S + feat, S + feat.interact, S + feat + feat.interact and only S
  mc.val.feat = getMarginalContributionValues(mc.feat, vf, with.f = TRUE)
  mc.val.feat.interact = getMarginalContributionValues(mc.feat.interact, vf, with.f = TRUE)
  mc.val.both = getMarginalContributionValues(mc.both, vf, with.f = TRUE)
  mc.val.S = getMarginalContributionValues(mc.feat, vf, with.f = FALSE)
  
  # interaction strength = v_both - v_feat - v_feat.interact + v_S
  interaction.strength = sum(mc.val.both$pred - mc.val.feat$pred - mc.val.feat.interact$pred + mc.val.S$pred)
  
  if(!exists("res")) res = data.frame("feat.val" = data[i, feat],"feat.interact.val" = data[i, feat.interact], "interaction" = interaction.strength)
  else res = rbind(res, cbind("feat.val" = data[i, feat],"feat.interact.val" = data[i, feat.interact], "interaction" = interaction.strength))
  
  }
  return(res)
}

shap.interaction.global = function(data, object, feat, target, n.sample, n.repl, predict.function){
  features = setdiff(colnames(data), target)
  features.interaction = setdiff(features, feat)
  sampled.data = data[sample(1:nrow(data), n.sample),]
  shap.global = lapply(features.interaction, function(feat.interact){
    shap = shap.interaction(sampled.data, object = object, feat = feat, feat.interact = feat.interact, target = target, n.repl = n.repl, predict.function = predict.function)
    shap = data.frame(feature = feat, feat.interact = feat.interact, shap.interaction = sum(abs(shap$interaction)))
  })
  shap.global = do.call("rbind", shap.global)
  shap.global$shap.interaction.rel = shap.global$shap.interaction/sum(abs(shap.global$shap.interaction))
  return(shap.global)
}

