bike = readRDS("bike.Rds")

library(effects)
mod = lm(cnt ~ temp + hum + windspeed + season + yr + dayofyear, data = bike)
# Marginal effects for lm
plot(allEffects(mod))
mod2 = lm(cnt ~ temp + hum + windspeed + season + yr + dayofyear + temp*season, data = bike)
# Marginal effects for lm with interaction between categorical and numeric feature
plot(allEffects(mod2))
mod3 = lm(cnt ~ temp + hum + windspeed + season + yr + dayofyear + temp*season + dayofyear*hum, 
  data = bike)
# Marginal effects for lm with interaction between categorical and numeric and 2 numeric features
plot(allEffects(mod3))
# For LMs we have to explicitely include interactions to visualize their effect. 
# For ML algorithms, we don't as they usually automatically recognize interactions.
# AIM: We want to produce similar visualizations as for LMs with interactions 
# but for arbitrary ML models. 
# Ideally, we are looking for an automatic procedure that finds interesting 
# interactions in a data-driven way (e.g. using the information from ICE curves).



library(iml)
library(ranger)
rf = ranger(cnt ~ temp + hum + windspeed + season + yr + dayofyear, data = bike)
mod = Predictor$new(rf, data = bike, 
  predict.function = function(model, newdata) predict(model, newdata)$predictions)
feature = c("temp", "hum", "windspeed", "season", "yr", "dayofyear")

# For ML models, we can visualize main effects but we do not know which 
# interactions were found and which ones are interesting to look at
eff = FeatureEffects$new(mod, feature = feature, method = "pdp+ice", grid.size = 20)
eff$plot()

# We could visualize interactions in a 2-dimensional PDP plot:
# 1. interaction between numeric and categorical features
eff2 = FeatureEffect$new(mod, feature = c("temp", "season"), method = "pdp", grid.size = 20)
eff$plot() + eff2$plot()
# 2. interaction between numeric and categorical and 2 numeric features
eff3 = FeatureEffect$new(mod, feature = c("hum", "dayofyear"), method = "pdp", grid.size = 20)
eff$plot() + eff2$plot() + eff3$plot()
# But 2d PDPs are often computationally expensive
# Instead, we use the information from ICE curves to find interactions and
# produce visualizations for the most interesting interactions as follows:
# 1. Compute ICE curves for feature of interest
# 2. Check if the ICE curves can be splitted by another feature (-> potential interaction)
# 3. The feature that most likely interacts with our feature of interest has lowest objective.
# 4. We can even get a ranking (sorted according to the objective) of which other feature might also interact with our feature of interest.
