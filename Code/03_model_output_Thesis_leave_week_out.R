# !/usr/bin/env Rscript

yyear = as.numeric(commandArgs(trailingOnly=TRUE)[1])
wweek = as.numeric(commandArgs(trailingOnly=TRUE)[2])
print(c(yyear, wweek))
inla.setOption(num.threads = "4:1")


# Script to run INLA models in cross validation prediction mode

# Step 0: load packages pre-processed data and functions
# Step 1: rerun the selected model (fitted with config = TRUE for sampling) 
# Step 2: produce cross-validated posterior predictive samples leaving out one year and one month at a time

# Step 0: load packages and pre-processed data 
#source("00_load_packages_data.R")

# Step 1: rerun the selected model (fitted with config = TRUE for sampling) 

# fit full final model to use with control.mode = list(result = model1, restart = TRUE)
formula <- Y ~ 1 + 
  f(T1, replicate = S2, model = "rw1", scale.model = TRUE, cyclic = TRUE, 
    constr = TRUE, hyper = precision.prior) +
  f(S1, model = "bym2", replicate = T2, graph = "C:\\Users\\u271201\\Downloads\\Data\\Sample test\\map.graph", 
    scale.model = TRUE, hyper = precision.prior) +
  basis_MaxTm + basis_PM25 + basis_PM25_pop_density3 + Vw


if (TRUE) {
    model1 <- inla(formula, data = df, family = "nbinomial", offset = log(E), 
                   control.inla = list(strategy = 'adaptive'), 
                   control.compute = list(dic = TRUE, config = TRUE, 
                                          cpo = TRUE, return.marginals = FALSE),
                   control.fixed = list(correlation.matrix = TRUE, 
                                        prec.intercept = 1, prec = 1),
                   control.predictor = list(link = 1, compute = TRUE), 
                   verbose = FALSE)
model1 <- inla.rerun(model1)
save(model1, file = "C:\\Users\\u271201\\Downloads\\output\\preds\\model1.3_PM25_interaction.RData")
} else {
  load(file = "C:\\Users\\u271201\\Downloads\\output\\preds\\model1.3_PM25_interaction.RData")
  model1$misc$configs <- NULL
  gc()
}


# Step 2: produce cross-validated posterior predictive samples leaving out one year and one month at a time
# define number of samples
s <- 1000
for(yyear in 1:7) {
  for(wweek in 1:52) {
if (TRUE) {
    # replace dengue data in testing period with NA for out of sample prediction
    casestopred <- data$Mort_Male # response variable
    idx.pred <- which(data$year_index == yyear & data$week == wweek)
    casestopred[idx.pred] <- NA # replace cases in year and month of interest to NA
    mpred <- length(idx.pred)
    
    # set response variable and year indicator
    df$Y <- casestopred
    
    # final model
    formula <- Y ~ 1 + 
        f(T1, replicate = S2, model = "rw1", cyclic = TRUE, constr = TRUE, 
          scale.model = TRUE, hyper = precision.prior) +
        f(S1, model = "bym2", replicate = T2, graph = "C:\\Users\\u271201\\Downloads\\Data\\Sample test\\map.graph", 
          scale.model = TRUE, hyper = precision.prior) +
     basis_MaxTm + basis_PM25 + basis_PM25_pop_density3 + Vw
    
    if (TRUE) {
        model <- inla(formula, data = df, family = "nbinomial", offset = log(E), 
                      control.inla = list(strategy = 'adaptive'), 
                      control.compute = list(dic = TRUE, config = TRUE, 
                                             cpo = TRUE, return.marginals = FALSE),
                      control.fixed = list(correlation.matrix = TRUE, 
                                           prec.intercept = 1, prec = 1),
                      control.predictor = list(link = 1, compute = TRUE), 
                      control.mode = list(result = model1, restart = TRUE),
                      verbose = TRUE)
        model <- inla.rerun(model)
    } else {
        model <- model1
    }
    
    xx <- inla.posterior.sample(s, model)
    xx.s <- inla.posterior.sample.eval(function(...) c(theta[1], Predictor[idx.pred]), xx)
    y.pred <- matrix(NA, mpred, s)
    for(s.idx in 1:s) {
        xx.sample <- xx.s[, s.idx]
        y.pred[, s.idx] <- rnbinom(mpred, mu = exp(xx.sample[-1]), size = xx.sample[1])
    }
    preds <- list(year = 2016 + nyear, week = nweek, idx.pred = idx.pred, 
                  mean = apply(y.pred, 1, mean), median = apply(y.pred, 1, median),
                  lci = apply(y.pred, 1, quantile, probs = c(0.025)),
                  uci = apply(y.pred, 1, quantile, probs = c(0.975)))
    save(preds, file = paste0("C:\\Users\\u271201\\Downloads\\output\\preds\\preds_",2016 + yyear, "_", wweek, ".RData"))
} }}   

