


# baseline model
baseformula <-Y ~ 1 + 
  f(T1, replicate = S2, model = "rw1", cyclic = TRUE, constr = TRUE, 
    scale.model = TRUE, hyper = precision.prior) +
  f(S1, model = "bym2", replicate = T2, graph = "C:\\Users\\u271201\\Downloads\\Data\\Sample test\\map.graph", 
    scale.model = TRUE, hyper = precision.prior) +
  basis_MaxTm 

# test baseline model with Poisson distribution
# model <- mymodel(baseformula, family = "poisson")
# model$dic$dic


# define formulas by updating the baseline formula with different combinations of Tmin, Tmax and PDSI cross-basis functions
formula0.1 <- update.formula(baseformula, ~. + basis_NO2 + f(YearMonth, model = "iid"))
formula0.2 <- update.formula(baseformula, ~. + basis_NO2 + f(interaction(NUTS3, YearMonth), model = "iid"))
formula0.3 <- update.formula(baseformula, ~. + basis_PM10 + f(YearMonth, model = "iid"))
formula0.4 <- update.formula(baseformula, ~. + basis_PM10 + f(interaction(NUTS3, YearMonth), model = "iid"))
formula0.5 <- update.formula(baseformula, ~. + basis_PM25 + f(YearMonth, model = "iid"))
formula0.6 <- update.formula(baseformula, ~. + basis_PM25 + f(interaction(NUTS3, YearMonth), model = "iid"))
formula0.7 <- update.formula(baseformula, ~. + basis_O3 + f(YearMonth, model = "iid"))
formula0.8 <- update.formula(baseformula, ~. + basis_O3 + f(interaction(NUTS3, YearMonth), model = "iid"))


# create a list of formulas
formulas <- list(baseformula, formula0.1, formula0.2, formula0.3, formula0.4, formula0.5, formula0.6,formula0.7, formula0.8)


# create model label string
lab <- c("basemodel", "NO2_Month_rf.RData", "NO2_Month_rf_nuts", "PM10_Month_rf.RData", "PM10_Month_rf_nuts", "PM25_Month_rf.RData", "PM25_Month_rf_nuts", "O3_Month_rf.RData", "O3_Month_rf_nuts")

# create a function to run a model for each formula in the list and save the model output to file
# WARNING: this may take a long time to run
models <- lapply(1:length(formulas), 
                 function(i) {
                   model <- mymodel(formulas[[i]], df)
                   save(model, file = paste0("C:\\Users\\u271201\\Downloads\\output\\Mort_Female_65_79\\", lab[i],".RData"))})
