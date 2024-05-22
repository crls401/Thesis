# R script to run INLA models of increasing complexity
# WARNING: the script may take over a day to run

# Step 0: load packages and pre-processed data 
# Step 1: formulate a baseline model including spatiotemporal random effects and test different combinations of DLNM climate indicators
# Step 2: using best fitting model in Step 1 to test interactions between PDSI-DLNM and 
# 1) % urban population from highly urbanised to more rural (land-use and density of population)
# centred at high (upper quartile), medium (median) and low (lower quartile)

# Step 0: load packages and pre-processed data
source("00_load_packages_data_Thesis.R")

# run models of increasing complexity in INLA

# Step 1: fit a baseline model including spatiotemporal random effects

# formulate a base model including: 
# state-specific monthly random effects to account for variation in seasonality between states (random walk cyclic prior)
# year-specific spatial random effects to account for interannual variation in spatial overdisperson and dependency structures (modified Besag-York-Mollie prior bym2)

# baseline model
baseformula <- Y ~ 1 + f(T1, replicate = S2, model = "rw1", cyclic = TRUE, constr = TRUE,
                         scale.model = TRUE,  hyper = precision.prior) +
  f(S1, model = "bym2", replicate = T2, graph = "C:\\Users\\u271201\\Downloads\\Data\\Sample test\\map.graph", 
    scale.model = TRUE, hyper = precision.prior) 

# test baseline model with Poisson distribution
# model <- mymodel(baseformula, family = "poisson")
# model$dic$dic


# define formulas by updating the baseline formula with different combinations of Tmin, Tmax and PDSI cross-basis functions
formula0.1 <- update.formula(baseformula, ~. + basis_MaxTm)
formula0.2 <- update.formula(baseformula, ~. + basis_NO2)
formula0.3 <- update.formula(baseformula, ~. + basis_PM10)
formula0.4 <- update.formula(baseformula, ~. + basis_PM25)
formula0.5 <- update.formula(baseformula, ~. + basis_O3)
formula0.6 <- update.formula(baseformula, ~. + basis_MaxTm + basis_NO2)
formula0.7 <- update.formula(baseformula, ~. + basis_MaxTm + basis_PM10)
formula0.8 <- update.formula(baseformula, ~. + basis_MaxTm + basis_PM25)
formula0.9 <- update.formula(baseformula, ~. + basis_MaxTm + basis_O3)

# create a list of formulas
formulas <- list(baseformula, formula0.1, formula0.2, formula0.3, formula0.4, formula0.5, formula0.6,formula0.7, formula0.8, formula0.9)


# create model label string
lab <- c("basemodel", "model0.1", "model0.2", "model0.3", "model0.4", "model0.5", "model0.6", "model0.7", "model0.8", "model0.9")

# create a function to run a model for each formula in the list and save the model output to file
# WARNING: this may take a long time to run
models <- lapply(1:length(formulas), 
                 function(i) {
                   model <- mymodel(formulas[[i]], df)
                   save(model, file = paste0("C:\\Users\\u271201\\Downloads\\output\\Mort_Female_65_79\\", lab[i],".RData"))})

# create table to store DIC and select best model 
table0 <- data.table(Model  = c("base", "MaxTm", "NO2", "PM10", "PM25","O3", "MaxTm + NO2", "MaxTm + PM10","MaxTm + PM25", "MaxTm + O3"), 
                     DIC = NA)

for(i in 1:length(formulas))
{
  load(paste0("C:\\Users\\u271201\\Downloads\\output\\ort_Female_65_79\\",lab[i],".RData"))
  table0$DIC[i] <- round(model$dic$dic, 0)
}

# view table
table0



# Step 3: use best fitting model in Step 2 to test interactions betweeen 
#PDSI-DLNM and % population living in urban areas and PDSI-DLNM.
#Centred on high , medium  and low  levels of urbanisation by pollutant


# assign formula for best fitting model to the new baseformula
# baseformula <- formulas[[best.fit]]

# redefine baseformula as best fitting model from above
baseformula <- Y ~ 1 + f(T1, replicate = S2, model = "rw1", cyclic = TRUE, constr = TRUE,
                         scale.model = TRUE,  hyper = precision.prior) +
  f(S1, model = "bym2", replicate = T2, graph = "C:\\Users\\u271201\\Downloads\\Data\\Sample test\\map.graph", 
    scale.model = TRUE, hyper = precision.prior) + basis_MaxTm + basis_PM10

# define formulas by updating the best fitting model0 formula with interactions between pdsi cross-basis and socio-economic indicators

formula1.1 <- update.formula(baseformula, ~. + basis_PM10_pop_density1 + Vw)
formula1.2 <- update.formula(baseformula, ~. + basis_PM10_pop_density2 + Vw)
formula1.3 <- update.formula(baseformula, ~. + basis_PM10_pop_density3 + Vw)

# create a list of formulas
formulas <- list(formula1.1, formula1.2, formula1.3)

# create model label string
lab <- c("model1.1_PM10_interaction", "model1.2_PM10_interaction", "model1.3_PM10_interaction")

# create a function to run a model for each formula in the list and save the model output to file
# WARNING: this may take a long time to run
models <- lapply(1:length(formulas), 
                 function(i) {
                   model <- mymodel(formulas[[i]], df)
                   save(model, file = paste0("C:\\Users\\u271201\\Downloads\\output\\Mort_Female_80_\\", lab[i],".RData"))})


# create table to store DIC
table1 <- data.frame(Model  = c("high interaction PM10", "intermediate interaction PM10", "low interaction PM10", DIC = NA))

for(i in 1:length(formulas))
{
  load(paste0("C:\\Users\\u271201\\Downloads\\output\\Mort_Female_80_\\",lab[i],".RData"))
  table1$DIC[i] <- round(model$dic$dic, 0)
}

# view table 
table1







# redefine baseformula as best fitting model from above
baseformula <- Y ~ 1 + f(T1, replicate = S2, model = "rw1", cyclic = TRUE, constr = TRUE,
                         scale.model = TRUE,  hyper = precision.prior) +
  f(S1, model = "bym2", replicate = T2, graph = "C:\\Users\\u271201\\Downloads\\Data\\Sample test\\map.graph", 
    scale.model = TRUE, hyper = precision.prior) + basis_MaxTm + basis_NO2

# define formulas by updating the best fitting model0 formula with interactions between pdsi cross-basis and socio-economic indicators

formula1.4 <- update.formula(baseformula, ~. + basis_NO2_pop_density1 + Vw)
formula1.5 <- update.formula(baseformula, ~. + basis_NO2_pop_density2 + Vw)
formula1.6 <- update.formula(baseformula, ~. + basis_NO2_pop_density3 + Vw)

# create a list of formulas
formulas <- list(formula1.4, formula1.5, formula1.6)

# create model label string
lab <- c(
  "model1.1_NO2_interaction", "model1.2_NO2_interaction", "model1.3_NO2_interaction")

# create a function to run a model for each formula in the list and save the model output to file
# WARNING: this may take a long time to run
models <- lapply(1:length(formulas), 
                 function(i) {
                   model <- mymodel(formulas[[i]], df)
                   save(model, file = paste0("C:\\Users\\u271201\\Downloads\\output\\Mort_Female_80_\\", lab[i],".RData"))})


# create table to store DIC
table2 <- data.frame(Model  = c("high interaction NO2", "intermediate interaction NO2", "low interaction NO2", DIC = NA))

for(i in 1:length(formulas))
{
  load(paste0("C:\\Users\\u271201\\Downloads\\output\\Mort_Female_80_\\",lab[i],".RData"))
  table1$DIC[i] <- round(model$dic$dic, 0)
}

# view table 
table2



# redefine baseformula as best fitting model from above
baseformula <- Y ~ 1 + f(T1, replicate = S2, model = "rw1", cyclic = TRUE, constr = TRUE,
                         scale.model = TRUE,  hyper = precision.prior) +
  f(S1, model = "bym2", replicate = T2, graph = "C:\\Users\\u271201\\Downloads\\Data\\Sample test\\map.graph", 
    scale.model = TRUE, hyper = precision.prior) + basis_MaxTm + basis_PM25


# define formulas by updating the best fitting model0 formula with interactions between pdsi cross-basis and socio-economic indicators

formula1.7 <- update.formula(baseformula, ~. + basis_PM25_pop_density1 + Vw)
formula1.8 <- update.formula(baseformula, ~. + basis_PM25_pop_density2 + Vw)
formula1.9 <- update.formula(baseformula, ~. + basis_PM25_pop_density3 + Vw)

# create a list of formulas
formulas <- list(formula1.7, formula1.8, formula1.9)

# create model label string
lab <- c(
  "model1.1_PM25_interaction", "model1.2_PM25_interaction", "model1.3_PM25_interaction")

# create a function to run a model for each formula in the list and save the model output to file
# WARNING: this may take a long time to run
models <- lapply(1:length(formulas), 
                 function(i) {
                   model <- mymodel(formulas[[i]], df)
                   save(model, file = paste0("C:\\Users\\u271201\\Downloads\\output\\Mort_Female_80_\\", lab[i],".RData"))})


# create table to store DIC
table3 <- data.frame(Model  = c("high interaction PM25", "intermediate interaction PM25", "low interaction PM25", DIC = NA))

for(i in 1:length(formulas))
{
  load(paste0("C:\\Users\\u271201\\Downloads\\output\\Mort_Female_80_\\",lab[i],".RData"))
  table1$DIC[i] <- round(model$dic$dic, 0)
}

# view table 
table3





# redefine baseformula as best fitting model from above
baseformula <- Y ~ 1 + f(T1, replicate = S2, model = "rw1", cyclic = TRUE, constr = TRUE,
                         scale.model = TRUE,  hyper = precision.prior) +
  f(S1, model = "bym2", replicate = T2, graph = "C:\\Users\\u271201\\Downloads\\Data\\Sample test\\map.graph", 
    scale.model = TRUE, hyper = precision.prior) + basis_MaxTm + basis_O3 


# define formulas by updating the best fitting model0 formula with interactions between pdsi cross-basis and socio-economic indicators

formula1.10 <- update.formula(baseformula, ~. + basis_O3_pop_density1 + Vw)
formula1.11 <- update.formula(baseformula, ~. + basis_O3_pop_density2 + Vw)
formula1.12 <- update.formula(baseformula, ~. + basis_O3_pop_density3 + Vw)

# create a list of formulas
formulas <- list(formula1.10, formula1.11, formula1.12)

# create model label string
lab <- c(
  "model1.1_O3_interaction", "model1.2_O3_interaction", "model1.3_O3_interaction")

# create a function to run a model for each formula in the list and save the model output to file
# WARNING: this may take a long time to run
models <- lapply(1:length(formulas), 
                 function(i) {
                   model <- mymodel(formulas[[i]], df)
                   save(model, file = paste0("C:\\Users\\u271201\\Downloads\\output\\Mort_Female_80_\\", lab[i],".RData"))})


# create table to store DIC
table4 <- data.frame(Model  = c("high interaction O3", "intermediate interaction O3", "low interaction O3", DIC = NA))

for(i in 1:length(formulas))
{
  load(paste0("C:\\Users\\u271201\\Downloads\\output\\Mort_Female_80_\\",lab[i],".RData"))
  table1$DIC[i] <- round(model$dic$dic, 0)
}

# view table 
table4


############################RANDOM EFFECTS######################################
df$YearMonth <- interaction(df$Year, df$Month)


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




