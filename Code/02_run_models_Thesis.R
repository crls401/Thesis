# R script to run INLA models of increasing complexity
# WARNING: the script may take over a day to run

# Step 0: load packages and pre-processed data
source("00_load_packages_data_Thesis.R")

# run models of increasing complexity in INLA

# Step 1: fit a baseline model including spatiotemporal random effects

# formulate a base model including: 
# province-specific weekly random effects to account for variation in seasonality between provinces (random walk cyclic prior)
# year-specific spatial random effects to account for interannual variation in spatial overdisperson and dependency structures (modified Besag-York-Mollie prior bym2)
inla.setOption(num.threads = "4:1")
# baseline model
baseformula <- Y ~ 1 + f(T1, replicate = S2, model = "rw1", cyclic = TRUE, constr = TRUE,
                         scale.model = TRUE,  hyper = precision.prior) +
  f(S1, model = "bym2", replicate = T2, graph = "C:\\Users\\u271201\\Downloads\\Data\\Sample test\\map.graph", 
    scale.model = TRUE, hyper = precision.prior) 

# test baseline model with Poisson distribution
# model <- mymodel(baseformula, family = "poisson")
# model$dic$dic


# define formulas by updating the baseline formula with different combinations of Tmax and cross-basis functions
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
                   save(model, file = paste0("C:\\Users\\u271201\\Downloads\\output\\Mort_Female_65_\\Prova\\", lab[i],".RData"))})

# create table to store DIC and select best model 
table0 <- data.table(Model  = c("base", "MaxTm", "NO2", "PM10", "PM25","O3", "MaxTm + NO2", "MaxTm + PM10","MaxTm + PM25", "MaxTm + O3"), 
                     DIC = NA)

for(i in 1:length(formulas))
{
  load(paste0("C:\\Users\\u271201\\Downloads\\output\\Mort_Female_65_\\Prova\\",lab[i],".RData"))
  table0$DIC[i] <- round(model$dic$dic, 0)
}

# view table
table0



# Step 3: use best fitting model in Step 2 to test interactions 
# assign formula for best fitting model to the new baseformula
# baseformula <- formulas[[best.fit]]

# redefine baseformula as best fitting model from above
baseformula <- Y ~ 1 + f(T1, replicate = S2, model = "rw1", cyclic = TRUE, constr = TRUE,
                         scale.model = TRUE,  hyper = precision.prior) +
  f(S1, model = "bym2", replicate = T2, graph = "C:\\Users\\u271201\\Downloads\\Data\\Sample test\\map.graph", 
    scale.model = TRUE, hyper = precision.prior) + basis_MaxTm + basis_PM10

# define formulas by updating the best fitting model0 formula with interactions  

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
                   save(model, file = paste0("C:\\Users\\u271201\\Downloads\\output\\Mort_Female_65_\\Prova\\", lab[i],".RData"))})


# create table to store DIC
table1 <- data.frame(Model  = c("high interaction PM10", "intermediate interaction PM10", "low interaction PM10", DIC = NA))

for(i in 1:length(formulas))
{
  load(paste0("C:\\Users\\u271201\\Downloads\\output\\Mort_Female_65_\\Prova\\",lab[i],".RData"))
  table1$DIC[i] <- round(model$dic$dic, 0)
}

# view table 
table1







# redefine baseformula as best fitting model from above
baseformula <- Y ~ 1 + f(T1, replicate = S2, model = "rw1", cyclic = TRUE, constr = TRUE,
                         scale.model = TRUE,  hyper = precision.prior) +
  f(S1, model = "bym2", replicate = T2, graph = "C:\\Users\\u271201\\Downloads\\Data\\Sample test\\map.graph", 
    scale.model = TRUE, hyper = precision.prior) + basis_MaxTm + basis_NO2

# define formulas by updating the best fitting model0 formula with interactions  

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
                   save(model, file = paste0("C:\\Users\\u271201\\Downloads\\output\\Mort_Female_65_\\Prova\\", lab[i],".RData"))})


# create table to store DIC
table2 <- data.frame(Model  = c("high interaction NO2", "intermediate interaction NO2", "low interaction NO2", DIC = NA))

for(i in 1:length(formulas))
{
  load(paste0("C:\\Users\\u271201\\Downloads\\output\\Mort_Female_65_\\Prova\\",lab[i],".RData"))
  table1$DIC[i] <- round(model$dic$dic, 0)
}

# view table 
table2



# redefine baseformula as best fitting model from above
baseformula <- Y ~ 1 + f(T1, replicate = S2, model = "rw1", cyclic = TRUE, constr = TRUE,
                         scale.model = TRUE,  hyper = precision.prior) +
  f(S1, model = "bym2", replicate = T2, graph = "C:\\Users\\u271201\\Downloads\\Data\\Sample test\\map.graph", 
    scale.model = TRUE, hyper = precision.prior) + basis_MaxTm + basis_PM25


# define formulas by updating the best fitting model0 formula with interactions between cross-basis and density population indicators

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
                   save(model, file = paste0("C:\\Users\\u271201\\Downloads\\output\\Mort_Female_65_\\Prova\\", lab[i],".RData"))})


# create table to store DIC
table3 <- data.frame(Model  = c("high interaction PM25", "intermediate interaction PM25", "low interaction PM25", DIC = NA))

for(i in 1:length(formulas))
{
  load(paste0("C:\\Users\\u271201\\Downloads\\output\\Mort_Female_65_\\Prova\\",lab[i],".RData"))
  table1$DIC[i] <- round(model$dic$dic, 0)
}

# view table 
table3





# redefine baseformula as best fitting model from above
baseformula <- Y ~ 1 + f(T1, replicate = S2, model = "rw1", cyclic = TRUE, constr = TRUE,
                         scale.model = TRUE,  hyper = precision.prior) +
  f(S1, model = "bym2", replicate = T2, graph = "C:\\Users\\u271201\\Downloads\\Data\\Sample test\\map.graph", 
    scale.model = TRUE, hyper = precision.prior) + basis_MaxTm + basis_O3 


# define formulas by updating the best fitting model0 formula with interactions 

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
                   save(model, file = paste0("C:\\Users\\u271201\\Downloads\\output\\Mort_Female_65_\\Prova\\", lab[i],".RData"))})


# create table to store DIC
table4 <- data.frame(Model  = c("high interaction O3", "intermediate interaction O3", "low interaction O3", DIC = NA))

for(i in 1:length(formulas))
{
  load(paste0("C:\\Users\\u271201\\Downloads\\output\\Mort_Female_65_\\Prova\\",lab[i],".RData"))
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


# define formulas by updating the baseline formula with different combinations for Random effects
formula2.1 <- update.formula(baseformula, ~. + basis_NO2 + f(YearMonth, model = "iid"))
formula2.2 <- update.formula(baseformula, ~. + basis_NO2 + f(interaction(NUTS3, YearMonth), model = "iid"))
formula2.3 <- update.formula(baseformula, ~. + basis_PM10 + f(YearMonth, model = "iid"))
formula2.4 <- update.formula(baseformula, ~. + basis_PM10 + f(interaction(NUTS3, YearMonth), model = "iid"))
formula2.5 <- update.formula(baseformula, ~. + basis_PM25 + f(YearMonth, model = "iid"))
formula2.6 <- update.formula(baseformula, ~. + basis_PM25 + f(interaction(NUTS3, YearMonth), model = "iid"))
formula2.7 <- update.formula(baseformula, ~. + basis_O3 + f(YearMonth, model = "iid"))
formula2.8 <- update.formula(baseformula, ~. + basis_O3 + f(interaction(NUTS3, YearMonth), model = "iid"))


# create a list of formulas
formulas <- list(baseformula, formula2.1, formula2.2, formula2.3, formula2.4, formula2.5, formula2.6,formula2.7, formula2.8)


# create model label string
lab <- c("basemodel", "NO2_Month_rf.RData", "NO2_Month_rf_nuts", "PM10_Month_rf.RData", "PM10_Month_rf_nuts", "PM25_Month_rf.RData", "PM25_Month_rf_nuts", "O3_Month_rf.RData", "O3_Month_rf_nuts")

# create a function to run a model for each formula in the list and save the model output to file
# WARNING: this may take a long time to run
models <- lapply(1:length(formulas), 
                 function(i) {
                   model <- mymodel(formulas[[i]], df)
                   save(model, file = paste0("C:\\Users\\u271201\\Downloads\\output\\Mort_Female_65_\\Prova\\", lab[i],".RData"))})









################################################################################
###################################DIC##########################################
################################################################################

lab <- c("model0.1","model0.6", "model0.7","model0.8", "model0.9")

table1 <- data.frame(Model  = c("basemodel", "MaxTm","MaxTm + NO2", "MaxTm + PM10","MaxTm + PM25", "MaxTm + O3"))
table2 <- data.frame(Model  = c("basemodel", "MaxTm","MaxTm + NO2", "MaxTm + PM10","MaxTm + PM25", "MaxTm + O3"))
table3 <- data.frame(Model  = c("basemodel", "MaxTm","MaxTm + NO2", "MaxTm + PM10","MaxTm + PM25", "MaxTm + O3"))
table4 <- data.frame(Model  = c("basemodel", "MaxTm","MaxTm + NO2", "MaxTm + PM10","MaxTm + PM25", "MaxTm + O3"))

for(i in 1:length(lab))
{
  load(paste0("C:\\Users\\u271201\\Downloads\\output\\Mort_Male_65_79\\",lab[i],".RData"))
  table1$DIC[i] <- round(model$dic$dic, 0)
}

for(i in 1:length(lab))
{
  load(paste0("C:\\Users\\u271201\\Downloads\\output\\Mort_Female_65_79\\",lab[i],".RData"))
  table2$DIC[i] <- round(model$dic$dic, 0)
}

for(i in 1:length(lab))
{
  load(paste0("C:\\Users\\u271201\\Downloads\\output\\Mort_Male_80_\\",lab[i],".RData"))
  table3$DIC[i] <- round(model$dic$dic, 0)
}

for(i in 1:length(lab))
{
  load(paste0("C:\\Users\\u271201\\Downloads\\output\\Mort_Female_80_\\",lab[i],".RData"))
  table4$DIC[i] <- round(model$dic$dic, 0)
}

table5 <- data.frame(Model  = c("basemodel", "NO2_Month_rf","NO2_Month_rf_nuts", "PM10_Month_rf", "PM10_Month_rf_nuts", "PM25_Month_rf", "PM25_Month_rf_nuts", "O3_Month_rf", "O3_Month_rf_nuts"))
table6 <- data.frame(Model  = c("basemodel", "NO2_Month_rf","NO2_Month_rf_nuts", "PM10_Month_rf", "PM10_Month_rf_nuts", "PM25_Month_rf", "PM25_Month_rf_nuts", "O3_Month_rf", "O3_Month_rf_nuts"))

for(i in 1:length(lab))
{
  load(paste0("C:\\Users\\u271201\\Downloads\\output\\Mort_Male_65_\\",lab[i],".RData"))
  table5$DIC[i] <- round(model$dic$dic, 0)
}

for(i in 1:length(lab))
{
  load(paste0("C:\\Users\\u271201\\Downloads\\output\\Mort_Female_65_\\",lab[i],".RData"))
  table6$DIC[i] <- round(model$dic$dic, 0)
}

# Adding a new column to each table to denote the source
table1$Source <- "Male_65_79"
table2$Source <- "Female_65_79"
table3$Source <- "Male_80_"
table4$Source <- "Female_80_"
table5$Source <- "Male_65_"
table6$Source <- "Female_65_"

