
# redefine baseformula as best fitting model from above
baseformula <- Y ~ 1 + f(T1, replicate = S2, model = "rw1", cyclic = TRUE, constr = TRUE,
                         scale.model = TRUE,  hyper = precision.prior) +
  f(S1, model = "bym2", replicate = T2, graph = "C:\\Users\\u271201\\Downloads\\Data\\Sample test\\map.graph", 
    scale.model = TRUE, hyper = precision.prior) + basis_MaxTm 

# define formulas by updating the best fitting model0 formula with interactions between pdsi cross-basis and socio-economic indicators

formula1.1 <- update.formula(baseformula, ~. + basis_PM10_pop_density1)
formula1.2 <- update.formula(baseformula, ~. + basis_PM10_pop_density2)
formula1.3 <- update.formula(baseformula, ~. + basis_PM10_pop_density3)

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
    scale.model = TRUE, hyper = precision.prior) + basis_MaxTm 

# define formulas by updating the best fitting model0 formula with interactions between pdsi cross-basis and socio-economic indicators

formula1.4 <- update.formula(baseformula, ~. + basis_NO2_pop_density1)
formula1.5 <- update.formula(baseformula, ~. + basis_NO2_pop_density2)
formula1.6 <- update.formula(baseformula, ~. + basis_NO2_pop_density3)

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
    scale.model = TRUE, hyper = precision.prior) + basis_MaxTm 


# define formulas by updating the best fitting model0 formula with interactions between pdsi cross-basis and socio-economic indicators

formula1.7 <- update.formula(baseformula, ~. + basis_PM25_pop_density1 )
formula1.8 <- update.formula(baseformula, ~. + basis_PM25_pop_density2 )
formula1.9 <- update.formula(baseformula, ~. + basis_PM25_pop_density3 )

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
    scale.model = TRUE, hyper = precision.prior) + basis_MaxTm 


# define formulas by updating the best fitting model0 formula with interactions between pdsi cross-basis and socio-economic indicators

formula1.10 <- update.formula(baseformula, ~. + basis_O3_pop_density1 )
formula1.11 <- update.formula(baseformula, ~. + basis_O3_pop_density2 )
formula1.12 <- update.formula(baseformula, ~. + basis_O3_pop_density3 )

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
load("C:\\Users\\u271201\\Downloads\\output\\Mort_Female_65_\\Prova\\model1.3_NO2_interaction.RData")
model1 <- model
load("C:\\Users\\u271201\\Downloads\\output\\Mort_Female_65_\\Prova\\model1.3_O3_interaction.RData")
model2 <- model
load("C:\\Users\\u271201\\Downloads\\output\\Mort_Female_65_\\Prova\\model1.3_PM10_interaction.RData")
model3 <- model
load("C:\\Users\\u271201\\Downloads\\output\\Mort_Female_65_\\Prova\\model1.3_PM25_interaction.RData")
model4 <- model 

mod.name <- c("model1", "model2", "model3", "model4")

table1 <- as.data.frame(matrix(NA, 4, 16))
colnames(table1) <- c("Pollutants", 
                      "extr_var", "extr_lag", "extr_rr", "extr_lci","extr_uci",
                      "extr1_var", "extr1_lag", "extr1_rr", "extr1_lci","extr1_uci",
                      "extr2_var", "extr2_lag", "extr2_rr", "extr2_lci","extr2_uci")
table1[,1] <- c("NO2","O3","PM10","PM25")

for (j in 1:length(mod.name))
{
  model <- eval(parse(text = as.name(mod.name[j]))) 
  
  # extract coefficients and variance-covariance matrix
  coef <- model$summary.fixed$mean
  vcov <- model$misc$lincomb.derived.covariance.matrix
  
  # Create indicators for terms associated with PDSI cross basis
  indp <- grep("basis_MaxTm", model$names.fixed)
  
  # Extract predictions from the PDSI DLNM centered on zero (normal conditions)
  predp <- crosspred(basis_MaxTm, coef = coef[indp], vcov = vcov[indp, indp],
                     model.link = "log", bylag = 0.1, cen = 34)
  lagbylag <- seq(0, 0.25, length.out = length(predp$matRRfit) / length(unique(predp$predvar)))
  
  # get exposures values
  vars<-predp$predvar
  
  results_df <- data.frame(rr = as.vector(predp$matRRfit),
                           rr.lci = as.vector(predp$matRRlow),
                           rr.uci = as.vector(predp$matRRhigh),
                           var = rep(vars, length(lagbylag)),
                           lag = rep(lagbylag, each = length(vars)))
  
  
  extr <- results_df[results_df$var == 40,]
  extr1 <- results_df[results_df$var == 35,]
  extr2 <- results_df[results_df$var == 30,]
  extr_ind <- which.max(extr$rr) 
  extr1_ind <- which.max(extr1$rr)
  extr2_ind <- which.max(extr2$rr)
  
  
  
  table1$extr_var[j] <- extr$var[extr_ind]
  table1$extr_lag[j] <- round(extr$lag[extr_ind],0)
  table1$extr_rr[j]  <- round(extr$rr[extr_ind], 2)
  table1$extr_lci[j] <- round(extr$rr.lci[extr_ind], 2)
  table1$extr_uci[j] <- round(extr$rr.uci[extr_ind], 2)
  table1$extr1_var[j] <- extr1$var[extr1_ind]
  table1$extr1_lag[j] <- round(extr1$lag[extr1_ind], 0)
  table1$extr1_rr[j]  <- round(extr1$rr[extr1_ind], 2)
  table1$extr1_lci[j] <- round(extr1$rr.lci[extr1_ind], 2)
  table1$extr1_uci[j] <- round(extr1$rr.uci[extr1_ind], 2)
  table1$extr2_var[j] <- extr2$var[extr2_ind]
  table1$extr2_lag[j] <- round(extr2$lag[extr2_ind], 0)
  table1$extr2_rr[j]  <- round(extr2$rr[extr2_ind], 2)
  table1$extr2_lci[j] <- round(extr2$rr.lci[extr2_ind], 2)
  table1$extr2_uci[j] <- round(extr2$rr.uci[extr2_ind], 2)
  
}
