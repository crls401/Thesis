################################################################################
                      #Level 1 of urban density# 
################################################################################

load("C:\\Users\\u271201\\Downloads\\output\\Mort_tot_80_\\model1.1_NO2_interaction.RData")
model1 <- model
load("C:\\Users\\u271201\\Downloads\\output\\Mort_tot_80_\\model1.1_O3_interaction.RData")
model2 <- model
load("C:\\Users\\u271201\\Downloads\\output\\Mort_tot_80_\\model1.1_PM10_interaction.RData")
model3 <- model
load("C:\\Users\\u271201\\Downloads\\output\\Mort_tot_80_\\model1.1_PM25_interaction.RData")
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


load("C:\\Users\\u271201\\Downloads\\output\\Mort_tot_65_79\\model1.1_NO2_interaction.RData")
model1 <- model
load("C:\\Users\\u271201\\Downloads\\output\\Mort_tot_65_79\\model1.1_O3_interaction.RData")
model2 <- model
load("C:\\Users\\u271201\\Downloads\\output\\Mort_tot_65_79\\model1.1_PM10_interaction.RData")
model3 <- model
load("C:\\Users\\u271201\\Downloads\\output\\Mort_tot_65_79\\model1.1_PM25_interaction.RData")
model4 <- model 

mod.name <- c("model1", "model2", "model3", "model4")

table2 <- as.data.frame(matrix(NA, 4, 16))
colnames(table2) <- c("Pollutants", 
                      "extr_var", "extr_lag", "extr_rr", "extr_lci","extr_uci",
                      "extr1_var", "extr1_lag", "extr1_rr", "extr1_lci","extr1_uci",
                      "extr2_var", "extr2_lag", "extr2_rr", "extr2_lci","extr2_uci")
table2[,1] <- c("NO2","O3","PM10","PM25")

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
  
  
  
  table2$extr_var[j] <- extr$var[extr_ind]
  table2$extr_lag[j] <- round(extr$lag[extr_ind],0)
  table2$extr_rr[j]  <- round(extr$rr[extr_ind], 2)
  table2$extr_lci[j] <- round(extr$rr.lci[extr_ind], 2)
  table2$extr_uci[j] <- round(extr$rr.uci[extr_ind], 2)
  table2$extr1_var[j] <- extr1$var[extr1_ind]
  table2$extr1_lag[j] <- round(extr1$lag[extr1_ind], 0)
  table2$extr1_rr[j]  <- round(extr1$rr[extr1_ind], 2)
  table2$extr1_lci[j] <- round(extr1$rr.lci[extr1_ind], 2)
  table2$extr1_uci[j] <- round(extr1$rr.uci[extr1_ind], 2)
  table2$extr2_var[j] <- extr2$var[extr2_ind]
  table2$extr2_lag[j] <- round(extr2$lag[extr2_ind], 0)
  table2$extr2_rr[j]  <- round(extr2$rr[extr2_ind], 2)
  table2$extr2_lci[j] <- round(extr2$rr.lci[extr2_ind], 2)
  table2$extr2_uci[j] <- round(extr2$rr.uci[extr2_ind], 2)
  
}

# Add a new column to each table to identify the group
table1$Age <- "80"
table2$Age <- "65"


# Combine the tables
combined_table <- rbind(table1, table2)

# If you need to reorder columns so 'Age' appears first for better readability
combined_table1 <- combined_table[c("Age", names(combined_table)[!names(combined_table) %in% "Age"])]
################################################################################
                        #Level 2 of urban density# 
################################################################################

load("C:\\Users\\u271201\\Downloads\\output\\Mort_tot_80_\\model1.2_NO2_interaction.RData")
model1 <- model
load("C:\\Users\\u271201\\Downloads\\output\\Mort_tot_80_\\model1.2_O3_interaction.RData")
model2 <- model
load("C:\\Users\\u271201\\Downloads\\output\\Mort_tot_80_\\model1.2_PM10_interaction.RData")
model3 <- model
load("C:\\Users\\u271201\\Downloads\\output\\Mort_tot_80_\\model1.2_PM25_interaction.RData")
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


load("C:\\Users\\u271201\\Downloads\\output\\Mort_tot_65_79\\model1.2_NO2_interaction.RData")
model1 <- model
load("C:\\Users\\u271201\\Downloads\\output\\Mort_tot_65_79\\model1.2_O3_interaction.RData")
model2 <- model
load("C:\\Users\\u271201\\Downloads\\output\\Mort_tot_65_79\\model1.2_PM10_interaction.RData")
model3 <- model
load("C:\\Users\\u271201\\Downloads\\output\\Mort_tot_65_79\\model1.2_PM25_interaction.RData")
model4 <- model 

mod.name <- c("model1", "model2", "model3", "model4")

table2 <- as.data.frame(matrix(NA, 4, 16))
colnames(table2) <- c("Pollutants", 
                      "extr_var", "extr_lag", "extr_rr", "extr_lci","extr_uci",
                      "extr1_var", "extr1_lag", "extr1_rr", "extr1_lci","extr1_uci",
                      "extr2_var", "extr2_lag", "extr2_rr", "extr2_lci","extr2_uci")
table2[,1] <- c("NO2","O3","PM10","PM25")

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
  
  
  
  table2$extr_var[j] <- extr$var[extr_ind]
  table2$extr_lag[j] <- round(extr$lag[extr_ind],0)
  table2$extr_rr[j]  <- round(extr$rr[extr_ind], 2)
  table2$extr_lci[j] <- round(extr$rr.lci[extr_ind], 2)
  table2$extr_uci[j] <- round(extr$rr.uci[extr_ind], 2)
  table2$extr1_var[j] <- extr1$var[extr1_ind]
  table2$extr1_lag[j] <- round(extr1$lag[extr1_ind], 0)
  table2$extr1_rr[j]  <- round(extr1$rr[extr1_ind], 2)
  table2$extr1_lci[j] <- round(extr1$rr.lci[extr1_ind], 2)
  table2$extr1_uci[j] <- round(extr1$rr.uci[extr1_ind], 2)
  table2$extr2_var[j] <- extr2$var[extr2_ind]
  table2$extr2_lag[j] <- round(extr2$lag[extr2_ind], 0)
  table2$extr2_rr[j]  <- round(extr2$rr[extr2_ind], 2)
  table2$extr2_lci[j] <- round(extr2$rr.lci[extr2_ind], 2)
  table2$extr2_uci[j] <- round(extr2$rr.uci[extr2_ind], 2)
  
}

# Add a new column to each table to identify the group
table1$Age <- "80"
table2$Age <- "65"


# Combine the tables
combined_table <- rbind(table1, table2)

# If you need to reorder columns so 'Age' appears first for better readability
combined_table2 <- combined_table[c("Age", names(combined_table)[!names(combined_table) %in% "Age"])]

################################################################################
                       #Level 3 of urban density# 
################################################################################

load("C:\\Users\\u271201\\Downloads\\output\\Mort_tot_80_\\model1.3_NO2_interaction.RData")
model1 <- model
load("C:\\Users\\u271201\\Downloads\\output\\Mort_tot_80_\\model1.3_O3_interaction.RData")
model2 <- model
load("C:\\Users\\u271201\\Downloads\\output\\Mort_tot_80_\\model1.3_PM10_interaction.RData")
model3 <- model
load("C:\\Users\\u271201\\Downloads\\output\\Mort_tot_80_\\model1.3_PM25_interaction.RData")
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


load("C:\\Users\\u271201\\Downloads\\output\\Mort_tot_65_79\\model1.3_NO2_interaction.RData")
model1 <- model
load("C:\\Users\\u271201\\Downloads\\output\\Mort_tot_65_79\\model1.3_O3_interaction.RData")
model2 <- model
load("C:\\Users\\u271201\\Downloads\\output\\Mort_tot_65_79\\model1.3_PM10_interaction.RData")
model3 <- model
load("C:\\Users\\u271201\\Downloads\\output\\Mort_tot_65_79\\model1.3_PM25_interaction.RData")
model4 <- model 

mod.name <- c("model1", "model2", "model3", "model4")

table2 <- as.data.frame(matrix(NA, 4, 16))
colnames(table2) <- c("Pollutants", 
                      "extr_var", "extr_lag", "extr_rr", "extr_lci","extr_uci",
                      "extr1_var", "extr1_lag", "extr1_rr", "extr1_lci","extr1_uci",
                      "extr2_var", "extr2_lag", "extr2_rr", "extr2_lci","extr2_uci")
table2[,1] <- c("NO2","O3","PM10","PM25")

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
  
  
  
  table2$extr_var[j] <- extr$var[extr_ind]
  table2$extr_lag[j] <- round(extr$lag[extr_ind],0)
  table2$extr_rr[j]  <- round(extr$rr[extr_ind], 2)
  table2$extr_lci[j] <- round(extr$rr.lci[extr_ind], 2)
  table2$extr_uci[j] <- round(extr$rr.uci[extr_ind], 2)
  table2$extr1_var[j] <- extr1$var[extr1_ind]
  table2$extr1_lag[j] <- round(extr1$lag[extr1_ind], 0)
  table2$extr1_rr[j]  <- round(extr1$rr[extr1_ind], 2)
  table2$extr1_lci[j] <- round(extr1$rr.lci[extr1_ind], 2)
  table2$extr1_uci[j] <- round(extr1$rr.uci[extr1_ind], 2)
  table2$extr2_var[j] <- extr2$var[extr2_ind]
  table2$extr2_lag[j] <- round(extr2$lag[extr2_ind], 0)
  table2$extr2_rr[j]  <- round(extr2$rr[extr2_ind], 2)
  table2$extr2_lci[j] <- round(extr2$rr.lci[extr2_ind], 2)
  table2$extr2_uci[j] <- round(extr2$rr.uci[extr2_ind], 2)
  
}

# Add a new column to each table to identify the group
table1$Age <- "80"
table2$Age <- "65"


# Combine the tables
combined_table <- rbind(table1, table2)

# If you need to reorder columns so 'Age' appears first for better readability
combined_table80 <- combined_table[c("Age", names(combined_table)[!names(combined_table) %in% "Age"])]

combined_data <- combined_table1 %>%
  full_join(combined_table2,by = c("Age", "Pollutants"))%>% 
  full_join(combined_table3, by = c("Age", "Pollutants"))
# Identify columns that don't end with ".x" or ".y"
cols_without_xy <- grep("(?<!\\.(x|y))$", names(combined_data), value = TRUE, perl = TRUE)

# Add ".z" suffix to these identified columns
names(combined_data)[names(combined_data) %in% cols_without_xy] <- paste0(cols_without_xy, ".z")

# Check the renamed columns
print(names(combined_data))

# Step 1: Rename columns replacing .x, .y, .z with 1, 2, 3
combined_data <- combined_data %>%
  rename_with(
    .fn = ~sub("\\.x$", ".1", sub("\\.y$", ".2", sub("\\.z$", ".3", .))),
    .cols = everything()
  )

# Step 2: Reshape the data with pivot_longer
long_data <- combined_data %>%
  pivot_longer(
    cols = matches("extr[0-9]?_(var|lag|rr|lci|uci)\\.(1|2|3)"),
    names_to = c("group", "measure", "type"),
    names_pattern = "(extr[0-9]?|extr[0-9]?|extr[0-9]?)_(var|lag|rr|lci|uci)\\.(1|2|3)",
    values_to = "value"
  ) %>%
  pivot_wider(
    names_from = measure,
    values_from = value
  ) %>%
  mutate(
    var = as.numeric(var),
    lag = as.numeric(lag),
    rr = as.numeric(rr),
    lci = as.numeric(lci),
    uci = as.numeric(uci)
  )

# Print the structure to verify the changes
glimpse(long_data)

write.csv(long_data, "C:\\Users\\u271201\\Downloads\\Figures analysis\\4.Urban\\Tables\\RR Urban.csv", row.names = FALSE)











################################################################################
##Levels of pollutants 65# 
################################################################################

load("C:\\Users\\u271201\\Downloads\\output\\Mort_tot_65_79\\model0.6.RData")
model1 <- model
load("C:\\Users\\u271201\\Downloads\\output\\Mort_tot_65_79\\model0.7.RData")
model2 <- model
load("C:\\Users\\u271201\\Downloads\\output\\Mort_tot_65_79\\model0.8.RData")
model3 <- model
load("C:\\Users\\u271201\\Downloads\\output\\Mort_tot_65_79\\model0.9.RData")
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
################################################################################
#Levels of pollutants 80# 
################################################################################

load("C:\\Users\\u271201\\Downloads\\output\\Mort_tot_80_\\model0.6.RData")
model1 <- model
load("C:\\Users\\u271201\\Downloads\\output\\Mort_tot_80_\\model0.7.RData")
model2 <- model
load("C:\\Users\\u271201\\Downloads\\output\\Mort_tot_80_\\model0.8.RData")
model3 <- model
load("C:\\Users\\u271201\\Downloads\\output\\Mort_tot_80_\\model0.9.RData")
model4 <- model 

mod.name <- c("model1", "model2", "model3", "model4")

table2 <- as.data.frame(matrix(NA, 4, 16))
colnames(table2) <- c("Pollutants", 
                      "extr_var", "extr_lag", "extr_rr", "extr_lci","extr_uci",
                      "extr1_var", "extr1_lag", "extr1_rr", "extr1_lci","extr1_uci",
                      "extr2_var", "extr2_lag", "extr2_rr", "extr2_lci","extr2_uci")
table2[,1] <- c("NO2","O3","PM10","PM25")

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
  
  
  
  table2$extr_var[j] <- extr$var[extr_ind]
  table2$extr_lag[j] <- round(extr$lag[extr_ind],0)
  table2$extr_rr[j]  <- round(extr$rr[extr_ind], 2)
  table2$extr_lci[j] <- round(extr$rr.lci[extr_ind], 2)
  table2$extr_uci[j] <- round(extr$rr.uci[extr_ind], 2)
  table2$extr1_var[j] <- extr1$var[extr1_ind]
  table2$extr1_lag[j] <- round(extr1$lag[extr1_ind], 0)
  table2$extr1_rr[j]  <- round(extr1$rr[extr1_ind], 2)
  table2$extr1_lci[j] <- round(extr1$rr.lci[extr1_ind], 2)
  table2$extr1_uci[j] <- round(extr1$rr.uci[extr1_ind], 2)
  table2$extr2_var[j] <- extr2$var[extr2_ind]
  table2$extr2_lag[j] <- round(extr2$lag[extr2_ind], 0)
  table2$extr2_rr[j]  <- round(extr2$rr[extr2_ind], 2)
  table2$extr2_lci[j] <- round(extr2$rr.lci[extr2_ind], 2)
  table2$extr2_uci[j] <- round(extr2$rr.uci[extr2_ind], 2)
  
}

# Add a new column to each table to identify the group
table1$Age <- "65"
table2$Age <- "80"



# Combine the tables
combined_table <- rbind(table1, table2)




# Step 2: Reshape the data with pivot_longer
long_data <- combined_table %>%
  pivot_longer(
    cols = matches("extr[0-9]?_(var|lag|rr|lci|uci)"),
    names_to = c("group", "measure"),
    names_pattern = "(extr[0-9]?|extr[0-9]?|extr[0-9]?)_(var|lag|rr|lci|uci)",
    values_to = "value"
  ) %>%
  pivot_wider(
    names_from = measure,
    values_from = value
  ) %>%
  mutate(
    var = as.numeric(var),
    lag = as.numeric(lag),
    rr = as.numeric(rr),
    lci = as.numeric(lci),
    uci = as.numeric(uci)
  )

# Print the structure to verify the changes
glimpse(long_data)

write.csv(long_data, "C:\\Users\\u271201\\Downloads\\Figures analysis\\4.Urban\\Tables\\RR pollutatnt for Urban.csv", row.names = FALSE)










