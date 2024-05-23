################################################################################
                              #Table 80#
################################################################################

load("C:\\Users\\u271201\\Downloads\\output\\Mort_Female_80_\\model0.6.RData")
model1 <- model
load("C:\\Users\\u271201\\Downloads\\output\\Mort_Female_80_\\model0.7.RData")
model2 <- model
load("C:\\Users\\u271201\\Downloads\\output\\Mort_Female_80_\\model0.8.RData")
model3 <- model
load("C:\\Users\\u271201\\Downloads\\output\\Mort_Female_80_\\model0.9.RData")
model4 <- model 

mod.name <- c("model1", "model2", "model3", "model4")

table1 <- as.data.frame(matrix(NA, 4, 16))
colnames(table1) <- c("Pollutants", 
                      "extr_var", "extr_lag", "extr_rr", "extr_lci","extr_uci",
                      "extr1_var", "extr1_lag", "extr1_rr", "extr1_lci","extr1_uci",
                      "extr2_var", "extr2_lag", "extr2_rr", "extr2_lci","extr2_uci")
table1[,1] <- c("NO2","PM10","PM25", "03")

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



load("C:\\Users\\u271201\\Downloads\\output\\Mort_Male_80_\\model0.6.RData")
model1 <- model
load("C:\\Users\\u271201\\Downloads\\output\\Mort_Male_80_\\model0.7.RData")
model2 <- model
load("C:\\Users\\u271201\\Downloads\\output\\Mort_Male_80_\\model0.8.RData")
model3 <- model
load("C:\\Users\\u271201\\Downloads\\output\\Mort_Male_80_\\model0.9.RData")
model4 <- model 

mod.name <- c("model1", "model2", "model3", "model4")

table2 <- as.data.frame(matrix(NA, 4, 16))
colnames(table2) <- c("Pollutants", 
                      "extr_var", "extr_lag", "extr_rr", "extr_lci","extr_uci",
                      "extr1_var", "extr1_lag", "extr1_rr", "extr1_lci","extr1_uci",
                      "extr2_var", "extr2_lag", "extr2_rr", "extr2_lci","extr2_uci")
table2[,1] <- c("NO2","PM10","PM25", "03")

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
load("C:\\Users\\u271201\\Downloads\\output\\Mort_tot_80_\\model0.6.RData")
model1 <- model
load("C:\\Users\\u271201\\Downloads\\output\\Mort_tot_80_\\model0.7.RData")
model2 <- model
load("C:\\Users\\u271201\\Downloads\\output\\Mort_tot_80_\\model0.8.RData")
model3 <- model
load("C:\\Users\\u271201\\Downloads\\output\\Mort_tot_80_\\model0.9.RData")
model4 <- model 

mod.name <- c("model1", "model2", "model3", "model4")

table3 <- as.data.frame(matrix(NA, 4, 16))
colnames(table3) <- c("Pollutants", 
                      "extr_var", "extr_lag", "extr_rr", "extr_lci","extr_uci",
                      "extr1_var", "extr1_lag", "extr1_rr", "extr1_lci","extr1_uci",
                      "extr2_var", "extr2_lag", "extr2_rr", "extr2_lci","extr2_uci")
table3[,1] <- c("NO2","PM10","PM25", "03")

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
  
  
  
  table3$extr_var[j] <- extr$var[extr_ind]
  table3$extr_lag[j] <- round(extr$lag[extr_ind],0)
  table3$extr_rr[j]  <- round(extr$rr[extr_ind], 2)
  table3$extr_lci[j] <- round(extr$rr.lci[extr_ind], 2)
  table3$extr_uci[j] <- round(extr$rr.uci[extr_ind], 2)
  table3$extr1_var[j] <- extr1$var[extr1_ind]
  table3$extr1_lag[j] <- round(extr1$lag[extr1_ind], 0)
  table3$extr1_rr[j]  <- round(extr1$rr[extr1_ind], 2)
  table3$extr1_lci[j] <- round(extr1$rr.lci[extr1_ind], 2)
  table3$extr1_uci[j] <- round(extr1$rr.uci[extr1_ind], 2)
  table3$extr2_var[j] <- extr2$var[extr2_ind]
  table3$extr2_lag[j] <- round(extr2$lag[extr2_ind], 0)
  table3$extr2_rr[j]  <- round(extr2$rr[extr2_ind], 2)
  table3$extr2_lci[j] <- round(extr2$rr.lci[extr2_ind], 2)
  table3$extr2_uci[j] <- round(extr2$rr.uci[extr2_ind], 2)
  
}


# Add a new column to each table to identify the group
table1$Gender <- "Female"
table2$Gender <- "Male"
table3$Gender <- "Total"

# Combine the tables
combined_table <- rbind(table1, table2, table3)

# If you need to reorder columns so 'Gender' appears first for better readability
combined_table <- combined_table[c("Gender", names(combined_table)[!names(combined_table) %in% "Gender"])]

# Print the combined table to check
print(combined_table)

# Optionally, you can also sort the table by Setting and Gender if needed
combined_table80 <- combined_table[order(combined_table$Pollutants, combined_table$Gender),]

# Print the sorted table to check
print(combined_table80)



################################################################################
                             #Table 65-79#
################################################################################
load("C:\\Users\\u271201\\Downloads\\output\\Mort_Female_65_79\\model0.6.RData")
model1 <- model
load("C:\\Users\\u271201\\Downloads\\output\\Mort_Female_65_79\\model0.7.RData")
model2 <- model
load("C:\\Users\\u271201\\Downloads\\output\\Mort_Female_65_79\\model0.8.RData")
model3 <- model
load("C:\\Users\\u271201\\Downloads\\output\\Mort_Female_65_79\\model0.9.RData")
model4 <- model 

mod.name <- c("model1", "model2", "model3", "model4")

table1 <- as.data.frame(matrix(NA, 4, 16))
colnames(table1) <- c("Pollutants", 
                      "extr_var", "extr_lag", "extr_rr", "extr_lci","extr_uci",
                      "extr1_var", "extr1_lag", "extr1_rr", "extr1_lci","extr1_uci",
                      "extr2_var", "extr2_lag", "extr2_rr", "extr2_lci","extr2_uci")
table1[,1] <- c("NO2","PM10","PM25", "03")

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



load("C:\\Users\\u271201\\Downloads\\output\\Mort_Male_65_79\\model0.6.RData")
model1 <- model
load("C:\\Users\\u271201\\Downloads\\output\\Mort_Male_65_79\\model0.7.RData")
model2 <- model
load("C:\\Users\\u271201\\Downloads\\output\\Mort_Male_65_79\\model0.8.RData")
model3 <- model
load("C:\\Users\\u271201\\Downloads\\output\\Mort_Male_65_79\\model0.9.RData")
model4 <- model 

mod.name <- c("model1", "model2", "model3", "model4")

table2 <- as.data.frame(matrix(NA, 4, 16))
colnames(table2) <- c("Pollutants", 
                      "extr_var", "extr_lag", "extr_rr", "extr_lci","extr_uci",
                      "extr1_var", "extr1_lag", "extr1_rr", "extr1_lci","extr1_uci",
                      "extr2_var", "extr2_lag", "extr2_rr", "extr2_lci","extr2_uci")
table2[,1] <- c("NO2","PM10","PM25", "03")

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
load("C:\\Users\\u271201\\Downloads\\output\\Mort_tot_65_79\\model0.6.RData")
model1 <- model
load("C:\\Users\\u271201\\Downloads\\output\\Mort_tot_65_79\\model0.7.RData")
model2 <- model
load("C:\\Users\\u271201\\Downloads\\output\\Mort_tot_65_79\\model0.8.RData")
model3 <- model
load("C:\\Users\\u271201\\Downloads\\output\\Mort_tot_65_79\\model0.9.RData")
model4 <- model 

mod.name <- c("model1", "model2", "model3", "model4")

table3 <- as.data.frame(matrix(NA, 4, 16))
colnames(table3) <- c("Pollutants", 
                      "extr_var", "extr_lag", "extr_rr", "extr_lci","extr_uci",
                      "extr1_var", "extr1_lag", "extr1_rr", "extr1_lci","extr1_uci",
                      "extr2_var", "extr2_lag", "extr2_rr", "extr2_lci","extr2_uci")
table3[,1] <- c("NO2","PM10","PM25", "03")

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
  
  
  
  table3$extr_var[j] <- extr$var[extr_ind]
  table3$extr_lag[j] <- round(extr$lag[extr_ind],0)
  table3$extr_rr[j]  <- round(extr$rr[extr_ind], 2)
  table3$extr_lci[j] <- round(extr$rr.lci[extr_ind], 2)
  table3$extr_uci[j] <- round(extr$rr.uci[extr_ind], 2)
  table3$extr1_var[j] <- extr1$var[extr1_ind]
  table3$extr1_lag[j] <- round(extr1$lag[extr1_ind], 0)
  table3$extr1_rr[j]  <- round(extr1$rr[extr1_ind], 2)
  table3$extr1_lci[j] <- round(extr1$rr.lci[extr1_ind], 2)
  table3$extr1_uci[j] <- round(extr1$rr.uci[extr1_ind], 2)
  table3$extr2_var[j] <- extr2$var[extr2_ind]
  table3$extr2_lag[j] <- round(extr2$lag[extr2_ind], 0)
  table3$extr2_rr[j]  <- round(extr2$rr[extr2_ind], 2)
  table3$extr2_lci[j] <- round(extr2$rr.lci[extr2_ind], 2)
  table3$extr2_uci[j] <- round(extr2$rr.uci[extr2_ind], 2)
  
}


# Add a new column to each table to identify the group
table1$Gender <- "Female"
table2$Gender <- "Male"
table3$Gender <- "Total"

# Combine the tables
combined_table <- rbind(table1, table2, table3)

# If you need to reorder columns so 'Gender' appears first for better readability
combined_table <- combined_table[c("Gender", names(combined_table)[!names(combined_table) %in% "Gender"])]

# Print the combined table to check
print(combined_table)

# Optionally, you can also sort the table by Setting and Gender if needed
combined_table65 <- combined_table[order(combined_table$Pollutants, combined_table$Gender),]

# Print the sorted table to check
print(combined_table65)
print(combined_table80)

################################################################################
################################################################################
                               #Combined#                                       
################################################################################
# Assuming 'combined_table' is already loaded into your R session

combined_data <- left_join(combined_table65, combined_table80, by = c("Gender", "Pollutants"))



long_data <- combined_data %>%
  pivot_longer(
    cols = matches("extr[0-9]?_(var|lag|rr|lci|uci)\\.(x|y)"),
    names_to = c("group", "measure", "Group of age"),
    names_pattern = "(extr[0-9]?|extr[0-9]?|extr[0-9]?)_(var|lag|rr|lci|uci)\\.(x|y)",
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

# Removing the 'group' column
long_data <- long_data %>%
  select(-group,-lag)  # Drops the 'group' column

Table_results <- long_data %>%
  mutate(
    `Group of age` = case_when(
      `Group of age` == "x" ~ "65-79",
      `Group of age` == "y" ~ "80",
      TRUE ~ `Group of age`  # Catch-all in case there are other unexpected values
    ))
write.csv(Table_results, "C:\\Users\\u271201\\Downloads\\Figures analysis\\Pollutants\\Tables\\RR Pollutants.csv", row.names = FALSE)

###############################################################################
                    #Fugures Pollutants#
###############################################################################

################################################################################
"Cumulative Expose response by temperature 65-79 and 80"
################################################################################
load("C:/Users/u271201/Downloads/output/Mort_tot_65_79/model0.6.RData")
model1 <- model
load("C:/Users/u271201/Downloads/output/Mort_tot_65_79/model0.7.RData")
model2 <- model
load("C:/Users/u271201/Downloads/output/Mort_tot_65_79/model0.8.RData")
model3 <- model
load("C:/Users/u271201/Downloads/output/Mort_tot_65_79/model0.9.RData")
model4 <- model
load("C:/Users/u271201/Downloads/output/Mort_tot_80_/model0.6.RData")
model5 <- model
load("C:/Users/u271201/Downloads/output/Mort_tot_80_/model0.7.RData")
model6 <- model
load("C:/Users/u271201/Downloads/output/Mort_tot_80_/model0.8.RData")
model7 <- model
load("C:/Users/u271201/Downloads/output/Mort_tot_80_/model0.9.RData")
model8 <- model

models <- list(model1, model2, model3, model4,model5, model6, model7, model8)

model_names <- c("NO2_65-79","PM10_65-79","PM25_65-79", "03_65-79","NO2_80+","PM10_80+","PM25_80+", "03_80+") # Define a vector of model names
colors <- c("black","blue", "red", "green", "orange", "yellow", "purple", "beige") 
# Initialize an empty list to store the predictions
pred_list <- list()

# Loop through the model file paths
for (i in seq_along(models)) {
  # Load the model
  model <- models[[i]]
  
  # Assume 'model' is the object that gets loaded and contains the DLNM model results
  # Also assume that 'basis_MaxTm' is already present in your environment and is the correct crossbasis object
  
  # Extract coefficients and variance-covariance matrix
  coef <- model$summary.fixed$mean
  vcov <- model$misc$lincomb.derived.covariance.matrix
  
  # Select the position of the terms associated with Tmax crossbasis
  indt <- grep("basis_MaxTm", model$names.fixed)
  
  # Extract predictions from the Tmax DLNM centered on a specified value (replace with your value)
  pred <- crosspred(basis_MaxTm, coef = coef[indt], vcov = vcov[indt, indt],
                    model.link = "log", bylag = 0.25, cen = 34) # Replace 32 with your desired centering temperature
  
  # Store the predictions in the list
  pred_list[[model_names[i]]] <- pred
}

pdf("C:/Users/u271201/Downloads/combined_plot.pdf", width = 12, height = 6)

# First, set up the plot with an initial range that will encompass all models


# Start the plot with the first model to set up axes, labels, etc.
plot(pred_list[[1]], "overall", xlab = expression(paste("Temperature (",degree,"C)")), 
     ylab = "Relative risk", type = 'n')

# Now add lines for each model
for (i in seq_along(pred_list)) {
  lines(pred_list[[i]]$predvar, pred_list[[i]]$allRRfit, col = colors[i], lwd = 2)
}

# Add a legend to the plot
legend("topleft", legend = model_names, col = colors, lwd = 1, cex = 0.8)

dev.off()


################################################################################
"Expose response by lag 65-79 and 80"
################################################################################

model_paths <- c(
  "C:\\Users\\u271201\\Downloads\\output\\Mort_Female_65_79\\model0.6.RData",
  "C:\\Users\\u271201\\Downloads\\output\\Mort_Female_65_79\\model0.7.RData",
  "C:\\Users\\u271201\\Downloads\\output\\Mort_Female_65_79\\model0.8.RData",
  "C:\\Users\\u271201\\Downloads\\output\\Mort_Female_65_79\\model0.9.RData"
)
model_names <- c("NO2","PM10","PM25", "03") # Define a vector of model names

# Initialize an empty list to store predictions
pred_list <- list()

# Loop through each model file
for (i in seq_along(model_paths)) {
  # Load the model
  load(model_paths[i])
  
  # extract full coef and vcov and create indicators for each term
  coef <- model$summary.fixed$mean
  vcov <- model$misc$lincomb.derived.covariance.matrix
  
  # find position of the terms associated with Tmin crossbasis
  indt <- grep("basis_MaxTm", model$names.fixed)
  
  # extract predictions from the Tmin DLNM centred on overall mean Tmin (19 deg C)
  predt <- crosspred(basis_MaxTm, coef = coef[indt], vcov=vcov[indt,indt],
                     model.link = "log", bylag = 0.25, cen = 34) 
  
  pred_list[[i]] <- predt
  # contour and scenario plots for Tmin (Main text Fig 3)
  
  # contour plot of exposure-lag-response associations (Main text Fig 3a)
  pdf(paste0("C:\\Users\\u271201\\Downloads\\Figures analysis\\Pollutants\\Figures\\Female_65\\Female_65_79", model_names[i], ".pdf"), width = 6.5, height = 6)
  
  y <- predt$predvar
  x <- seq(0, nlag, 0.25)
  z <- t(predt$matRRfit)
  
  pal <- rev(brewer.pal(11, "PRGn"))
  levels <- pretty(z, 20)
  col1 <- colorRampPalette(pal[1:6])
  col2 <- colorRampPalette(pal[6:11])
  cols <- c(col1(sum(levels < 1)), col2(sum(levels > 1)))
  
  filled.contour(x,y,z,
                 xlab = "Lag", ylab = expression(paste("Temperature (",degree,"C)")), main = model_names[i],
                 col = cols,levels = levels,
                 plot.axes = { axis(1, at = 0:nlag, c(0:nlag)) 
                   axis(2)})
  
  
  dev.off()
  
  # lag response for different Tmin scenarios (Main text Fig 3b)
  pdf(paste0("C:\\Users\\u271201\\Downloads\\Figures analysis\\Pollutants\\Figures\\Female_65\\Female_65_79", model_names[i], ".pdf"), width = 6.5, height = 6)
  
  # get exposures values
  vars <- predt$predvar
  
  # obtain relative risk (RR) fit and upper and lower confidence limits for all exposure variables
  rr <- predt$matRRfit
  rr.lci <- predt$matRRlow
  rr.uci <- predt$matRRhigh
  
  # set relative risk range 
  r1 <- min(range(rr, rr.lci, rr.uci))
  r2 <- max(range(rr, rr.lci, rr.uci))
  
  # get selected exposure variable positions
mn <- which(round(vars, 2) == 30)
mx <- which(round(vars, 2) == 35)
mx2 <- which(round(vars, 2) == 40)

  
  # define colours
  col1 <- brewer.pal(11, "RdBu")[9]
  tcol1 <- do.call(rgb, c(as.list(col2rgb(col1)), alpha = 255/4, max = 255))
  
  col2 <- brewer.pal(11, "RdBu")[3]
  tcol2 <- do.call(rgb, c(as.list(col2rgb(col2)), alpha = 255/4, max = 255))
  
  col3 <- brewer.pal(11, "RdBu")[1]
  tcol3 <- do.call(rgb, c(as.list(col2rgb(col3)), alpha = 255/4, max = 255))
  
  # define x values (lag, by lag)
  lagbylag <- seq(0, nlag, 0.25)
  
  # cool
  plot(lagbylag, rr[mn,], col = col1, type = "l", lwd = 1, 
       xlab = "Lag", ylab = "Relative risk", main = model_names[i], 
       ylim = range(r1, r2*1.11), frame.plot = T, axes = F)
  axis(1, at = 0:nlag, labels = 0:nlag)
  axis(2)
  xx <- c(lagbylag, rev(lagbylag))
  yy <- c(rr.lci[mn,], rev(rr.uci[mn,]))
  polygon(xx, yy, col = tcol1, border = tcol1)
  # warm
  lines(lagbylag, rr[mx,], col = col2, lwd = 1)
  xx <- c(lagbylag, rev(lagbylag))
  yy <- c(rr.lci[mx,], rev(rr.uci[mx,]))
  polygon(xx, yy, col = tcol2, border = tcol2)
  abline(h = 1, lty = 3)
  # warmest
  lines(lagbylag, rr[mx2,], col = col3, lwd = 1)
  xx <- c(lagbylag, rev(lagbylag))
  yy <- c(rr.lci[mx2,],rev(rr.uci[mx2,]))
  polygon(xx, yy, col = tcol3, border = tcol3)
  abline(h = 1, lty = 3)
  
  legend("topleft",
         legend = c(paste0("Tmin = ",vars[mn]," deg C"),
                    paste0("Tmin = ", vars[mx]," deg C"),
                    paste0("Tmin = ", vars[mx2]," deg C")),
         col = c(col1, col2, col3), 
         lwd = 2, lty = 1, bty = "n", 
         y.intersp = 1.5, horiz = F)
  
  
  dev.off()
  write.csv(data.frame(x = x, y = rep(y, each = length(x)), z = as.vector(z)),
            file = paste0("C:\\Users\\u271201\\Downloads\\Figures analysis\\Pollutants\\Tables\\data_extract_Female_65", model_names[i], ".csv"), row.names = FALSE)
  
}


###############################################################################

model_paths <- c(
  "C:\\Users\\u271201\\Downloads\\output\\Mort_Male_65_79\\model0.6.RData",
  "C:\\Users\\u271201\\Downloads\\output\\Mort_Male_65_79\\model0.7.RData",
  "C:\\Users\\u271201\\Downloads\\output\\Mort_Male_65_79\\model0.8.RData",
  "C:\\Users\\u271201\\Downloads\\output\\Mort_Male_65_79\\model0.9.RData"
)
model_names <- c("NO2","PM10","PM25", "03") # Define a vector of model names

# Initialize an empty list to store predictions
pred_list <- list()

# Loop through each model file
for (i in seq_along(model_paths)) {
  # Load the model
  load(model_paths[i])
  
  # extract full coef and vcov and create indicators for each term
  coef <- model$summary.fixed$mean
  vcov <- model$misc$lincomb.derived.covariance.matrix
  
  # find position of the terms associated with Tmin crossbasis
  indt <- grep("basis_MaxTm", model$names.fixed)
  
  # extract predictions from the Tmin DLNM centred on overall mean Tmin (19 deg C)
  predt <- crosspred(basis_MaxTm, coef = coef[indt], vcov=vcov[indt,indt],
                     model.link = "log", bylag = 0.25, cen = 34) 
  
  pred_list[[i]] <- predt
  # contour and scenario plots for Tmin (Main text Fig 3)
  
  # contour plot of exposure-lag-response associations (Main text Fig 3a)
  pdf(paste0("C:\\Users\\u271201\\Downloads\\Figures analysis\\Pollutants\\Figures\\Male_65\\Male_65_79", model_names[i], ".pdf"), width = 6.5, height = 6)
  
  y <- predt$predvar
  x <- seq(0, nlag, 0.25)
  z <- t(predt$matRRfit)
  
  pal <- rev(brewer.pal(11, "PRGn"))
  levels <- pretty(z, 20)
  col1 <- colorRampPalette(pal[1:6])
  col2 <- colorRampPalette(pal[6:11])
  cols <- c(col1(sum(levels < 1)), col2(sum(levels > 1)))
  
  filled.contour(x,y,z,
                 xlab = "Lag", ylab = expression(paste("Temperature (",degree,"C)")), main = model_names[i],
                 col = cols,levels = levels,
                 plot.axes = { axis(1, at = 0:nlag, c(0:nlag)) 
                   axis(2)})
  
  
  dev.off()
  
  # lag response for different Tmin scenarios (Main text Fig 3b)
  pdf(paste0("C:\\Users\\u271201\\Downloads\\Figures analysis\\Pollutants\\Figures\\Male_65\\Male_65_79", model_names[i], ".pdf"), width = 6.5, height = 6)
  
  # get exposures values
  vars <- predt$predvar
  
  # obtain relative risk (RR) fit and upper and lower confidence limits for all exposure variables
  rr <- predt$matRRfit
  rr.lci <- predt$matRRlow
  rr.uci <- predt$matRRhigh
  
  # set relative risk range 
  r1 <- min(range(rr, rr.lci, rr.uci))
  r2 <- max(range(rr, rr.lci, rr.uci))
  
  # get selected exposure variable positions
  mn <- which(round(vars, 2) == 30)
  mx <- which(round(vars, 2) == 35)
  mx2 <- which(round(vars, 2) == 40)
  
  
  # define colours
  col1 <- brewer.pal(11, "RdBu")[9]
  tcol1 <- do.call(rgb, c(as.list(col2rgb(col1)), alpha = 255/4, max = 255))
  
  col2 <- brewer.pal(11, "RdBu")[3]
  tcol2 <- do.call(rgb, c(as.list(col2rgb(col2)), alpha = 255/4, max = 255))
  
  col3 <- brewer.pal(11, "RdBu")[1]
  tcol3 <- do.call(rgb, c(as.list(col2rgb(col3)), alpha = 255/4, max = 255))
  
  # define x values (lag, by lag)
  lagbylag <- seq(0, nlag, 0.25)
  
  # cool
  plot(lagbylag, rr[mn,], col = col1, type = "l", lwd = 1, 
       xlab = "Lag", ylab = "Relative risk", main = model_names[i], 
       ylim = range(r1, r2*1.11), frame.plot = T, axes = F)
  axis(1, at = 0:nlag, labels = 0:nlag)
  axis(2)
  xx <- c(lagbylag, rev(lagbylag))
  yy <- c(rr.lci[mn,], rev(rr.uci[mn,]))
  polygon(xx, yy, col = tcol1, border = tcol1)
  # warm
  lines(lagbylag, rr[mx,], col = col2, lwd = 1)
  xx <- c(lagbylag, rev(lagbylag))
  yy <- c(rr.lci[mx,], rev(rr.uci[mx,]))
  polygon(xx, yy, col = tcol2, border = tcol2)
  abline(h = 1, lty = 3)
  # warmest
  lines(lagbylag, rr[mx2,], col = col3, lwd = 1)
  xx <- c(lagbylag, rev(lagbylag))
  yy <- c(rr.lci[mx2,],rev(rr.uci[mx2,]))
  polygon(xx, yy, col = tcol3, border = tcol3)
  abline(h = 1, lty = 3)
  
  legend("topleft",
         legend = c(paste0("Tmin = ",vars[mn]," deg C"),
                    paste0("Tmin = ", vars[mx]," deg C"),
                    paste0("Tmin = ", vars[mx2]," deg C")),
         col = c(col1, col2, col3), 
         lwd = 2, lty = 1, bty = "n", 
         y.intersp = 1.5, horiz = F)
  
  
  dev.off()
  
  write.csv(data.frame(x = x, y = rep(y, each = length(x)), z = as.vector(z)),
            file = paste0("C:\\Users\\u271201\\Downloads\\Figures analysis\\Pollutants\\Tables\\data_extract_Male_65", model_names[i], ".csv"), row.names = FALSE)
  
}

################################################################################
model_paths <- c(
  "C:\\Users\\u271201\\Downloads\\output\\Mort_Female_80_\\model0.6.RData",
  "C:\\Users\\u271201\\Downloads\\output\\Mort_Female_80_\\model0.7.RData",
  "C:\\Users\\u271201\\Downloads\\output\\Mort_Female_80_\\model0.8.RData",
  "C:\\Users\\u271201\\Downloads\\output\\Mort_Female_80_\\model0.9.RData"
)
model_names <- c("NO2","PM10","PM25", "03") # Define a vector of model names

# Initialize an empty list to store predictions
pred_list <- list()

# Loop through each model file
for (i in seq_along(model_paths)) {
  # Load the model
  load(model_paths[i])
  
  # extract full coef and vcov and create indicators for each term
  coef <- model$summary.fixed$mean
  vcov <- model$misc$lincomb.derived.covariance.matrix
  
  # find position of the terms associated with Tmin crossbasis
  indt <- grep("basis_MaxTm", model$names.fixed)
  
  # extract predictions from the Tmin DLNM centred on overall mean Tmin (19 deg C)
  predt <- crosspred(basis_MaxTm, coef = coef[indt], vcov=vcov[indt,indt],
                     model.link = "log", bylag = 0.25, cen = 34) 
  
  pred_list[[i]] <- predt
  # contour and scenario plots for Tmin (Main text Fig 3)
  
  # contour plot of exposure-lag-response associations (Main text Fig 3a)
  pdf(paste0("C:\\Users\\u271201\\Downloads\\Figures analysis\\Pollutants\\Figures\\Female_80\\Female_80", model_names[i], ".pdf"), width = 6.5, height = 6)
  
  y <- predt$predvar
  x <- seq(0, nlag, 0.25)
  z <- t(predt$matRRfit)
  
  pal <- rev(brewer.pal(11, "PRGn"))
  levels <- pretty(z, 20)
  col1 <- colorRampPalette(pal[1:6])
  col2 <- colorRampPalette(pal[6:11])
  cols <- c(col1(sum(levels < 1)), col2(sum(levels > 1)))
  
  filled.contour(x,y,z,
                 xlab = "Lag", ylab = expression(paste("Temperature (",degree,"C)")), main = model_names[i],
                 col = cols,levels = levels,
                 plot.axes = { axis(1, at = 0:nlag, c(0:nlag)) 
                   axis(2)})
  
  
  dev.off()
  
  # lag response for different Tmin scenarios (Main text Fig 3b)
  pdf(paste0("C:\\Users\\u271201\\Downloads\\Figures analysis\\Pollutants\\Figures\\Female_80\\Female_80", model_names[i], ".pdf"), width = 6.5, height = 6)
  
  # get exposures values
  vars <- predt$predvar
  
  # obtain relative risk (RR) fit and upper and lower confidence limits for all exposure variables
  rr <- predt$matRRfit
  rr.lci <- predt$matRRlow
  rr.uci <- predt$matRRhigh
  
  # set relative risk range 
  r1 <- min(range(rr, rr.lci, rr.uci))
  r2 <- max(range(rr, rr.lci, rr.uci))
  
  # get selected exposure variable positions
  mn <- which(round(vars, 2) == 30)
  mx <- which(round(vars, 2) == 35)
  mx2 <- which(round(vars, 2) == 40)
  
  
  # define colours
  col1 <- brewer.pal(11, "RdBu")[9]
  tcol1 <- do.call(rgb, c(as.list(col2rgb(col1)), alpha = 255/4, max = 255))
  
  col2 <- brewer.pal(11, "RdBu")[3]
  tcol2 <- do.call(rgb, c(as.list(col2rgb(col2)), alpha = 255/4, max = 255))
  
  col3 <- brewer.pal(11, "RdBu")[1]
  tcol3 <- do.call(rgb, c(as.list(col2rgb(col3)), alpha = 255/4, max = 255))
  
  # define x values (lag, by lag)
  lagbylag <- seq(0, nlag, 0.25)
  
  # cool
  plot(lagbylag, rr[mn,], col = col1, type = "l", lwd = 1, 
       xlab = "Lag", ylab = "Relative risk", main = model_names[i], 
       ylim = range(r1, r2*1.11), frame.plot = T, axes = F)
  axis(1, at = 0:nlag, labels = 0:nlag)
  axis(2)
  xx <- c(lagbylag, rev(lagbylag))
  yy <- c(rr.lci[mn,], rev(rr.uci[mn,]))
  polygon(xx, yy, col = tcol1, border = tcol1)
  # warm
  lines(lagbylag, rr[mx,], col = col2, lwd = 1)
  xx <- c(lagbylag, rev(lagbylag))
  yy <- c(rr.lci[mx,], rev(rr.uci[mx,]))
  polygon(xx, yy, col = tcol2, border = tcol2)
  abline(h = 1, lty = 3)
  # warmest
  lines(lagbylag, rr[mx2,], col = col3, lwd = 1)
  xx <- c(lagbylag, rev(lagbylag))
  yy <- c(rr.lci[mx2,],rev(rr.uci[mx2,]))
  polygon(xx, yy, col = tcol3, border = tcol3)
  abline(h = 1, lty = 3)
  
  legend("topleft",
         legend = c(paste0("Tmin = ",vars[mn]," deg C"),
                    paste0("Tmin = ", vars[mx]," deg C"),
                    paste0("Tmin = ", vars[mx2]," deg C")),
         col = c(col1, col2, col3), 
         lwd = 2, lty = 1, bty = "n", 
         y.intersp = 1.5, horiz = F)
  
  
  dev.off()
  write.csv(data.frame(x = x, y = rep(y, each = length(x)), z = as.vector(z)),
            file = paste0("C:\\Users\\u271201\\Downloads\\Figures analysis\\Pollutants\\Tables\\data_extract_Female_80", model_names[i], ".csv"), row.names = FALSE)
  
}


###############################################################################

model_paths <- c(
  "C:\\Users\\u271201\\Downloads\\output\\Mort_Male_80_\\model0.6.RData",
  "C:\\Users\\u271201\\Downloads\\output\\Mort_Male_80_\\model0.7.RData",
  "C:\\Users\\u271201\\Downloads\\output\\Mort_Male_80_\\model0.8.RData",
  "C:\\Users\\u271201\\Downloads\\output\\Mort_Male_80_\\model0.9.RData"
)
model_names <- c("NO2","PM10","PM25", "03") # Define a vector of model names

# Initialize an empty list to store predictions
pred_list <- list()

# Loop through each model file
for (i in seq_along(model_paths)) {
  # Load the model
  load(model_paths[i])
  
  # extract full coef and vcov and create indicators for each term
  coef <- model$summary.fixed$mean
  vcov <- model$misc$lincomb.derived.covariance.matrix
  
  # find position of the terms associated with Tmin crossbasis
  indt <- grep("basis_MaxTm", model$names.fixed)
  
  # extract predictions from the Tmin DLNM centred on overall mean Tmin (19 deg C)
  predt <- crosspred(basis_MaxTm, coef = coef[indt], vcov=vcov[indt,indt],
                     model.link = "log", bylag = 0.25, cen = 34) 
  
  pred_list[[i]] <- predt
  # contour and scenario plots for Tmin (Main text Fig 3)
  
  # contour plot of exposure-lag-response associations (Main text Fig 3a)
  pdf(paste0("C:\\Users\\u271201\\Downloads\\Figures analysis\\Pollutants\\Figures\\Male_80\\Male80", model_names[i], ".pdf"), width = 6.5, height = 6)
  
  y <- predt$predvar
  x <- seq(0, nlag, 0.25)
  z <- t(predt$matRRfit)
  
  pal <- rev(brewer.pal(11, "PRGn"))
  levels <- pretty(z, 20)
  col1 <- colorRampPalette(pal[1:6])
  col2 <- colorRampPalette(pal[6:11])
  cols <- c(col1(sum(levels < 1)), col2(sum(levels > 1)))
  
  filled.contour(x,y,z,
                 xlab = "Lag", ylab = expression(paste("Temperature (",degree,"C)")), main = model_names[i],
                 col = cols,levels = levels,
                 plot.axes = { axis(1, at = 0:nlag, c(0:nlag)) 
                   axis(2)})
  
  
  dev.off()
  
  # lag response for different Tmin scenarios (Main text Fig 3b)
  pdf(paste0("C:\\Users\\u271201\\Downloads\\Figures analysis\\Pollutants\\Figures\\Male_80\\Male80", model_names[i], ".pdf"), width = 6.5, height = 6)
  
  # get exposures values
  vars <- predt$predvar
  
  # obtain relative risk (RR) fit and upper and lower confidence limits for all exposure variables
  rr <- predt$matRRfit
  rr.lci <- predt$matRRlow
  rr.uci <- predt$matRRhigh
  
  # set relative risk range 
  r1 <- min(range(rr, rr.lci, rr.uci))
  r2 <- max(range(rr, rr.lci, rr.uci))
  
  # get selected exposure variable positions
  mn <- which(round(vars, 2) == 30)
  mx <- which(round(vars, 2) == 35)
  mx2 <- which(round(vars, 2) == 40)
  
  
  # define colours
  col1 <- brewer.pal(11, "RdBu")[9]
  tcol1 <- do.call(rgb, c(as.list(col2rgb(col1)), alpha = 255/4, max = 255))
  
  col2 <- brewer.pal(11, "RdBu")[3]
  tcol2 <- do.call(rgb, c(as.list(col2rgb(col2)), alpha = 255/4, max = 255))
  
  col3 <- brewer.pal(11, "RdBu")[1]
  tcol3 <- do.call(rgb, c(as.list(col2rgb(col3)), alpha = 255/4, max = 255))
  
  # define x values (lag, by lag)
  lagbylag <- seq(0, nlag, 0.25)
  
  # cool
  plot(lagbylag, rr[mn,], col = col1, type = "l", lwd = 1, 
       xlab = "Lag", ylab = "Relative risk", main = model_names[i], 
       ylim = range(r1, r2*1.11), frame.plot = T, axes = F)
  axis(1, at = 0:nlag, labels = 0:nlag)
  axis(2)
  xx <- c(lagbylag, rev(lagbylag))
  yy <- c(rr.lci[mn,], rev(rr.uci[mn,]))
  polygon(xx, yy, col = tcol1, border = tcol1)
  # warm
  lines(lagbylag, rr[mx,], col = col2, lwd = 1)
  xx <- c(lagbylag, rev(lagbylag))
  yy <- c(rr.lci[mx,], rev(rr.uci[mx,]))
  polygon(xx, yy, col = tcol2, border = tcol2)
  abline(h = 1, lty = 3)
  # warmest
  lines(lagbylag, rr[mx2,], col = col3, lwd = 1)
  xx <- c(lagbylag, rev(lagbylag))
  yy <- c(rr.lci[mx2,],rev(rr.uci[mx2,]))
  polygon(xx, yy, col = tcol3, border = tcol3)
  abline(h = 1, lty = 3)
  
  legend("topleft",
         legend = c(paste0("Tmin = ",vars[mn]," deg C"),
                    paste0("Tmin = ", vars[mx]," deg C"),
                    paste0("Tmin = ", vars[mx2]," deg C")),
         col = c(col1, col2, col3), 
         lwd = 2, lty = 1, bty = "n", 
         y.intersp = 1.5, horiz = F)
  
  
  dev.off()
  
  write.csv(data.frame(x = x, y = rep(y, each = length(x)), z = as.vector(z)),
            file = paste0("C:\\Users\\u271201\\Downloads\\Figures analysis\\Pollutants\\Tables\\data_extract_Male_80", model_names[i], ".csv"), row.names = FALSE)
  
}