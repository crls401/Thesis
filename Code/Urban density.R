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
colnames(table1) <- c("Setting", 
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
                     model.link = "log", bylag = 0.1, cen = 21)
  
  
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
colnames(table2) <- c("Setting", 
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
                     model.link = "log", bylag = 0.1, cen = 21)
  
  
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
colnames(table3) <- c("Setting", 
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
                     model.link = "log", bylag = 0.1, cen = 21)
  
  
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
combined_table80 <- combined_table[order(combined_table$Setting, combined_table$Gender),]

# Print the sorted table to check
print(combined_table80)