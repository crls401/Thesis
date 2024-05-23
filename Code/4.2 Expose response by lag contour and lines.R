
#Select the hydrometeorological  model (without interactions) 
model <- model4

# extract full coef and vcov and create indicators for each term
coef <- model$summary.fixed$mean
vcov <- model$misc$lincomb.derived.covariance.matrix

# find position of the terms associated with Tmin crossbasis
indt <- grep("basis_MaxTm", model$names.fixed)

# extract predictions from the Tmin DLNM centred on overall mean Tmin (19 deg C)
predt <- crosspred(basis_MaxTm, coef = coef[indt], vcov=vcov[indt,indt],
                   model.link = "log", bylag = 0.25, cen = 32) 

# contour and scenario plots for Tmin (Main text Fig 3)

# contour plot of exposure-lag-response associations (Main text Fig 3a)
pdf("C:\\Users\\u271201\\Downloads\\fig_03a_TMAX.pdf", width = 6.5, height = 6)

y <- predt$predvar
x <- seq(0, nlag, 0.25)
z <- t(predt$matRRfit)

pal <- rev(brewer.pal(11, "PRGn"))
levels <- pretty(z, 20)
col1 <- colorRampPalette(pal[1:6])
col2 <- colorRampPalette(pal[6:11])
cols <- c(col1(sum(levels < 1)), col2(sum(levels > 1)))

filled.contour(x,y,z,
               xlab = "Lag", ylab = expression(paste("Temperature (",degree,"C)")), main = "",
               col = cols,levels = levels,
               plot.axes = { axis(1, at = 0:nlag, c(0:nlag)) 
                 axis(2)})
mtext(side = 2, at = max(y)*1.1, text = "a", las = 2, cex = 1.2, line = 2)

dev.off()

# lag response for different Tmin scenarios (Main text Fig 3b)
pdf("C:\\Users\\u271201\\Downloads\\fig_03b_TMAX.pdf", width = 6, height = 6)

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
mn <- which(round(vars, 2) == 35)
mx <- which(round(vars, 2) == 40)
mx2 <- which(round(vars, 2) == 42)

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
     xlab = "Lag", ylab = "Relative risk", main = "", 
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

mtext(side = 2, at = r2*1.25, text = "b", las = 2, cex = 1.2, line = 2)

dev.off()


#######################Combined lines###########################################


pdf("C:/Users/u271201/Downloads/fig_05_pdsi_scenariO3.pdf", width = 12, height = 6)   
model_paths <- c("C:\\Users\\u271201\\Downloads\\output\\Mort_Female_65_\\model0.1.RData",
  "C:\\Users\\u271201\\Downloads\\output\\Mort_Male_65_\\model0.1.RData")

# Initialize an empty list to store the predictions
pred_list <- list()

# Loop through the model file paths
for (i in seq_along(model_paths)) {
  # Load the model
  load(model_paths[i])
  
  # Assume 'model' is the object that gets loaded and contains the DLNM model results
  # Also assume that 'basis_MaxTm' is already present in your environment and is the correct crossbasis object
  
  # Extract coefficients and variance-covariance matrix
  coef <- model$summary.fixed$mean
  vcov <- model$misc$lincomb.derived.covariance.matrix
  
  # Select the position of the terms associated with Tmax crossbasis
  indt <- grep("basis_MaxTm", model$names.fixed)
  
  # Extract predictions from the Tmax DLNM centered on a specified value (replace with your value)
  pred <- crosspred(basis_MaxTm, coef = coef[indt], vcov = vcov[indt, indt],
                    model.link = "log", bylag = 0.25, cen = 32) # Replace 21 with your desired centering temperature
  
  # Store the predictions in the list
  pred_list[[i]] <- pred
}

pdf("C:/Users/u271201/Downloads/fig_05_pdsi_scenariO3.pdf", width = 12, height = 6)  
# Assuming pred_list is a list of model prediction objects
colors <- c("blue", "red", "orange", "black") # Define a vector of colors for the lines
model_names <- c("Model 1", "Model 2") # Define a vector of model names


plot(pred_list[[1]]$allRRfit, var=40, ylab="RR for exposure 40", xlab="Lag (years)", xlim=c(0,3)) 

for (i in seq_along(pred_list)) {
  lines(pred_list[[i]], var=35, col = colors[i]) +
  lines(pred_list[[i]], var=40,col = colors[i] ) +
  lines(pred_list[[i]], var=35, col = colors[i])
}

# Adding a legend to the plot for clarity
legend("topright", legend=c("Exposure 40 (predp)", "Exposure 35 (predp)", "Exposure 40 (predp1)", "Exposure 35 (predp1)"), 
     col= colors)

dev.off()








# Define the paths to the model files
model_files <- c(
  "C:\\Users\\u271201\\Downloads\\output\\Mort_Female_65_\\model0.6.RData",
  "C:\\Users\\u271201\\Downloads\\output\\Mort_Female_65_\\model0.7.RData",
  "C:\\Users\\u271201\\Downloads\\output\\Mort_Female_65_\\model0.8.RData",
  "C:\\Users\\u271201\\Downloads\\output\\Mort_Female_65_\\model0.9.RData"
)


# Initialize an empty list for storing predictions
pred_list <- list()

# Loop through the model files and generate predictions
for (i in seq_along(model_files)) {
  load(model_files[i]) # This will load the 'model' object
  modelxx <- model
  
  # Extract coefficients and variance-covariance matrix
  coef <- modelxx$summary.fixed$mean
  vcov <- modelxx$misc$lincomb.derived.covariance.matrix
  
  # Create indicators for terms associated with PDSI cross basis
  indp <- grep("basis_MaxTm", modelxx$names.fixed)
  
  # Extract predictions from the PDSI DLNM centered on zero (normal conditions)
  predp <- crosspred(basis_MaxTm, coef = coef[indp], vcov = vcov[indp,indp],
                     model.link = "log", bylag = 0.1, cen = 32)
  
  # Store predictions in the list
  pred_list[[i]] <- predp
}

# Open PDF device for plotting
pdf("C:/Users/u271201/Downloads/combined_plots.pdf", width = 12, height = 6)

# Plot the predictions for each model on the same graph
for (i in seq_along(pred_list)) {
  if (i == 1) {
    # Plot the first model's predictions with axes
    plot(pred_list[[i]], var = 40, ylab = " for exposure 40", xlab = "Lag (years)", xlim = c(0, 3), type = "l", lty = i)
  } else {
    # For subsequent models, add the lines without axes
    par(new = TRUE)
    plot(pred_list[[i]], var = 40, ylab = "", xlab = "", xlim = c(0, 3), type = "l", lty = i, axes = FALSE, ann = FALSE)
  }
  # Add lines for var = 35 for each model
  lines(pred_list[[i]], var = 35, lty = i + 1, type = "l")
}

# Add a legend to the plot for clarity
legend("topright", legend = paste("Model", seq_along(pred_list)), lty = 1:length(pred_list), col = "black")

# Close the PDF device to save the plot
dev.off()













# Print the final table to view the results
table1 <- as.data.frame(matrix(NA, 4, 16))
colnames(table1) <- c("Setting", 
                      "extr_var", "extr_lag", "extr_rr", "extr_lci","extr_uci",
                      "extr1_var", "extr1_lag", "extr1_rr", "extr1_lci","extr1_uci","extr2_var", "extr2_lag", "extr2_rr", "extr2_lci","extr2_uci")
table1[,1] <- c("Overall", "High","Intermediate", "Low")



model_files <- c(
  "C:\\Users\\u271201\\Downloads\\output\\Mort_Female_65_\\model0.6.RData",
  "C:\\Users\\u271201\\Downloads\\output\\Mort_Female_65_\\model0.7.RData",
  "C:\\Users\\u271201\\Downloads\\output\\Mort_Female_65_\\model0.8.RData",
  "C:\\Users\\u271201\\Downloads\\output\\Mort_Female_65_\\model0.9.RData"
)

# Initialize an empty list for storing predictions and a vector for model labels
pred_list <- list()
model_labels <- vector("character", length(model_files))

# Loop through the model files and generate predictions
for (j in seq_along(model_files)) {
  load(model_files[i]) # This will load the 'model' object
  modelxx <- model
  
  # Generate a model label based on file names or another unique identifier
  model_labels[j] <- paste("model", i, sep="")
  
  # Extract coefficients and variance-covariance matrix
  coef <- modelxx$summary.fixed$mean
  vcov <- modelxx$misc$lincomb.derived.covariance.matrix
  
  # Create indicators for terms associated with PDSI cross basis
  indp <- grep("basis_MaxTm", modelxx$names.fixed)
  
  # Extract predictions from the PDSI DLNM centered on zero (normal conditions)
  predp <- crosspred(basis_MaxTm, coef = coef[indp], vcov = vcov[indp, indp],
                     model.link = "log", bylag = 0.1, cen = 32)
  
  # Store predictions in the list
  pred_list[[j]] <- predp


# Create a data frame to gather all the results
results_df <- data.frame()
for (i in seq_along(pred_list)) {
  # Get variables and lag steps - ensure these are correctly specified
  pred_data <- pred_list[[i]]
  vars <- pred_data$predvar  # Ensure this contains the correct data for variables
  lagbylag <- seq(0, max(pred_data$lag), length.out = length(pred_data$matRRfit) / length(unique(pred_data$predvar)))
  
  temp_df <- data.frame(
    model = model_labels[j],
    rr = as.vector(pred_data$matRRfit),
    rr_lci = as.vector(pred_data$matRRlow),
    rr_uci = as.vector(pred_data$matRRhigh),
    var = rep(vars, each = length(lagbylag)),
    lag = rep(lagbylag, times = length(vars))
  )
  
  results_df <- rbind(results_df, temp_df)
}

# Now print or return results_df which contains all the data structured as needed
print(results_df)
# Print the final table to view the results
################################################################################

load("C:\\Users\\u271201\\Downloads\\output\\Mort_Female_65_79\\model0.6.RData")
model1 <- model
load("C:\\Users\\u271201\\Downloads\\output\\Mort_Female_65_\\model0.7.RData")
model2 <- model
load("C:\\Users\\u271201\\Downloads\\output\\Mort_Female_65_\\model0.8.RData")
model3 <- model
load("C:\\Users\\u271201\\Downloads\\output\\Mort_Female_65_\\model0.9.RData")
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
                   model.link = "log", bylag = 0.25, cen = 32)
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



load("C:\\Users\\u271201\\Downloads\\output\\model0.6.RData")
model1 <- model
load("C:\\Users\\u271201\\Downloads\\output\\model0.7.RData")
model2 <- model
load("C:\\Users\\u271201\\Downloads\\output\\model0.8.RData")
model3 <- model
load("C:\\Users\\u271201\\Downloads\\output\\model0.9.RData")
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
                     model.link = "log", bylag = 0.25, cen = 32)
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
load("C:\\Users\\u271201\\Downloads\\output\\Mort_tot_65_\\model0.6.RData")
model1 <- model
load("C:\\Users\\u271201\\Downloads\\output\\Mort_tot_65_\\model0.7.RData")
model2 <- model
load("C:\\Users\\u271201\\Downloads\\output\\Mort_tot_65_\\model0.8.RData")
model3 <- model
load("C:\\Users\\u271201\\Downloads\\output\\Mort_tot_65_\\model0.9.RData")
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
                     model.link = "log", bylag = 0.25, cen = 32)
  
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
combined_table65 <- combined_table[order(combined_table$Setting, combined_table$Gender),]

# Print the sorted table to check
print(combined_table)






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
                     model.link = "log", bylag = 0.1, cen = 32)
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
                     model.link = "log", bylag = 0.1, cen = 32)
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
                     model.link = "log", bylag = 0.1, cen = 32)
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
combined_table80 <- combined_table[order(combined_table$Setting, combined_table$Gender),]

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
                     model.link = "log", bylag = 0.1, cen = 32)
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
                     model.link = "log", bylag = 0.1, cen = 32)
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
                     model.link = "log", bylag = 0.1, cen = 32)
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
combined_table65<- combined_table[order(combined_table$Setting, combined_table$Gender),]

# Print the sorted table to check
print(combined_table65_79)
print(combined_table80)


combined_data <- inner_join(combined_table65, combined_table80, by = c("Gender", "Setting"))




long_data <- combined_data %>%
  pivot_longer(
    cols = -c(Gender, Setting),  # Ensure these are the only columns not to pivot
    names_to = c("Group", "Variable"),
    names_pattern = "^(age[0-9][0-9])_(.*)$"  # Adjusted regex to correctly capture groups
  )


# Pivot back to wide format with specific order
wide_data <- long_data %>%
  arrange(Gender, Setting, Group, Variable) %>%
  pivot_wider(
    names_from = c(Group, Variable),
    values_from = Value
  )

# Optionally, order columns manually if automatic methods don't suffice
desired_order <- c("Gender", "Setting", sort(setdiff(names(wide_data), c("Gender", "Setting"))))
wide_data <- wide_data %>%
  select(all_of(desired_order))