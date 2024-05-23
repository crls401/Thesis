# Expose-response temperature by pollutants

load("C:\\Users\\u271201\\Downloads\\output\\Mort_Male_65_\\model0.1.RData")
model1 <- model
load("C:\\Users\\u271201\\Downloads\\output\\Mort_Female_65_\\model0.1.RData")
model2 <- model
load("C:\\Users\\u271201\\Downloads\\output\\Mort_tot_65_\\model0.1.RData")
model3 <- model

model_paths <- c(
 " C:\\Users\\u271201\\Downloads\\output\\Mort_tot_65_\\model0.1.RData",
 "C:\\Users\\u271201\\Downloads\\output\\Mort_Female_65_\\model0.1.RData",
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
                    model.link = "log", bylag = 0.25, cen = 32) # Replace 32 with your desired centering temperature
  
  # Store the predictions in the list
  pred_list[[i]] <- pred
}


# Assuming pred_list is a list of model prediction objects
colors <- c("blue", "red", "green", "orange") # Define a vector of colors for the lines
model_names <- c("Model 1", "Model 2", "Model 3", "Model 4") # Define a vector of model names

pdf("C:/Users/u271201/Downloads/combined_plot.pdf", width = 12, height = 6)

# First, set up the plot with an initial range that will encompass all models
plot_range <- range(unlist(lapply(pred_list, function(x) c(range(x$allRRlow, x$allRRhigh)))))

# Start the plot with the first model to set up axes, labels, etc.
plot(pred_list[[1]], "overall", xlab = expression(paste("Temperature (",degree,"C)")), 
     ylab = "Relative risk", ylim = plot_range, type = 'n')

# Now add lines for each model
for (i in seq_along(pred_list)) {
  lines(pred_list[[i]]$predvar, pred_list[[i]]$allRRfit, col = colors[i], lwd = 2)
}

# Add a legend to the plot
legend("topright", legend = model_names, col = colors, lwd = 2, cex = 0.8)

dev.off()










model_paths <- c(
  "C:\\Users\\u271201\\Downloads\\output\\Mort_Male_65_\\model0.1.RData",
  "C:\\Users\\u271201\\Downloads\\output\\Mort_Male_65_\\model0.6.RData",
  "C:\\Users\\u271201\\Downloads\\output\\Mort_Male_65_\\model0.7.RData",
  "C:\\Users\\u271201\\Downloads\\output\\Mort_Male_65_\\model0.8.RData",
  "C:\\Users\\u271201\\Downloads\\output\\Mort_Male_65_\\model0.9.RData"
)

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
                    model.link = "log", bylag = 0.25, cen = 21) # Replace 21 with your desired centering temperature
  
  # Store the predictions in the list
  pred_list[[i]] <- pred
}


# Assuming pred_list is a list of model prediction objects
colors <- c("black","blue", "red", "green", "orange") # Define a vector of colors for the lines
model_names <- c("Model base", "Model 1", "Model 2", "Model 3", "Model 4") # Define a vector of model names

pdf("C:/Users/u271201/Downloads/combined_plot.pdf", width = 12, height = 6)

# First, set up the plot with an initial range that will encompass all models
plot_range <- range(unlist(lapply(pred_list, function(x) c(range(x$allRRlow, x$allRRhigh)))))

# Start the plot with the first model to set up axes, labels, etc.
plot(pred_list[[1]], "overall", xlab = expression(paste("Temperature (",degree,"C)")), 
     ylab = "Relative risk", ylim = plot_range, type = 'n')

# Now add lines for each model
for (i in seq_along(pred_list)) {
  lines(pred_list[[i]]$predvar, pred_list[[i]]$allRRfit, col = colors[i], lwd = 2)
}

# Add a legend to the plot
legend("topright", legend = model_names, col = colors, lwd = 2, cex = 0.8)

dev.off()


################################################################################
              "Expose response by temperature 65-79 and 80"
################################################################################
model_paths <- c(
  "C:\\Users\\u271201\\Downloads\\output\\Mort_Female_65_79\\model0.1.RData",
  "C:\\Users\\u271201\\Downloads\\output\\Mort_Male_65_79\\model0.1.RData",
  "C:\\Users\\u271201\\Downloads\\output\\Mort_Female_80_\\model0.1.RData",
  "C:\\Users\\u271201\\Downloads\\output\\Mort_Male_80_\\model0.1.RData"
)

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


# Assuming pred_list is a list of model prediction objects
colors <- c("black","blue", "red", "green") # Define a vector of colors for the lines
model_names <- c("Female 65-79", "Male 65-79", "Female 80+", "Male 80+") # Define a vector of model names

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
legend("topright", legend = model_names, col = colors, lwd = 2, cex = 0.8)

dev.off()

