

# Loop through each month
for (nweek in 1:52) {
  # Construct the file path based on the month
  file_path <- sprintf("C:/Users/u271201/Downloads/output/preds/preds_2017_%d.RData",nweek)
  
  # Load the data from the file
  load(file_path)
  
  # Update to use the loaded 'model' directly
  # Extract coefficients and variance-covariance matrix
  coef <- model$summary.fixed$mean
  vcov <- model$misc$lincomb.derived.covariance.matrix
  
  # Select the position of the terms associated with Tmax crossbasis
  indt <- grep("basis_MaxTm", model$names.fixed)
  
  # Extract predictions from the Tmax DLNM centred on overall mean Tmax (30 deg C)
  # Assume `basis_MaxTm` is already defined or loaded appropriately
  pred_name <- paste("predtmx_",  week, sep = "")
  assign(pred_name, crosspred(basis_MaxTm, coef = coef[indt], vcov = vcov[indt, indt],
                              model.link = "log", bylag = 0.25, cen = 27))
  
 
} 
  
  
  
  
   # Here, `basis_MaxTm` needs to be defined or loaded before the loop if it doesn't change per month
  # or inside the loop if it changes with the model loaded from each RData file.
  pdf("C:/Users/u271201/Downloads/fig.pdf", width = 12, height = 6)   
  # Setting up the initial plot with broad enough settings
  plot(1, type="n", xlim=c(0,3), ylim=c(0.8, 1.4), xlab="Lag (years)", ylab="OR for exposure")
  
  # Define colors and line types for each var and month
  colors <- rainbow(104) # 24 distinct colors for 12 months * 2 vars per month
  line_types <- rep(1:2, 52) # Alternating line types for differentiation
  
  # Loop through each month and plot both var=40 and var=35
  for ( nweek in 1:52) {
    predtmx_current <- get(paste("predtmx_", week, sep = ""))
    
    # Plotting lines for var=40 and var=35
    # Var=40
    lines(predtmx_current, var=40, col=colors[week*2-1], lty=line_types[week*2-1])
    
    # Var=nweek
    lines(predtmx_current, var=35, col=colors[week*2], lty=line_types[week*2])
  }
  
  # Adding a legend to the plot for clarity
  legend("topright", legend=c(paste("Month", 1:52, "Var=40"), paste("Month", 1:52, "Var=35")), col=colors, lty=rep(1:2, 12), cex=0.5, ncol=2)
  
  dev.off()
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # Loop through each month
  for (nweek in 1:52) {
    # Construct the file path based on the month
    file_path <- sprintf("C:/Users/u271201/Downloads/output/preds/preds_2017_%d.RData",nweek)
    
    # Load the data from the file
    load(file_path)
    
    # Assuming the RR needs to be calculated from the model coefficients
    # This is an example calculation. Adjust according to how your RRs are derived.
    # Here, I'm assuming 'coef' represents the log of the RR (common in logistic regression models)
    # and you want to extract it for a specific variable, e.g., "basis_MaxTm"
    coef <- model$summary.fixed$mean
    indt <- grep("basis_MaxTm", model$names.fixed)
    rr <- exp(coef[indt]) # Exponentiating to get the RR
    
    # Store the RR values in the list with a descriptive name
    rr_list[[paste("RR_month_", nweek, sep = "")]] <- rr
  }
  
  summary(rr_list)
  