################################################################################
                                     "NO2"
################################################################################

load("C:\\Users\\u271201\\Downloads\\output\\Mort_Female_65_\\NO2_Month_rf.RData")
model1 <- model
load("C:\\Users\\u271201\\Downloads\\output\\Mort_Male_65_\\NO2_Month_rf.RData")
model2 <- model 
# Create a list to store the results data frames
results_list <- list()

# List of model names for reference
mod.names <- c("model1", "model2")

# Loop through the model names, calculate the relative risks, and store results
for (j in seq_along(mod.names)) {
  # Obtain the model from the name
  model <- get(mod.names[j])
  
  # Extract the data from the model and calculate relative risks
  id <- model$summary.random$YearMonth$ID
  rr <- exp(model$summary.random$YearMonth$mean)
  rr.lci <- exp(model$summary.random$YearMonth$"0.025quant")
  rr.uci <- exp(model$summary.random$YearMonth$"0.975quant")
  
  # Create a data frame of the results
  results <- data.frame(id = id, rr = rr, rr.lci = rr.lci, rr.uci = rr.uci, group = mod.names[j])
  
  # Subset the results if necessary (assumed based on your code)
  interaction_data <- results[grep("[1-8].[5-8]", results$id), ]
  
  # Add the subsetted data to the results list
  results_list[[mod.names[j]]] <- interaction_data
}

# Now results_list has two data frames: one for each model
# Combine them for plotting
all_results <- do.call(rbind, results_list)

# Adjusting the transformation for rounding to handle precision issues
all_results <- all_results %>%
  mutate(
    # Ensure 'id' is numeric
    id_numeric = as.numeric(as.character(id)),
    # Add rounding to handle potential floating-point precision issues
    month_code = round((id_numeric %% 1) * 10, 0),  # Multiply by 10 and round to handle decimals like 1.5, 1.6
    year = 2016 + floor(id_numeric),  # Adjust the base year to 2017
    month = case_when(
      month_code == 5 ~ "May",
      month_code == 6 ~ "June",
      month_code == 7 ~ "July",
      month_code == 8 ~ "August"
    ),
    # Combine the new year and month into a new 'id'
    id = paste(month, year),
    # Rename the groups
    group = case_when(
      group == "model1" ~ "Female",
      group == "model2" ~ "Male"
    )
  ) %>%
  select(-year, -month_code, -id_numeric)  # Clean up by removing intermediate columns
write.csv(all_results, "C:\\Users\\u271201\\Downloads\\Figures analysis\\5.Summer\\Tables\\RR Temperatures month NO2.csv", row.names = FALSE)


theme_set(bayesplot::theme_default())
pos <- position_nudge(y = ifelse(all_results$group == "Male", 0, 0.3))

summer_month <- ggplot(all_results, aes(x = rr, y =id , color = group)) + 
  geom_point(position = pos) +
  geom_linerange(aes(xmin = rr.lci, xmax = rr.uci), position = pos) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "black", size=1) +  # Reference line # Facet by group with separate x axes
  theme_bw(base_size = 11) +  # Theme
  theme(
    axis.text.y = element_text(size = 11, colour = "black"),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 11, angle = 0, hjust = 1, colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", colour = "black"),
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    strip.background = element_rect(fill = "white", colour = "black"),
    strip.text = element_text(size = 11, face = "bold")
  ) +
  labs(
    title = "Relative Risk by Month and Year Interaction",
    x = "Relative Risk (RR)",
    y = ""
  ) +
  scale_color_brewer(palette = "Set1") 
ggsave("C:\\Users\\u271201\\Downloads\\Figures analysis\\5.Summer\\Figures\\NO2\\months.eps", height = 20, width = 20, units = "cm")
################################################################################
################################################################################
load("C:\\Users\\u271201\\Downloads\\output\\Mort_Female_65_\\NO2_Month_rf_nuts.RData")
model1 <- model
load("C:\\Users\\u271201\\Downloads\\output\\Mort_Male_65_\\NO2_Month_rf_nuts.RData")
model2 <- model 

# Create a list to store the results data frames
results_list <- list()

# List of model names for reference
mod.names <- c("model1", "model2")

# Loop through the model names, calculate the relative risks, and store results
for (j in seq_along(mod.names)) {
  # Obtain the model from the name
  model <- get(mod.names[j])

  id <- model$summary.random$`interaction(NUTS3, YearMonth)`$ID
  rr <- exp(model$summary.random$`interaction(NUTS3, YearMonth)`$mean)
  rr.lci<- exp(model$summary.random$`interaction(NUTS3, YearMonth)`$"0.025quant")
  rr.uci <- exp(model$summary.random$`interaction(NUTS3, YearMonth)`$"0.975quant")
  
  results <- data.frame(id = id, rr = rr, rr.lci=rr.lci,rr.uci = rr.uci,  group = mod.names[j] )
  # Assuming your data is in a data frame called 'results'
  interaction_data <- results[grep("ES[0-9][0-9][0-9].[1-7].[5-8]", results$id), ]
  # Extract the NUTS3 code using str_extract and a regex that captures the pattern
  interaction_data$NUTS3 <- str_extract(interaction_data$id, "ES\\d+")
  # Add the subsetted data to the results list
  results_list[[mod.names[j]]] <- interaction_data
}
# Now results_list has two data frames: one for each model
# Combine them for plotting
all_results <- do.call(rbind, results_list)

# Update the transformation with debugging info and improved logic
all_results <- all_results %>%
  mutate(
    # Extract the last 'number.number' from 'id'
    id_numeric = as.numeric(str_extract(id, "\\d+\\.\\d+$")),
    
    # Debug: View raw decimal parts before any manipulation
    raw_decimal = id_numeric %% 1,
    
    # Adjust month_code calculation to handle edge cases more gracefully
    month_code = round(raw_decimal * 10),
    
    # Debug: Check what values month_code takes
    check_month_code = month_code,
    
    # Calculate year
    year = if_else(!is.na(id_numeric), 2016 + floor(id_numeric), NA_integer_),
    
    # Determine month from month_code
    month = case_when(
      month_code == 5 ~ "May",
      month_code == 6 ~ "June",
      month_code == 7 ~ "July",
      month_code == 8 ~ "August",
      TRUE ~ "Invalid month"  # Handling unexpected month_code values
    ),
    
    # Create the new 'date_category' field
    date_category = if_else(month != "Invalid month" & !is.na(year), paste(month, year), "Invalid ID"),
    
    # Rename groups
    group = case_when(
      group == "model1" ~ "Female",
      group == "model2" ~ "Male"
    )
  ) %>%
  select(-id_numeric, -year, -raw_decimal, -check_month_code)  # Optionally keep these for debugging
write.csv(all_results, "C:\\Users\\u271201\\Downloads\\Figures analysis\\5.Summer\\Tables\\RR Temperatures month nuts NO2.csv", row.names = FALSE)


theme_set(bayesplot::theme_default())
pos <- position_nudge(y = ifelse(all_results$group == "Male", 0, 0.3))

summer_month_nuts <- 
  all_results %>% 
  group_by(rr,rr.lci,rr.uci,NUTS3) %>%
  # add the predefined state grid by state code
  left_join(grid, by = c("NUTS3" = "code_num")) %>%
  ggplot(aes(x = rr, y =id, color = group)) + 
  geom_point(position = pos) +
  geom_linerange(aes(xmin = rr.lci, xmax = rr.uci), position = pos) +
  scale_y_discrete(labels = setNames(transformed_data$date_category, transformed_data$id)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "black", size=1) +  # Reference line # Facet by group with separate x axes
  theme_bw(base_size = 8) +  # Theme
  theme(
    axis.text.y = element_text(size = 8, colour = "black"),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 8, angle = 0, hjust = 1, colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", colour = "black"),
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    strip.background = element_rect(fill = "grey", colour = "black"),
    strip.text = element_text(size = 8, face = "bold"),
    plot.background = element_rect(fill = "aliceblue", colour = NA) 
  ) +
  labs(
    title = "Relative Risk by Month and Year Interaction",
    x = "Relative Risk (RR)",
    y = "date_category"
  ) +
  scale_color_brewer(palette = "Set1") +
  # organise by state name in grid file
  facet_geo( ~name, grid = grid, scales = "free_y")

ggsave("C:\\Users\\u271201\\Downloads\\Figures analysis\\5.Summer\\Figures\\NO2\\months_nuts.eps", height = 70, width = 100, units = "cm")


################################################################################
                                        "O3"
################################################################################
load("C:\\Users\\u271201\\Downloads\\output\\Mort_Female_65_\\O3_Month_rf.RData")
model1 <- model
load("C:\\Users\\u271201\\Downloads\\output\\Mort_Male_65_\\O3_Month_rf.RData")
model2 <- model 
# Create a list to store the results data frames
results_list <- list()

# List of model names for reference
mod.names <- c("model1", "model2")

# Loop through the model names, calculate the relative risks, and store results
for (j in seq_along(mod.names)) {
  # Obtain the model from the name
  model <- get(mod.names[j])
  
  # Extract the data from the model and calculate relative risks
  id <- model$summary.random$YearMonth$ID
  rr <- exp(model$summary.random$YearMonth$mean)
  rr.lci <- exp(model$summary.random$YearMonth$"0.025quant")
  rr.uci <- exp(model$summary.random$YearMonth$"0.975quant")
  
  # Create a data frame of the results
  results <- data.frame(id = id, rr = rr, rr.lci = rr.lci, rr.uci = rr.uci, group = mod.names[j])
  
  # Subset the results if necessary (assumed based on your code)
  interaction_data <- results[grep("[1-8].[5-8]", results$id), ]
  
  # Add the subsetted data to the results list
  results_list[[mod.names[j]]] <- interaction_data
}

# Now results_list has two data frames: one for each model
# Combine them for plotting
all_results <- do.call(rbind, results_list)

# Adjusting the transformation for rounding to handle precision issues
all_results <- all_results %>%
  mutate(
    # Ensure 'id' is numeric
    id_numeric = as.numeric(as.character(id)),
    # Add rounding to handle potential floating-point precision issues
    month_code = round((id_numeric %% 1) * 10, 0),  # Multiply by 10 and round to handle decimals like 1.5, 1.6
    year = 2016 + floor(id_numeric),  # Adjust the base year to 2017
    month = case_when(
      month_code == 5 ~ "May",
      month_code == 6 ~ "June",
      month_code == 7 ~ "July",
      month_code == 8 ~ "August"
    ),
    # Combine the new year and month into a new 'id'
    id = paste(month, year),
    # Rename the groups
    group = case_when(
      group == "model1" ~ "Female",
      group == "model2" ~ "Male"
    )
  ) %>%
  select(-year, -month_code, -id_numeric)  # Clean up by removing intermediate columns
write.csv(all_results, "C:\\Users\\u271201\\Downloads\\Figures analysis\\5.Summer\\Tables\\RR Temperatures month O3.csv", row.names = FALSE)


theme_set(bayesplot::theme_default())
pos <- position_nudge(y = ifelse(all_results$group == "Male", 0, 0.3))

summer_month <- ggplot(all_results, aes(x = rr, y =id , color = group)) + 
  geom_point(position = pos) +
  geom_linerange(aes(xmin = rr.lci, xmax = rr.uci), position = pos) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "black", size=1) +  # Reference line # Facet by group with separate x axes
  theme_bw(base_size = 11) +  # Theme
  theme(
    axis.text.y = element_text(size = 11, colour = "black"),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 11, angle = 0, hjust = 1, colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", colour = "black"),
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    strip.background = element_rect(fill = "white", colour = "black"),
    strip.text = element_text(size = 11, face = "bold")
  ) +
  labs(
    title = "Relative Risk by Month and Year Interaction",
    x = "Relative Risk (RR)",
    y = ""
  ) +
  scale_color_brewer(palette = "Set1") 
ggsave("C:\\Users\\u271201\\Downloads\\Figures analysis\\5.Summer\\Figures\\O3\\months.eps", height = 20, width = 20, units = "cm")
################################################################################
################################################################################
load("C:\\Users\\u271201\\Downloads\\output\\Mort_Female_65_\\NO2_Month_rf_nuts.RData")
model1 <- model
load("C:\\Users\\u271201\\Downloads\\output\\Mort_Male_65_\\NO2_Month_rf_nuts.RData")
model2 <- model 

# Create a list to store the results data frames
results_list <- list()

# List of model names for reference
mod.names <- c("model1", "model2")

# Loop through the model names, calculate the relative risks, and store results
for (j in seq_along(mod.names)) {
  # Obtain the model from the name
  model <- get(mod.names[j])
  
  id <- model$summary.random$`interaction(NUTS3, YearMonth)`$ID
  rr <- exp(model$summary.random$`interaction(NUTS3, YearMonth)`$mean)
  rr.lci<- exp(model$summary.random$`interaction(NUTS3, YearMonth)`$"0.025quant")
  rr.uci <- exp(model$summary.random$`interaction(NUTS3, YearMonth)`$"0.975quant")
  
  results <- data.frame(id = id, rr = rr, rr.lci=rr.lci,rr.uci = rr.uci,  group = mod.names[j] )
  # Assuming your data is in a data frame called 'results'
  interaction_data <- results[grep("ES[0-9][0-9][0-9].[1-7].[5-8]", results$id), ]
  # Extract the NUTS3 code using str_extract and a regex that captures the pattern
  interaction_data$NUTS3 <- str_extract(interaction_data$id, "ES\\d+")
  # Add the subsetted data to the results list
  results_list[[mod.names[j]]] <- interaction_data
}
# Now results_list has two data frames: one for each model
# Combine them for plotting
all_results <- do.call(rbind, results_list)

# Update the transformation with debugging info and improved logic
all_results <- all_results %>%
  mutate(
    # Extract the last 'number.number' from 'id'
    id_numeric = as.numeric(str_extract(id, "\\d+\\.\\d+$")),
    
    # Debug: View raw decimal parts before any manipulation
    raw_decimal = id_numeric %% 1,
    
    # Adjust month_code calculation to handle edge cases more gracefully
    month_code = round(raw_decimal * 10),
    
    # Debug: Check what values month_code takes
    check_month_code = month_code,
    
    # Calculate year
    year = if_else(!is.na(id_numeric), 2016 + floor(id_numeric), NA_integer_),
    
    # Determine month from month_code
    month = case_when(
      month_code == 5 ~ "May",
      month_code == 6 ~ "June",
      month_code == 7 ~ "July",
      month_code == 8 ~ "August",
      TRUE ~ "Invalid month"  # Handling unexpected month_code values
    ),
    
    # Create the new 'date_category' field
    date_category = if_else(month != "Invalid month" & !is.na(year), paste(month, year), "Invalid ID"),
    
    # Rename groups
    group = case_when(
      group == "model1" ~ "Female",
      group == "model2" ~ "Male"
    )
  ) %>%
  select(-id_numeric, -year, -raw_decimal, -check_month_code)  # Optionally keep these for debugging
write.csv(all_results, "C:\\Users\\u271201\\Downloads\\Figures analysis\\5.Summer\\Tables\\RR Temperatures month nuts O3.csv", row.names = FALSE)


theme_set(bayesplot::theme_default())
pos <- position_nudge(y = ifelse(all_results$group == "Male", 0, 0.3))

summer_month_nuts <- 
  all_results %>% 
  group_by(rr,rr.lci,rr.uci,NUTS3) %>%
  # add the predefined state grid by state code
  left_join(grid, by = c("NUTS3" = "code_num")) %>%
  ggplot(aes(x = rr, y =id, color = group)) + 
  geom_point(position = pos) +
  geom_linerange(aes(xmin = rr.lci, xmax = rr.uci), position = pos) +
  scale_y_discrete(labels = setNames(transformed_data$date_category, transformed_data$id)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "black", size=1) +  # Reference line # Facet by group with separate x axes
  theme_bw(base_size = 8) +  # Theme
  theme(
    axis.text.y = element_text(size = 8, colour = "black"),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 8, angle = 0, hjust = 1, colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", colour = "black"),
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    strip.background = element_rect(fill = "grey", colour = "black"),
    strip.text = element_text(size = 8, face = "bold"),
    plot.background = element_rect(fill = "aliceblue", colour = NA) 
  ) +
  labs(
    title = "Relative Risk by Month and Year Interaction",
    x = "Relative Risk (RR)",
    y = "date_category"
  ) +
  scale_color_brewer(palette = "Set1") +
  # organise by state name in grid file
  facet_geo( ~name, grid = grid, scales = "free_y")

ggsave("C:\\Users\\u271201\\Downloads\\Figures analysis\\5.Summer\\Figures\\O3\\months_nuts.eps", height = 70, width = 100, units = "cm")

################################################################################
                                    "PM25"
################################################################################
load("C:\\Users\\u271201\\Downloads\\output\\Mort_Female_65_\\PM25_Month_rf.RData")
model1 <- model
load("C:\\Users\\u271201\\Downloads\\output\\Mort_Male_65_\\PM25_Month_rf.RData")
model2 <- model 
# Create a list to store the results data frames
results_list <- list()

# List of model names for reference
mod.names <- c("model1", "model2")

# Loop through the model names, calculate the relative risks, and store results
for (j in seq_along(mod.names)) {
  # Obtain the model from the name
  model <- get(mod.names[j])
  
  # Extract the data from the model and calculate relative risks
  id <- model$summary.random$YearMonth$ID
  rr <- exp(model$summary.random$YearMonth$mean)
  rr.lci <- exp(model$summary.random$YearMonth$"0.025quant")
  rr.uci <- exp(model$summary.random$YearMonth$"0.975quant")
  
  # Create a data frame of the results
  results <- data.frame(id = id, rr = rr, rr.lci = rr.lci, rr.uci = rr.uci, group = mod.names[j])
  
  # Subset the results if necessary (assumed based on your code)
  interaction_data <- results[grep("[1-8].[5-8]", results$id), ]
  
  # Add the subsetted data to the results list
  results_list[[mod.names[j]]] <- interaction_data
}

# Now results_list has two data frames: one for each model
# Combine them for plotting
all_results <- do.call(rbind, results_list)

# Adjusting the transformation for rounding to handle precision issues
all_results <- all_results %>%
  mutate(
    # Ensure 'id' is numeric
    id_numeric = as.numeric(as.character(id)),
    # Add rounding to handle potential floating-point precision issues
    month_code = round((id_numeric %% 1) * 10, 0),  # Multiply by 10 and round to handle decimals like 1.5, 1.6
    year = 2016 + floor(id_numeric),  # Adjust the base year to 2017
    month = case_when(
      month_code == 5 ~ "May",
      month_code == 6 ~ "June",
      month_code == 7 ~ "July",
      month_code == 8 ~ "August"
    ),
    # Combine the new year and month into a new 'id'
    id = paste(month, year),
    # Rename the groups
    group = case_when(
      group == "model1" ~ "Female",
      group == "model2" ~ "Male"
    )
  ) %>%
  select(-year, -month_code, -id_numeric)  # Clean up by removing intermediate columns
write.csv(all_results, "C:\\Users\\u271201\\Downloads\\Figures analysis\\5.Summer\\Tables\\RR Temperatures month PM25.csv", row.names = FALSE)



pos <- position_nudge(y = ifelse(all_results$group == "Male", 0, 0.3))

summer_month <- ggplot(all_results, aes(x = rr, y =id , color = group)) + 
  geom_point(position = pos) +
  geom_linerange(aes(xmin = rr.lci, xmax = rr.uci), position = pos) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "black", size=1) +  # Reference line # Facet by group with separate x axes
  theme_bw(base_size = 11) +  # Theme
  theme(
    axis.text.y = element_text(size = 11, colour = "black"),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 11, angle = 0, hjust = 1, colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", colour = "black"),
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    strip.background = element_rect(fill = "white", colour = "black"),
    strip.text = element_text(size = 11, face = "bold")
  ) +
  labs(
    title = "Relative Risk by Month and Year Interaction",
    x = "Relative Risk (RR)",
    y = ""
  ) +
  scale_color_brewer(palette = "Set1") 
ggsave("C:\\Users\\u271201\\Downloads\\Figures analysis\\5.Summer\\Figures\\PM25\\months.eps", height = 20, width = 20, units = "cm")
################################################################################
################################################################################
load("C:\\Users\\u271201\\Downloads\\output\\Mort_Female_65_\\PM25_Month_rf_nuts.RData")
model1 <- model
load("C:\\Users\\u271201\\Downloads\\output\\Mort_Male_65_\\PM25_Month_rf_nuts.RData")
model2 <- model 

# Create a list to store the results data frames
results_list <- list()

# List of model names for reference
mod.names <- c("model1", "model2")

# Loop through the model names, calculate the relative risks, and store results
for (j in seq_along(mod.names)) {
  # Obtain the model from the name
  model <- get(mod.names[j])
  
  id <- model$summary.random$`interaction(NUTS3, YearMonth)`$ID
  rr <- exp(model$summary.random$`interaction(NUTS3, YearMonth)`$mean)
  rr.lci<- exp(model$summary.random$`interaction(NUTS3, YearMonth)`$"0.025quant")
  rr.uci <- exp(model$summary.random$`interaction(NUTS3, YearMonth)`$"0.975quant")
  
  results <- data.frame(id = id, rr = rr, rr.lci=rr.lci,rr.uci = rr.uci,  group = mod.names[j] )
  # Assuming your data is in a data frame called 'results'
  interaction_data <- results[grep("ES[0-9][0-9][0-9].[1-7].[5-8]", results$id), ]
  # Extract the NUTS3 code using str_extract and a regex that captures the pattern
  interaction_data$NUTS3 <- str_extract(interaction_data$id, "ES\\d+")
  # Add the subsetted data to the results list
  results_list[[mod.names[j]]] <- interaction_data
}
# Now results_list has two data frames: one for each model
# Combine them for plotting
all_results <- do.call(rbind, results_list)

# Update the transformation with debugging info and improved logic
all_results <- all_results %>%
  mutate(
    # Extract the last 'number.number' from 'id'
    id_numeric = as.numeric(str_extract(id, "\\d+\\.\\d+$")),
    
    # Debug: View raw decimal parts before any manipulation
    raw_decimal = id_numeric %% 1,
    
    # Adjust month_code calculation to handle edge cases more gracefully
    month_code = round(raw_decimal * 10),
    
    # Debug: Check what values month_code takes
    check_month_code = month_code,
    
    # Calculate year
    year = if_else(!is.na(id_numeric), 2016 + floor(id_numeric), NA_integer_),
    
    # Determine month from month_code
    month = case_when(
      month_code == 5 ~ "May",
      month_code == 6 ~ "June",
      month_code == 7 ~ "July",
      month_code == 8 ~ "August",
      TRUE ~ "Invalid month"  # Handling unexpected month_code values
    ),
    
    # Create the new 'date_category' field
    date_category = if_else(month != "Invalid month" & !is.na(year), paste(month, year), "Invalid ID"),
    
    # Rename groups
    group = case_when(
      group == "model1" ~ "Female",
      group == "model2" ~ "Male"
    )
  ) %>%
  select(-id_numeric, -year, -raw_decimal, -check_month_code)  # Optionally keep these for debugging
write.csv(all_results, "C:\\Users\\u271201\\Downloads\\Figures analysis\\5.Summer\\Tables\\RR Temperatures month nuts PM25.csv", row.names = FALSE)


theme_set(bayesplot::theme_default())
pos <- position_nudge(y = ifelse(all_results$group == "Male", 0, 0.3))

summer_month_nuts <- 
  all_results %>% 
  group_by(rr,rr.lci,rr.uci,NUTS3) %>%
  # add the predefined state grid by state code
  left_join(grid, by = c("NUTS3" = "code_num")) %>%
  ggplot(aes(x = rr, y =id, color = group)) + 
  geom_point(position = pos) +
  geom_linerange(aes(xmin = rr.lci, xmax = rr.uci), position = pos) +
  scale_y_discrete(labels = setNames(transformed_data$date_category, transformed_data$id)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "black", size=1) +  # Reference line # Facet by group with separate x axes
  theme_bw(base_size = 8) +  # Theme
  theme(
    axis.text.y = element_text(size = 8, colour = "black"),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 8, angle = 0, hjust = 1, colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", colour = "black"),
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    strip.background = element_rect(fill = "grey", colour = "black"),
    strip.text = element_text(size = 8, face = "bold"),
    plot.background = element_rect(fill = "aliceblue", colour = NA) 
  ) +
  labs(
    title = "Relative Risk by Month and Year Interaction",
    x = "Relative Risk (RR)",
    y = "date_category"
  ) +
  scale_color_brewer(palette = "Set1") +
  # organise by state name in grid file
  facet_geo( ~name, grid = grid, scales = "free_y")

ggsave("C:\\Users\\u271201\\Downloads\\Figures analysis\\5.Summer\\Figures\\PM25\\months_nuts.eps", height = 70, width = 100, units = "cm")


################################################################################
                                    "PM10"
################################################################################
load("C:\\Users\\u271201\\Downloads\\output\\Mort_Female_65_\\PM10_Month_rf.RData")
model1 <- model
load("C:\\Users\\u271201\\Downloads\\output\\Mort_Male_65_\\PM10_Month_rf.RData")
model2 <- model 
# Create a list to store the results data frames
results_list <- list()

# List of model names for reference
mod.names <- c("model1", "model2")

# Loop through the model names, calculate the relative risks, and store results
for (j in seq_along(mod.names)) {
  # Obtain the model from the name
  model <- get(mod.names[j])
  
  # Extract the data from the model and calculate relative risks
  id <- model$summary.random$YearMonth$ID
  rr <- exp(model$summary.random$YearMonth$mean)
  rr.lci <- exp(model$summary.random$YearMonth$"0.025quant")
  rr.uci <- exp(model$summary.random$YearMonth$"0.975quant")
  
  # Create a data frame of the results
  results <- data.frame(id = id, rr = rr, rr.lci = rr.lci, rr.uci = rr.uci, group = mod.names[j])
  
  # Subset the results if necessary (assumed based on your code)
  interaction_data <- results[grep("[1-8].[5-8]", results$id), ]
  
  # Add the subsetted data to the results list
  results_list[[mod.names[j]]] <- interaction_data
}

# Now results_list has two data frames: one for each model
# Combine them for plotting
all_results <- do.call(rbind, results_list)

# Adjusting the transformation for rounding to handle precision issues
all_results <- all_results %>%
  mutate(
    # Ensure 'id' is numeric
    id_numeric = as.numeric(as.character(id)),
    # Add rounding to handle potential floating-point precision issues
    month_code = round((id_numeric %% 1) * 10, 0),  # Multiply by 10 and round to handle decimals like 1.5, 1.6
    year = 2016 + floor(id_numeric),  # Adjust the base year to 2017
    month = case_when(
      month_code == 5 ~ "May",
      month_code == 6 ~ "June",
      month_code == 7 ~ "July",
      month_code == 8 ~ "August"
    ),
    # Combine the new year and month into a new 'id'
    id = paste(month, year),
    # Rename the groups
    group = case_when(
      group == "model1" ~ "Female",
      group == "model2" ~ "Male"
    )
  ) %>%
  select(-year, -month_code, -id_numeric)  # Clean up by removing intermediate columns
write.csv(all_results, "C:\\Users\\u271201\\Downloads\\Figures analysis\\5.Summer\\Tables\\RR Temperatures month PM10.csv", row.names = FALSE)


theme_set(bayesplot::theme_default())
pos <- position_nudge(y = ifelse(all_results$group == "Male", 0, 0.3))

summer_month <- ggplot(all_results, aes(x = rr, y =id , color = group)) + 
  geom_point(position = pos) +
  geom_linerange(aes(xmin = rr.lci, xmax = rr.uci), position = pos) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "black", size=1) +  # Reference line # Facet by group with separate x axes
  theme_bw(base_size = 11) +  # Theme
  theme(
    axis.text.y = element_text(size = 11, colour = "black"),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 11, angle = 0, hjust = 1, colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", colour = "black"),
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    strip.background = element_rect(fill = "white", colour = "black"),
    strip.text = element_text(size = 11, face = "bold")
  ) +
  labs(
    title = "Relative Risk by Month and Year Interaction",
    x = "Relative Risk (RR)",
    y = ""
  ) +
  scale_color_brewer(palette = "Set1") 
ggsave("C:\\Users\\u271201\\Downloads\\Figures analysis\\5.Summer\\Figures\\PM10\\months.eps", height = 20, width = 20, units = "cm")
################################################################################
################################################################################
load("C:\\Users\\u271201\\Downloads\\output\\Mort_Female_65_\\PM10_Month_rf_nuts.RData")
model1 <- model
load("C:\\Users\\u271201\\Downloads\\output\\Mort_Male_65_\\PM10_Month_rf_nuts.RData")
model2 <- model 

# Create a list to store the results data frames
results_list <- list()

# List of model names for reference
mod.names <- c("model1", "model2")

# Loop through the model names, calculate the relative risks, and store results
for (j in seq_along(mod.names)) {
  # Obtain the model from the name
  model <- get(mod.names[j])
  
  id <- model$summary.random$`interaction(NUTS3, YearMonth)`$ID
  rr <- exp(model$summary.random$`interaction(NUTS3, YearMonth)`$mean)
  rr.lci<- exp(model$summary.random$`interaction(NUTS3, YearMonth)`$"0.025quant")
  rr.uci <- exp(model$summary.random$`interaction(NUTS3, YearMonth)`$"0.975quant")
  
  results <- data.frame(id = id, rr = rr, rr.lci=rr.lci,rr.uci = rr.uci,  group = mod.names[j] )
  # Assuming your data is in a data frame called 'results'
  interaction_data <- results[grep("ES[0-9][0-9][0-9].[1-7].[5-8]", results$id), ]
  # Extract the NUTS3 code using str_extract and a regex that captures the pattern
  interaction_data$NUTS3 <- str_extract(interaction_data$id, "ES\\d+")
  # Add the subsetted data to the results list
  results_list[[mod.names[j]]] <- interaction_data
}
# Now results_list has two data frames: one for each model
# Combine them for plotting
all_results <- do.call(rbind, results_list)

# Update the transformation with debugging info and improved logic
all_results <- all_results %>%
  mutate(
    # Extract the last 'number.number' from 'id'
    id_numeric = as.numeric(str_extract(id, "\\d+\\.\\d+$")),
    
    # Debug: View raw decimal parts before any manipulation
    raw_decimal = id_numeric %% 1,
    
    # Adjust month_code calculation to handle edge cases more gracefully
    month_code = round(raw_decimal * 10),
    
    # Debug: Check what values month_code takes
    check_month_code = month_code,
    
    # Calculate year
    year = if_else(!is.na(id_numeric), 2016 + floor(id_numeric), NA_integer_),
    
    # Determine month from month_code
    month = case_when(
      month_code == 5 ~ "May",
      month_code == 6 ~ "June",
      month_code == 7 ~ "July",
      month_code == 8 ~ "August",
      TRUE ~ "Invalid month"  # Handling unexpected month_code values
    ),
    
    # Create the new 'date_category' field
    date_category = if_else(month != "Invalid month" & !is.na(year), paste(month, year), "Invalid ID"),
    
    # Rename groups
    group = case_when(
      group == "model1" ~ "Female",
      group == "model2" ~ "Male"
    )
  ) %>%
  select(-id_numeric, -year, -raw_decimal, -check_month_code)  # Optionally keep these for debugging
write.csv(all_results, "C:\\Users\\u271201\\Downloads\\Figures analysis\\5.Summer\\Tables\\RR Temperatures month nuts PM10.csv", row.names = FALSE)



pos <- position_nudge(y = ifelse(all_results$group == "Male", 0, 0.3))

summer_month_nuts <- 
  all_results %>% 
  group_by(rr,rr.lci,rr.uci,NUTS3) %>%
  # add the predefined state grid by state code
  left_join(grid, by = c("NUTS3" = "code_num")) %>%
  ggplot(aes(x = rr, y =id, color = group)) + 
  geom_point(position = pos) +
  geom_linerange(aes(xmin = rr.lci, xmax = rr.uci), position = pos) +
  scale_y_discrete(labels = setNames(all_results$date_category, all_results$id)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "black", size=1) +  # Reference line # Facet by group with separate x axes
  theme_bw(base_size = 8) +  # Theme
  theme(
    axis.text.y = element_text(size = 8, colour = "black"),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 8, angle = 0, hjust = 1, colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", colour = "black"),
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    strip.background = element_rect(fill = "grey", colour = "black"),
    strip.text = element_text(size = 8, face = "bold"),
    plot.background = element_rect(fill = "aliceblue", colour = NA) 
  ) +
  labs(
    title = "Relative Risk by Month and Year Interaction",
    x = "Relative Risk (RR)",
    y = "date_category"
  ) +
  scale_color_brewer(palette = "Set1") +
  # organise by state name in grid file
  facet_geo( ~name, grid = grid, scales = "free_y")

ggsave("C:\\Users\\u271201\\Downloads\\Figures analysis\\5.Summer\\Figures\\PM10\\months_nuts.eps", height = 70, width = 100, units = "cm")

################################################################################
                                  "combined"
################################################################################
load("C:\\Users\\u271201\\Downloads\\output\\Mort_Female_65_\\PM10_Month_rf.RData")
PM10_Female <- model
load("C:\\Users\\u271201\\Downloads\\output\\Mort_Male_65_\\PM10_Month_rf.RData")
PM10_Male <- model 
load("C:\\Users\\u271201\\Downloads\\output\\Mort_Female_65_\\PM25_Month_rf.RData")
PM25_Female <- model
load("C:\\Users\\u271201\\Downloads\\output\\Mort_Male_65_\\PM25_Month_rf.RData")
PM25_Male <- model 
load("C:\\Users\\u271201\\Downloads\\output\\Mort_Female_65_\\O3_Month_rf.RData")
O3_Female <- model
load("C:\\Users\\u271201\\Downloads\\output\\Mort_Male_65_\\O3_Month_rf.RData")
O3_Male <- model 
load("C:\\Users\\u271201\\Downloads\\output\\Mort_Female_65_\\NO2_Month_rf.RData")
NO2_Female <- model
load("C:\\Users\\u271201\\Downloads\\output\\Mort_Male_65_\\NO2_Month_rf.RData")
NO2_Male <- model 

# Create a list to store the results data frames
results_list <- list()

# List of model names for reference
mod.names <- c("PM10_Female", "PM10_Male","PM25_Female", "PM25_Male","O3_Female", "O3_Male","NO2_Female", "NO2_Male")
# Loop through the model names, calculate the relative risks, and store results
# Loop through the model names, calculate the relative risks, and store results
for (j in seq_along(mod.names)) {
  # Obtain the model from the name
  model <- get(mod.names[j])
  
  # Extract the data from the model and calculate relative risks
  id <- model$summary.random$YearMonth$ID
  rr <- exp(model$summary.random$YearMonth$mean)
  rr.lci <- exp(model$summary.random$YearMonth$"0.025quant")
  rr.uci <- exp(model$summary.random$YearMonth$"0.975quant")
  
  # Create a data frame of the results
  results <- data.frame(id = id, rr = rr, rr.lci = rr.lci, rr.uci = rr.uci, group = mod.names[j])
  
  # Subset the results if necessary (assumed based on your code)
  interaction_data <- results[grep("[1-8].[5-8]", results$id), ]
  
  # Add the subsetted data to the results list
  results_list[[mod.names[j]]] <- interaction_data
}

# Now results_list has two data frames: one for each model
# Combine them for plotting
all_results <- do.call(rbind, results_list)
# Assuming all_results is your data frame
all_results <- all_results %>%
  mutate(
    # Ensure 'id' is numeric
    id_numeric = as.numeric(as.character(id)),
    # Add rounding to handle potential floating-point precision issues
    year = 2016 + floor(id_numeric),
    month_code = round((id_numeric %% 1) * 10, 0),  # Multiply by 10 and round to handle decimals like 1.5, 1.6  # Adjust the base year to 2017
    month = case_when(
      month_code == 5 ~ "May",
      month_code == 6 ~ "June",
      month_code == 7 ~ "July",
      month_code == 8 ~ "August",
      TRUE ~ "Unknown"  # Handling any unexpected values
    ),#Combine the new year and month into a new 'id'
    id = paste( year, month)
  ) %>%
  # Now perform the separation of the 'group' column
  separate(group, into = c("pollutant", "gender"), sep = "_") %>%
  # Further mutate to standardize names
  mutate(
    # Standardize gender names
    gender = case_when(
      gender == "Female" ~ "Female",
      gender == "Male" ~ "Male",
      TRUE ~ "Unknown"  # Fallback in case there are any unexpected formats
    ),
    # Update pollutant names if necessary (optional)
    pollutant = case_when(
      pollutant == "PM10" ~ "PM10",
      pollutant == "PM25" ~ "PM2.5",  # Correcting to a more common notation
      pollutant == "O3" ~ "Ozone",
      pollutant == "NO2" ~ "Nitrogen Dioxide",
      TRUE ~ "Other"  # Handling any unexpected values
    )
  ) %>%
  select( -month_code, -id_numeric)  # Clean up by removing intermediate columns


# Convert the 'id' column to a numeric factor that represents the y-position on the plot
all_results <- all_results %>%
  mutate(
    id_numeric = as.numeric(factor(id))
  )

# Calculate the nudge amount for each observation based on conditions
all_results <- all_results %>%
  mutate(
    nudge_amount = case_when(
      gender == "Male" & pollutant == "PM2.5" ~ 0.1,
      gender == "Male" & pollutant == "PM10" ~ 0.4,
      gender == "Male" & pollutant == "Ozone" ~ 0.3,
      gender == "Male" & pollutant == "Nitrogen Dioxide" ~ 0.2,
      gender == "Female" & pollutant == "PM2.5" ~ -0.1,
      gender == "Female" & pollutant == "PM10" ~ -0.4,
      gender == "Female" & pollutant == "Ozone" ~ -0.3,
      gender == "Female" & pollutant == "Nitrogen Dioxide" ~ -0.2,
      TRUE ~ 0  # Default no nudge
    )
  )

# Now create the plot
summer_month <- ggplot(all_results, aes(x = rr, y = id, color = pollutant, shape = gender)) +  # Add shape aesthetic
  geom_errorbar(
    aes(ymin = id_numeric + nudge_amount, ymax = id_numeric + nudge_amount , xmin = rr.lci, xmax = rr.uci),
    width = 0.1
  ) +
  geom_point(aes(y = id_numeric + nudge_amount), position = position_dodge(width = 0)) +  # Dodge positions to avoid overlap
  geom_vline(xintercept = 1, linetype = "dashed", color = "black", size = 1 ) +
  scale_y_discrete(labels = setNames(all_results$id, all_results$id_numeric)) +
  theme_bw() +
  labs(
    title = "Relative Risk by Month and Year Interaction",
    x = "Relative Risk (RR)",
    y = "ID"
  ) +
  scale_color_brewer(palette = "Set1") +
  scale_shape_manual(values = c(16, 17)) +  # Manual shape values, can be adjusted
    theme(
      axis.text.y = element_text(size = rel(0.8), colour = "black"),
      axis.title.y = element_blank(),
      axis.text.x = element_text(size = 11, angle = 0, hjust = 1, colour = "black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white", colour = "black"),
      plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
      strip.background = element_rect(fill = "white", colour = "black"),
      strip.text = element_text(size = 11, face = "bold")
  ) +
  guides(color = guide_legend(title = "Pollutant"), shape = guide_legend(title = "Gender"))
ggsave("C:\\Users\\u271201\\Downloads\\Figures analysis\\5.Summer\\Figures\\months_COMBINED.eps", height = 30, width = 20, units = "cm")
# Print the plot
print(summer_month)

write.csv(all_results, "C:\\Users\\u271201\\Downloads\\Figures analysis\\5.Summer\\Tables\\RR Temperatures month pollutants.csv", row.names = FALSE)
################################################################################

################################################################################
load("C:\\Users\\u271201\\Downloads\\output\\Mort_Female_65_\\PM10_Month_rf_nuts.RData")
PM10_Female <- model
load("C:\\Users\\u271201\\Downloads\\output\\Mort_Male_65_\\PM10_Month_rf_nuts.RData")
PM10_Male <- model 
load("C:\\Users\\u271201\\Downloads\\output\\Mort_Female_65_\\PM25_Month_rf_nuts.RData")
PM25_Female <- model
load("C:\\Users\\u271201\\Downloads\\output\\Mort_Male_65_\\PM25_Month_rf_nuts.RData")
PM25_Male <- model 
load("C:\\Users\\u271201\\Downloads\\output\\Mort_Female_65_\\O3_Month_rf_nuts.RData")
O3_Female <- model
load("C:\\Users\\u271201\\Downloads\\output\\Mort_Male_65_\\O3_Month_rf_nuts.RData")
O3_Male <- model 
load("C:\\Users\\u271201\\Downloads\\output\\Mort_Female_65_\\NO2_Month_rf_nuts.RData")
NO2_Female <- model
load("C:\\Users\\u271201\\Downloads\\output\\Mort_Male_65_\\NO2_Month_rf_nuts.RData")
NO2_Male <- model 

# Create a list to store the results data frames
results_list <- list()

# List of model names for reference
mod.names <- c("PM10_Female", "PM10_Male","PM25_Female", "PM25_Male","O3_Female", "O3_Male","NO2_Female", "NO2_Male")

# Loop through the model names, calculate the relative risks, and store results
for (j in seq_along(mod.names)) {
  # Obtain the model from the name
  model <- get(mod.names[j])
  
  id <- model$summary.random$`interaction(NUTS3, YearMonth)`$ID
  rr <- exp(model$summary.random$`interaction(NUTS3, YearMonth)`$mean)
  rr.lci<- exp(model$summary.random$`interaction(NUTS3, YearMonth)`$"0.025quant")
  rr.uci <- exp(model$summary.random$`interaction(NUTS3, YearMonth)`$"0.975quant")
  
  results <- data.frame(id = id, rr = rr, rr.lci=rr.lci,rr.uci = rr.uci,  group = mod.names[j] )
  # Assuming your data is in a data frame called 'results'
  interaction_data <- results[grep("ES[0-9][0-9][0-9].[1-7].[5-8]", results$id), ]
  # Extract the NUTS3 code using str_extract and a regex that captures the pattern
  interaction_data$NUTS3 <- str_extract(interaction_data$id, "ES\\d+")
  # Add the subsetted data to the results list
  results_list[[mod.names[j]]] <- interaction_data
}
# Now results_list has two data frames: one for each model
# Combine them for plotting
# Assuming results_list is already defined and contains your data
all_results <- do.call(rbind, results_list)

# Update the transformation with debugging info and improved logic
all_results <- all_results %>%
  mutate(
    # Extract the last 'number.number' from 'id'
    id_numeric = as.numeric(str_extract(id, "\\d+\\.\\d+$")),
    
    # Debug: View raw decimal parts before any manipulation
    raw_decimal = id_numeric %% 1,
    
    # Adjust month_code calculation to handle edge cases more gracefully
    month_code = round(raw_decimal * 10),
    
    # Debug: Check what values month_code takes
    check_month_code = month_code,
    
    # Calculate year
    year = if_else(!is.na(id_numeric), 2016 + floor(id_numeric), NA_integer_),
    
    # Determine month from month_code
    month = case_when(
      month_code == 5 ~ "May",
      month_code == 6 ~ "June",
      month_code == 7 ~ "July",
      month_code == 8 ~ "August",
      TRUE ~ "Invalid month"  # Handling unexpected month_code values
    ),
    
    # Create the new 'date_category' field
    date_category = if_else(month != "Invalid month" & !is.na(year), paste(month, year), "Invalid ID")
  ) %>%
  # Separate 'group' into 'pollutant' and 'Gender'
  separate(group, into = c("pollutant", "gender"), sep = "_")
    

# Step 3: Filter the data to isolate the rows where year is 2022
data_2022 <- all_results %>% filter(year == 2022)

# Calculate the nudge amount for each observation based on conditions
data_2022 <- data_2022 %>%
  mutate(
    nudge_amount = case_when(
      gender == "Male" & pollutant == "PM25" ~ 0.1,
      gender == "Male" & pollutant == "PM10" ~ 0.4,
      gender == "Male" & pollutant == "O3" ~ 0.3,
      gender == "Male" & pollutant == "NO2" ~ 0.2,
      gender == "Female" & pollutant == "PM25" ~ -0.1,
      gender == "Female" & pollutant == "PM10" ~ -0.4,
      gender == "Female" & pollutant == "O3" ~ -0.3,
      gender == "Female" & pollutant == "NO2" ~ -0.2,
      TRUE ~ 0  # Default no nudge
    )
  )

# Update the transformation and plotting code
summer_month_nuts_22 <- data_2022 %>%
  group_by(rr, rr.lci, rr.uci, NUTS3) %>%
  # Add the predefined state grid by state code
  left_join(grid, by = c("NUTS3" = "code_num")) %>%
  ggplot(aes(x = rr, y = date_category, color = pollutant, shape = gender)) +
  geom_errorbar(
    aes(ymin = as.numeric(as.factor(date_category)) + nudge_amount, 
        ymax = as.numeric(as.factor(date_category)) + nudge_amount, 
        xmin = rr.lci, xmax = rr.uci),
    width = 0.1,
    position = position_dodge(width = 0.6)  # Adjust dodge width to ensure separation
  ) +
  geom_point(aes(y = as.numeric(as.factor(date_category)) + nudge_amount), 
             position = position_dodge(width = 0.5), 
             size = 2) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "black", size = 1) +
  scale_y_discrete(labels = setNames(data_2022$date_category, as.factor(data_2022$date_category))) +
  theme_bw() +
  labs(
    title = "Relative Risk by Month and Year Interaction",
    x = "Relative Risk (RR)",
    y = "Date Category"
  ) +
  scale_color_brewer(palette = "Set1") +
  scale_shape_manual(values = c(16, 17)) +  # Manual shape values, can be adjusted
  theme(
    axis.text.y = element_text(size = rel(0.8), colour = "black"),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 11, angle = 0, hjust = 1, colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", colour = "black"),
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    strip.background = element_rect(fill = "white", colour = "black"),
    strip.text = element_text(size = 11, face = "bold")
  ) +
  guides(color = guide_legend(title = "Pollutant"), shape = guide_legend(title = "Gender")) +
  # Organize by state name in grid file
  facet_geo(~name, grid = grid, scales = "free_y")

# Save and display the plot
ggsave("C:\\Users\\u271201\\Downloads\\Figures analysis\\5.Summer\\Figures\\months_COMBINED_nuts.eps", height = 60, width = 90, units = "cm")

