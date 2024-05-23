# R script to visualise INLA-DLMN model output

# Step 0: load packages and pre-processed data
# Step 1: compare models using goodness of fit statistics
# Step 2: visualise random effects for selected model
# Step 3: compare selected model to the baseline model using the mean absolute error
# Step 4: compare cross-validated posterior predictions to observations


# Step 0: load packages and pre-processed data

source("00_load_packages_data_Thesis.R")

# Step 1: compare models using goodness of fit statistics.

# create a table to store model adequacy results (DIC, CV log score, MAE difference results)
# create model label string

mod.name <- c("basemodel", 
         "model0.1", "model0.2", "model0.3", "model0.4", "model0.5", "model0.6","model0.7", "model0.8", "model0.9",  
         "model1.1_PM10_interaction", "model1.2_PM10_interaction", "model1.3_PM10_interaction")

table1 <- data.table(Model = mod.name, 
                     DIC = NA,
                     logscore = NA)

# create loop to read in model object and extract DIC and CV log score
for (i in 1:length(mod.name))
{
  load(paste0("C:\\Users\\u271201\\Downloads\\output\\Mort_Male_65_\\", mod.name[i],".RData"))
  # add model fit statistics from each model to table
  table1$DIC[i] <- round(model$dic$dic,0)
  table1$logscore[i] <- round(-mean(log(model$cpo$cpo), na.rm = T), 3)
}

# load baseline and selected model
load("C:\\Users\\u271201\\Downloads\\output\\Mort_Male_65_\\basemodel.RData")
basemodel <- model
load("C:\\Users\\u271201\\Downloads\\output\\Mort_Male_65_\\model0.7.RData")
tm_NO2 <- model



# Step 2: visualise random effects for selected model
month_breaks <- c(1,5,9,14,18,22,27,31, 35, 40, 44, 48)  # Week numbers where months approximately start
month_labels <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
# explore spatial and temporal random effects 

# plot monthly random effects per state (Appendix Fig S7)
week_effects <- data.table(cbind(rep(unique(data$NUTS3), each = 52),
                                     model$summary.random$T1))
names(week_effects)[1:2] <- c("NUTS3", "week")

# plot monthly random effects per state
week_effects %>% 
  # add the predefined state grid by state code
  left_join(grid, by = c("NUTS3" = "code_num")) %>% 
  ggplot() + 
  geom_ribbon(aes(x = week, ymin = `0.025quant`, ymax = `0.975quant`), 
              fill = "cadetblue4", alpha = 0.5) + 
  geom_line(aes(x = week, y = `mean`), col = "cadetblue4") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey70") +
  xlab("Month") +
  ylab("Contribution to log(DIR)") +
  scale_y_continuous() +
  scale_x_continuous(breaks = c(1,5,9,14,18,22,27,31, 35, 40, 44, 48), labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  theme_bw() + 
  # organise by state name in grid file
  facet_geo( ~name, grid = grid)
  
ggsave("C:\\Users\\u271201\\Downloads\\fig_S06_month_effect.eps", height = 60, width = 120, units = "cm")


# make maps of spatial random effects per year (Appendix Fig S8)
# extract posterior mean estimates for combined unstructured and structured random effects
  
space <- data.table(model$summary.random$S1)
total_units <- nrow(space)
years <- max(data$year) - min(data$year) + 1  # inclusive count of years
space$year <- rep(min(data$year):max(data$year), each = total_units / years)
# Recycling explicitly, if that is your intention
space$re <- rep(c(rep(1, nnuts3), rep(2, nnuts3)), times = nyear, length.out = nrow(space))
space$NUTS3 <- rep(unique(data$NUTS3), length.out = total_units)
mn <-min(space$mean)
mx <-max(space$mean)

# Add the map geometry to the space dataframe
space <- left_join(map, space, by = c("NUTS_ID" = "NUTS3"))
space_effects <- ggplot() + 
  geom_sf(data = space, aes(fill = mean), lwd = 0, color = NA) +
  scale_fill_distiller(palette = "PRGn", direction = -1, 
                       limits = c(min(mn,-mx),max(mx,-mn))) +
  labs(fill = "Contribution to \n log(DIR)") +
  theme_void() +
  facet_wrap(~space$year, ncol = 5)
  
ggsave("C:\\Users\\u271201\\Downloads\\fig_S07_year_spatial_effect.eps", height = 20, width = 30, units = "cm")


# Step 3: compare selected model to the baseline model using the mean absolute error

# add baseline fitted model result summaries (2.5%, 50%, 97.5% percentiles) to data
data$base.fit <- basemodel$summary.fitted.values$`0.5quant`
data$base.fit.lci <- basemodel$summary.fitted.values$`0.025quant`
data$base.fit.uci <- basemodel$summary.fitted.values$`0.975quant`

# add selected fitted model result summaries (2.5%, 50%, 97.5% percentiles) to data
data$fit <- model$summary.fitted.values$`0.5quant`
data$fit.lci<-model$summary.fitted.values$`0.025quant`
data$fit.uci<-model$summary.fitted.values$`0.975quant`

# compute mean absolute error and compare base model to final model
# Initialize vectors to store MAE values
MAE <- as.data.frame(matrix(NA, nrow = nnuts3, ncol = 2))
MAE$base <- numeric(length(unique(data$nuts3_index)))
MAE$new <- numeric(length(unique(data$nuts3_index)))

  
  # Loop through unique micro_index values
  unique_indices <- unique(data$nuts3_index)
  for (i in unique_indices) {
    # Calculate MAE using base R
    actual_base <- data$Mort_Male[data$nuts3_index == i]
    predicted_base <- data$base.fit[data$nuts3_index == i]
    MAE$base[i] <- mean(abs(actual_base - predicted_base), na.rm = TRUE)
    
    
    actual_new <- data$Mort_Male[data$nuts3_index == i]
    predicted_new <- data$fit[data$nuts3_index == i]
    MAE$new[i] <- mean(abs(actual_new - predicted_new), na.rm = TRUE)
    
  }

# calculate difference between MAE from the baseline model and MAE from the selected model
MAE$diff <- MAE$base - MAE$new
mn <-min(MAE$diff)
mx <-max(MAE$diff)

MAE$value <- 1
# assign areas where the difference is zero or more as '2', i.e. MAE from new model is smaller (fits better than the base model)
MAE$value[MAE$diff >= 0 ] <- 2

# plot map to show areas where the new model provided 'added value' over the basemodel (e.g. MAE is smaller for new model) (Appendix Fig S11)
value_map <- ggplot(map) + 
  geom_sf(aes(fill = factor(MAE$value)), lwd = 0) +
  scale_fill_manual(values = c("#17becf","#dc5fbd"), breaks = 1:2, 
                    labels = c("No added value", "Added value")) +
  labs(fill = "") +
  theme_void()

ggsave(value_map, filename = "C:\\Users\\u271201\\Downloads\\fig_S08_value_map.pdf")

# create a table to store the number and proportion of microregions where the selected model fits better than the baseline model
tableS2 <- as.data.frame(matrix(NA, 7, 3))
# add column names to table
names(tableS2) <- c("Region", "Added value", "Total")
# add model names to first column
tableS2[,1] <-  c("North", "Northeast", "Southeast", "South", "Centre-West", "Brazil")

# extract region names for first time step
region <- data$NUTS3[data$time == 1]
# create unique region name vector
k <- unique(region)

for (i in 1:length(k))
{
  
  # number of microregions where the selected model fits better than the baseline model
  tableS2$`Added value`[i] <- length(MAE$value[MAE$value == 2 & region == k[i]])
  # total number of microregions
  tableS2$Total[i] <- length(region[region == k[i]])
  
}

# Brazil
tableS2[6,2] <- length(MAE$value[MAE$value == 2])
tableS2[6,3] <- nnuts3

# calculate proportion
tableS2$Proportion <- round(tableS2$`Added value`/tableS2$Total *100, 0)

tableS2

# Appendix Table S2
write.csv(tableS2, file = "figs/table_S02.csv", quote = FALSE, 
          row.names = FALSE) 


# Step 4: compare cross-validated posterior predictions to observations

# add cross-validation posterior predictive summaries (already processed) to data
# see output/preds/ for script to perform cross-validation in parallel (note, very computationally intensive)

data$pred.mean <- NA
data$pred.median <- NA
data$pred.lci <- NA
data$pred.uci <- NA

for (i in 1:2)
{
  for (j in 1:12)
    
  {
    load(paste0("C:\\Users\\u271201\\Downloads\\output\\preds\\preds_",2016 + i,"_",j,".RData"))
    
    data$pred.mean[preds$idx.pred] <- preds$mean
    data$pred.median[preds$idx.pred] <- preds$median
    data$pred.lci[preds$idx.pred] <- preds$lci
    data$pred.uci[preds$idx.pred] <- preds$uci
  }
}


# plot maps of posterior median dengue incidence rate (DIR) per 100,000 population per year (Appendix Fig S9)
dir_year <- 
  data %>% 
  group_by(year, NUTS3) %>%
  # calculate annual incidence rate
  summarise( cases = sum(pred.mean),
             pop = sum(Popu_Male)) %>% 
  mutate(var = cases / pop * 10^5) %>%
  # Add the map
  left_join(map, ., by = c("NUTS_ID" = "NUTS3")) %>% 
  ggplot() + 
  geom_sf(aes(fill = var), lwd = 0, color = NA) +
  scale_fill_gradientn(name = "DIR", colours = brewer.pal(9, "PuRd"), 
                       trans = "log1p", breaks = c(0, 10, 100, 300, 1000), 
                       labels = c(0, 10, 100, 300, 1000) ) + 
  theme_void() +
  facet_wrap(~year, ncol = 5)

ggsave("C:\\Users\\u271201\\Downloads\\fig_S09_pred_DIR_2001_2019.pdf", height = 20, width = 30, units = "cm")

# plot observed v fitted DIR per state (Appendix Fig S10)
dir_ts_state <- 
  data %>% 
  group_by(time, NUTS3) %>%
  # calculate state level incidence rate
  summarise(cases = mean(Mort_Male),
            fit = mean(pred.mean), 
            fit.lci = mean(pred.lci),
            fit.uci = mean(pred.uci),
            pop = mean(Popu_Male)) %>% 
  mutate(inc = cases / pop * 10^5,
         inc.fit = fit / pop * 10^5,
         inc.fit.lci = fit.lci / pop * 10^5,
         inc.fit.uci = fit.uci / pop * 10^5) %>% 
  # add the predefined state grid by state code
  left_join(grid, by = c(c("NUTS3" = "code_num"))) %>% 
  ggplot() + 
  geom_ribbon(aes(x = time, ymin = inc.fit.lci, ymax = inc.fit.uci), 
              fill = "#D37295", alpha = 0.5) + 
  geom_line(aes(x = time, y = inc.fit, col = "#D37295")) +
  geom_line(aes(x = time, y = inc, col = "#499894")) +
  xlab("Time") +
  ylab("DIR") +
  scale_colour_identity(name = "",
                        breaks = c("#499894", "#D37295"),
                        labels = c("Observed", "Posterior predicted"),
                        guide = "legend") +
  scale_x_continuous(breaks = seq(1, ntime, 12*4), labels = seq(min(data$year), max(data$year), 4)) +
  scale_y_continuous(breaks = c(10, 100, 1000), labels = c(10, 100, 1000), 
                     trans = "log1p") + 
  theme_bw() + 
  theme(axis.text.x = element_text(size = rel(0.85)), legend.position = "bottom") +
  # organise by state name in grid file
  facet_geo( ~name, grid = grid)

ggsave("C:\\Users\\u271201\\Downloads\\fig_S10_dir_obs_pred_state.pdf", height = 30, width = 25, units = "cm")
