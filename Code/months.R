df$YearMonth <- interaction(df$Year, df$Month)


formula <- Y ~ 1 + 
  f(T1, replicate = S2, model = "rw1", cyclic = TRUE, constr = TRUE, 
    scale.model = TRUE, hyper = precision.prior) +
  f(S1, model = "bym2", replicate = T2, graph = "C:\\Users\\u271201\\Downloads\\Data\\Sample test\\map.graph", 
    scale.model = TRUE, hyper = precision.prior) +
  basis_MaxTm + basis_PM25 + f(YearMonth, model = "iid") 

# create a function to run a model for each formula in the list and save the model output to file
# WARNING: this may take a long time to run
 model <- mymodel(formula, df)
                   save(model, file = paste0("C:\\Users\\u271201\\Downloads\\output\\Mort_Female_65_79\\PM25_Month_rf.RData"))




load("C:\\Users\\u271201\\Downloads\\output\\Mort_Male_65_\\PM25_Month_rf.RData")
model5 <- model
summary(model5)
model$summary.random

id <- model5$summary.random$YearMonth$ID
rr <- exp(model5$summary.random$YearMonth$mean)
rr.lci<- exp(model5$summary.random$YearMonth$"0.025quant")
rr.uci <- exp(model5$summary.random$YearMonth$"0.975quant")

results <- data.frame(id = id, rr = rr, rr.lci=rr.lci,rr.uci = rr.uci )
# Assuming your data is in a data frame called 'results'
interaction_data <- results[grep("[1-8].[5-8]", results$id), ]


lancet_plot <- ggplot(interaction_data, aes(x = id, y = rr)) +
  geom_point(size=2, alpha=1, show.legend=FALSE) +  # Optimal size and slightly transparent for overlap
  geom_errorbar(aes(ymin=rr.lci, ymax=rr.uci), colour="black", width=0.3, size=0.8) +  # More professional color and thickness
  geom_hline(yintercept = 1, linetype = "dashed", color = "black", size=1) +  # Prominent reference line
  coord_flip() +  # Horizontal plot for better label readability
  theme_bw(base_size = 11) +  # Increased base font size for better clarity
  theme(
    axis.text.y = element_text(size = 11, colour = "black"),  # Enhanced y-axis text
    axis.title.y = element_blank(),  # Cleaner y-axis
    axis.text.x = element_text(size = 11, angle = 0, colour = "black"),  # Enhanced x-axis text readability
    panel.grid.major = element_blank(),  # Clean major grid lines
    panel.grid.minor = element_blank(),  # Clean minor grid lines
    panel.background = element_rect(fill = "white", colour = "black"),  # High-contrast background
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold")  # Prominent title
  ) +
  labs(
    title = "Relative Risk by Month and Year Interaction",
    x = "Relative Risk (RR)",
    y = ""
  ) 


lancet_plot 


formula <- Y ~ 1 + 
  f(T1, replicate = S2, model = "rw1", cyclic = TRUE, constr = TRUE, 
    scale.model = TRUE, hyper = precision.prior) +
  f(S1, model = "bym2", replicate = T2, graph = "C:\\Users\\u271201\\Downloads\\Data\\Sample test\\map.graph", 
    scale.model = TRUE, hyper = precision.prior) +
  basis_MaxTm + basis_PM25 + f(interaction(NUTS3, YearMonth), model = "iid")
  

# create a function to run a model for each formula in the list and save the model output to file
# WARNING: this may take a long time to run
model <- mymodel(formula, df)
save(model, file = paste0("C:\\Users\\u271201\\Downloads\\output\\Mort_Female_65_79\\PM25_Month_rf_nuts.RData"))
load("C:\\Users\\u271201\\Downloads\\output\\Mort_tot_65_\\NO2_Month_rf_nuts.RData")
model2 <- model
summary(model2)


model2$summary.random$`interaction(NUTS3, YearMonth)`$mean

id <- model2$summary.random$`interaction(NUTS3, YearMonth)`$ID
rr <- exp(model2$summary.random$`interaction(NUTS3, YearMonth)`$mean)
rr.lci<- exp(model2$summary.random$`interaction(NUTS3, YearMonth)`$"0.025quant")
rr.uci <- exp(model2$summary.random$`interaction(NUTS3, YearMonth)`$"0.975quant")

results <- data.frame(id = id, rr = rr, rr.lci=rr.lci,rr.uci = rr.uci )
# Assuming your data is in a data frame called 'results'
interaction_data <- results[grep("ES[0-9][0-9][0-9].[1-7].[5-8]", results$id), ]
# Extract the NUTS3 code using str_extract and a regex that captures the pattern
interaction_data$NUTS3 <- str_extract(interaction_data$id, "ES\\d+")




lancet_plot <- 
  interaction_data %>% 
  group_by(rr,rr.lci,rr.uci,NUTS3) %>%
  # add the predefined state grid by state code
  left_join(grid, by = c("NUTS3" = "code_num")) %>%
   ggplot(aes(x = id, y = rr, group = NUTS3)) +
  geom_point(size=0.2, colour=muted("black"), alpha=1, show.legend=FALSE) +  # Optimal size and slightly transparent for overlap
  geom_errorbar(aes(ymin=rr.lci, ymax=rr.uci,group = NUTS3), colour="black",size=0.02, width=0.3, linewidth=0.02) +  # More professional color and thickness
  geom_hline(yintercept = 1 , linetype = "dashed", color = "black", linewidth=1) +  # Prominent reference line
  coord_flip() +
  theme_bw() +  # Increased base font size for better clarity
  theme(
    axis.text.y = element_text(size = 5, colour = "black"),  # Enhanced y-axis tex  # Cleaner y-axis
    axis.text.x = element_text(size = 11, angle = 0, colour = "black"),  # Enhanced x-axis text readability
    panel.grid.major = element_blank(),  # Clean major grid lines
    panel.grid.minor = element_blank(),  # Clean minor grid lines
    panel.background = element_rect(fill = "white", colour = "black"),  # High-contrast background
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold")  # Prominent title
  ) + 
  labs(
    title = "Relative Risk by Month and Year Interaction",
    x = "Relative Risk (RR)",
    y = ""
  ) +
  # organise by state name in grid file
  facet_geo( ~name, grid = grid, scales = "free_y")

ggsave("C:\\Users\\u271201\\Downloads\\months.eps", height = 70, width = 100, units = "cm")



formula <- Y ~ 1 + 
  f(T1, replicate = S2, model = "rw1", cyclic = TRUE, constr = TRUE, 
    scale.model = TRUE, hyper = precision.prior) +
  f(S1, model = "bym2", replicate = T2, graph = "C:\\Users\\u271201\\Downloads\\Data\\Sample test\\map.graph", 
    scale.model = TRUE, hyper = precision.prior) +
  basis_MaxTm + basis_PM10 + f(YearMonth, model = "iid") 

# create a function to run a model for each formula in the list and save the model output to file
# WARNING: this may take a long time to run
model <- mymodel(formula, df)
save(model, file = paste0("C:\\Users\\u271201\\Downloads\\output\\Mort_Female_65_79\\PM10_Month_rf.RData"))




load("C:\\Users\\u271201\\Downloads\\output\\Mort_Female_65_79\\PM10_Month_rf.RData")
model5 <- model
summary(model5)
model$summary.random

id <- model5$summary.random$YearMonth$ID
rr <- exp(model5$summary.random$YearMonth$mean)
rr.lci<- exp(model5$summary.random$YearMonth$"0.025quant")
rr.uci <- exp(model5$summary.random$YearMonth$"0.975quant")

results <- data.frame(id = id, rr = rr, rr.lci=rr.lci,rr.uci = rr.uci )
# Assuming your data is in a data frame called 'results'
interaction_data <- results[grep("[1-8].[5-8]", results$id), ]


lancet_plot <- ggplot(interaction_data, aes(x = id, y = rr)) +
  geom_point(size=2, colour=muted("black"), alpha=1, show.legend=FALSE) +  # Optimal size and slightly transparent for overlap
  geom_errorbar(aes(ymin=rr.lci, ymax=rr.uci), colour="black", width=0.3, size=0.8) +  # More professional color and thickness
  geom_hline(yintercept = 1, linetype = "dashed", color = "black", size=1) +  # Prominent reference line
  coord_flip() +  # Horizontal plot for better label readability
  theme_bw(base_size = 11) +  # Increased base font size for better clarity
  theme(
    axis.text.y = element_text(size = 11, colour = "black"),  # Enhanced y-axis text
    axis.title.y = element_blank(),  # Cleaner y-axis
    axis.text.x = element_text(size = 11, angle = 0, colour = "black"),  # Enhanced x-axis text readability
    panel.grid.major = element_blank(),  # Clean major grid lines
    panel.grid.minor = element_blank(),  # Clean minor grid lines
    panel.background = element_rect(fill = "white", colour = "black"),  # High-contrast background
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold")  # Prominent title
  ) +
  labs(
    title = "Relative Risk by Month and Year Interaction",
    x = "Relative Risk (RR)",
    y = ""
  ) 


lancet_plot 


formula <- Y ~ 1 + 
  f(T1, replicate = S2, model = "rw1", cyclic = TRUE, constr = TRUE, 
    scale.model = TRUE, hyper = precision.prior) +
  f(S1, model = "bym2", replicate = T2, graph = "C:\\Users\\u271201\\Downloads\\Data\\Sample test\\map.graph", 
    scale.model = TRUE, hyper = precision.prior) +
  basis_MaxTm + basis_PM10 + f(interaction(NUTS3, YearMonth), model = "iid")


# create a function to run a model for each formula in the list and save the model output to file
# WARNING: this may take a long time to run
model <- mymodel(formula, df)
save(model, file = paste0("C:\\Users\\u271201\\Downloads\\Mort_Female_65_79\\PM10_Month_rf_nuts.RData"))
load("C:\\Users\\u271201\\Downloads\\Mort_Female_65_79\\PM10_Month_rf_nuts.RData")
model2 <- model
summary(model2)


model2$summary.random$`interaction(NUTS3, YearMonth)`$mean

id <- model2$summary.random$`interaction(NUTS3, YearMonth)`$ID
rr <- exp(model2$summary.random$`interaction(NUTS3, YearMonth)`$mean)
rr.lci<- exp(model2$summary.random$`interaction(NUTS3, YearMonth)`$"0.025quant")
rr.uci <- exp(model2$summary.random$`interaction(NUTS3, YearMonth)`$"0.975quant")

results <- data.frame(id = id, rr = rr, rr.lci=rr.lci,rr.uci = rr.uci )
# Assuming your data is in a data frame called 'results'
interaction_data <- results[grep("ES[0-9][0-9][0-9].[1-7].[5-8]", results$id), ]
# Extract the NUTS3 code using str_extract and a regex that captures the pattern
interaction_data$NUTS3 <- str_extract(interaction_data$id, "ES\\d+")




lancet_plot <- 
  interaction_data %>% 
  group_by(rr,rr.lci,rr.uci,NUTS3) %>%
  # add the predefined state grid by state code
  left_join(grid, by = c("NUTS3" = "code_num")) %>%
  ggplot(aes(x = id, y = rr, group = NUTS3)) +
  geom_point(size=0.2, alpha=1, show.legend=FALSE) +  # Optimal size and slightly transparent for overlap
  geom_errorbar(aes(ymin=rr.lci, ymax=rr.uci,group = NUTS3), colour="black",size=0.02, width=0.3, linewidth=0.02) +  # More professional color and thickness
  geom_hline(yintercept = 1 , linetype = "dashed", color = "black", linewidth=1) +  # Prominent reference line
  coord_flip() +
  theme_bw() +  # Increased base font size for better clarity
  theme(
    axis.text.y = element_text(size = 5, colour = "black"),  # Enhanced y-axis tex  # Cleaner y-axis
    axis.text.x = element_text(size = 11, angle = 0, colour = "black"),  # Enhanced x-axis text readability
    panel.grid.major = element_blank(),  # Clean major grid lines
    panel.grid.minor = element_blank(),  # Clean minor grid lines
    panel.background = element_rect(fill = "white", colour = "black"),  # High-contrast background
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold")  # Prominent title
  ) + 
  labs(
    title = "Relative Risk by Month and Year Interaction",
    x = "Relative Risk (RR)",
    y = ""
  ) +
  # organise by state name in grid file
  facet_geo( ~name, grid = grid, scales = "free_y")

ggsave("C:\\Users\\u271201\\Downloads\\months.eps", height = 70, width = 100, units = "cm")







formula <- Y ~ 1 + 
  f(T1, replicate = S2, model = "rw1", cyclic = TRUE, constr = TRUE, 
    scale.model = TRUE, hyper = precision.prior) +
  f(S1, model = "bym2", replicate = T2, graph = "C:\\Users\\u271201\\Downloads\\Data\\Sample test\\map.graph", 
    scale.model = TRUE, hyper = precision.prior) +
  basis_MaxTm + basis_O3 + f(YearMonth, model = "iid") 

# create a function to run a model for each formula in the list and save the model output to file
# WARNING: this may take a long time to run
model <- mymodel(formula, df)
save(model, file = paste0("C:\\Users\\u271201\\Downloads\\output\\Mort_Female_65_79\\O3_Month_rf.RData"))




load("C:\\Users\\u271201\\Downloads\\output\\Mort_Female_65_79\\O3_Month_rf.RData")
model5 <- model
summary(model5)
model$summary.random

id <- model5$summary.random$YearMonth$ID
rr <- exp(model5$summary.random$YearMonth$mean)
rr.lci<- exp(model5$summary.random$YearMonth$"0.025quant")
rr.uci <- exp(model5$summary.random$YearMonth$"0.975quant")

results <- data.frame(id = id, rr = rr, rr.lci=rr.lci,rr.uci = rr.uci )
# Assuming your data is in a data frame called 'results'
interaction_data <- results[grep("[1-8].[5-8]", results$id), ]


lancet_plot <- ggplot(interaction_data, aes(x = id, y = rr)) +
  geom_point(size=2, colour=muted("black"), alpha=1, show.legend=FALSE) +  # Optimal size and slightly transparent for overlap
  geom_errorbar(aes(ymin=rr.lci, ymax=rr.uci), colour="black", width=0.3, size=0.8) +  # More professional color and thickness
  geom_hline(yintercept = 1, linetype = "dashed", color = "black", size=1) +  # Prominent reference line
  coord_flip() +  # Horizontal plot for better label readability
  theme_bw(base_size = 11) +  # Increased base font size for better clarity
  theme(
    axis.text.y = element_text(size = 11, colour = "black"),  # Enhanced y-axis text
    axis.title.y = element_blank(),  # Cleaner y-axis
    axis.text.x = element_text(size = 11, angle = 0, colour = "black"),  # Enhanced x-axis text readability
    panel.grid.major = element_blank(),  # Clean major grid lines
    panel.grid.minor = element_blank(),  # Clean minor grid lines
    panel.background = element_rect(fill = "white", colour = "black"),  # High-contrast background
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold")  # Prominent title
  ) +
  labs(
    title = "Relative Risk by Month and Year Interaction",
    x = "Relative Risk (RR)",
    y = ""
  ) 


lancet_plot 


formula <- Y ~ 1 + 
  f(T1, replicate = S2, model = "rw1", cyclic = TRUE, constr = TRUE, 
    scale.model = TRUE, hyper = precision.prior) +
  f(S1, model = "bym2", replicate = T2, graph = "C:\\Users\\u271201\\Downloads\\Data\\Sample test\\map.graph", 
    scale.model = TRUE, hyper = precision.prior) +
  basis_MaxTm + basis_O3 + f(interaction(NUTS3, YearMonth), model = "iid")


# create a function to run a model for each formula in the list and save the model output to file
# WARNING: this may take a long time to run
model <- mymodel(formula, df)
save(model, file = paste0("C:\\Users\\u271201\\Downloads\\output\\Mort_Female_65_79\\O3_Month_rf_nuts.RData"))
load("C:\\Users\\u271201\\Downloads\\output\\Mort_Female_65_79\\O3_Month_rf_nuts.RData")
model2 <- model
summary(model2)


model2$summary.random$`interaction(NUTS3, YearMonth)`$mean

id <- model2$summary.random$`interaction(NUTS3, YearMonth)`$ID
rr <- exp(model2$summary.random$`interaction(NUTS3, YearMonth)`$mean)
rr.lci<- exp(model2$summary.random$`interaction(NUTS3, YearMonth)`$"0.025quant")
rr.uci <- exp(model2$summary.random$`interaction(NUTS3, YearMonth)`$"0.975quant")

results <- data.frame(id = id, rr = rr, rr.lci=rr.lci,rr.uci = rr.uci )
# Assuming your data is in a data frame called 'results'
interaction_data <- results[grep("ES[0-9][0-9][0-9].[1-7].[5-8]", results$id), ]
# Extract the NUTS3 code using str_extract and a regex that captures the pattern
interaction_data$NUTS3 <- str_extract(interaction_data$id, "ES\\d+")




lancet_plot <- 
  interaction_data %>% 
  group_by(rr,rr.lci,rr.uci,NUTS3) %>%
  # add the predefined state grid by state code
  left_join(grid, by = c("NUTS3" = "code_num")) %>%
  ggplot(aes(x = id, y = rr, group = NUTS3)) +
  geom_point(size=0.2, colour=muted("black"), alpha=1, show.legend=FALSE) +  # Optimal size and slightly transparent for overlap
  geom_errorbar(aes(ymin=rr.lci, ymax=rr.uci,group = NUTS3), colour="black",size=0.02, width=0.3, linewidth=0.02) +  # More professional color and thickness
  geom_hline(yintercept = 1 , linetype = "dashed", color = "black", linewidth=1) +  # Prominent reference line
  coord_flip() +
  theme_bw() +  # Increased base font size for better clarity
  theme(
    axis.text.y = element_text(size = 5, colour = "black"),  # Enhanced y-axis tex  # Cleaner y-axis
    axis.text.x = element_text(size = 11, angle = 0, colour = "black"),  # Enhanced x-axis text readability
    panel.grid.major = element_blank(),  # Clean major grid lines
    panel.grid.minor = element_blank(),  # Clean minor grid lines
    panel.background = element_rect(fill = "white", colour = "black"),  # High-contrast background
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold")  # Prominent title
  ) + 
  labs(
    title = "Relative Risk by Month and Year Interaction",
    x = "Relative Risk (RR)",
    y = ""
  ) +
  # organise by state name in grid file
  facet_geo( ~name, grid = grid, scales = "free_y")

ggsave("C:\\Users\\u271201\\Downloads\\months.eps", height = 70, width = 100, units = "cm")



formula <- Y ~ 1 + 
  f(T1, replicate = S2, model = "rw1", cyclic = TRUE, constr = TRUE, 
    scale.model = TRUE, hyper = precision.prior) +
  f(S1, model = "bym2", replicate = T2, graph = "C:\\Users\\u271201\\Downloads\\Data\\Sample test\\map.graph", 
    scale.model = TRUE, hyper = precision.prior) +
  basis_MaxTm + basis_NO2 + f(YearMonth, model = "iid") 

# create a function to run a model for each formula in the list and save the model output to file
# WARNING: this may take a long time to run
model <- mymodel(formula, df)
save(model, file = paste0("C:\\Users\\u271201\\Downloads\\output\\Mort_Female_65_79\\NO2_Month_rf.RData"))




load("C:\\Users\\u271201\\Downloads\\output\\Mort_Female_65_\\NO2_Month_rf.RData")
model5 <- model
summary(model5)
model$summary.random

id <- model5$summary.random$YearMonth$ID
rr <- exp(model5$summary.random$YearMonth$mean)
rr.lci<- exp(model5$summary.random$YearMonth$"0.025quant")
rr.uci <- exp(model5$summary.random$YearMonth$"0.975quant")

results <- data.frame(id = id, rr = rr, rr.lci=rr.lci,rr.uci = rr.uci )
# Assuming your data is in a data frame called 'results'
interaction_data <- results[grep("[1-8].[5-8]", results$id), ]


lancet_plot <- ggplot(interaction_data, aes(x = id, y = rr)) +
  geom_point(size=2, colour=muted("black"), alpha=1, show.legend=FALSE) +  # Optimal size and slightly transparent for overlap
  geom_errorbar(aes(ymin=rr.lci, ymax=rr.uci), colour="black", width=0.3, size=0.8) +  # More professional color and thickness
  geom_hline(yintercept = 1, linetype = "dashed", color = "black", size=1) +  # Prominent reference line
  coord_flip() +  # Horizontal plot for better label readability
  theme_bw(base_size = 11) +  # Increased base font size for better clarity
  theme(
    axis.text.y = element_text(size = 11, colour = "black"),  # Enhanced y-axis text
    axis.title.y = element_blank(),  # Cleaner y-axis
    axis.text.x = element_text(size = 11, angle = 0, colour = "black"),  # Enhanced x-axis text readability
    panel.grid.major = element_blank(),  # Clean major grid lines
    panel.grid.minor = element_blank(),  # Clean minor grid lines
    panel.background = element_rect(fill = "white", colour = "black"),  # High-contrast background
    plot.title = element_text(hjust = 0.5, size = 11, face = "bold")  # Prominent title
  ) +
  labs(
    title = "Relative Risk by Month and Year Interaction",
    x = "Relative Risk (RR)",
    y = ""
  ) 


lancet_plot 
ggsave("C:\\Users\\u271201\\Downloads\\months.eps", height = 10, width = 10, units = "cm")

formula <- Y ~ 1 + 
  f(T1, replicate = S2, model = "rw1", cyclic = TRUE, constr = TRUE, 
    scale.model = TRUE, hyper = precision.prior) +
  f(S1, model = "bym2", replicate = T2, graph = "C:\\Users\\u271201\\Downloads\\Data\\Sample test\\map.graph", 
    scale.model = TRUE, hyper = precision.prior) +
  basis_MaxTm + basis_NO2 + f(interaction(NUTS3, YearMonth), model = "iid")


# create a function to run a model for each formula in the list and save the model output to file
# WARNING: this may take a long time to run
model <- mymodel(formula, df)
save(model, file = paste0("C:\\Users\\u271201\\Downloads\\output\\Mort_Female_65_79\\NO2_Month_rf_nuts.RData"))
load("C:\\Users\\u271201\\Downloads\\output\\Mort_Female_65_\\NO2_Month_rf_nuts.RData")
model2 <- model
summary(model2)


model2$summary.random$`interaction(NUTS3, YearMonth)`$mean

id <- model2$summary.random$`interaction(NUTS3, YearMonth)`$ID
rr <- exp(model2$summary.random$`interaction(NUTS3, YearMonth)`$mean)
rr.lci<- exp(model2$summary.random$`interaction(NUTS3, YearMonth)`$"0.025quant")
rr.uci <- exp(model2$summary.random$`interaction(NUTS3, YearMonth)`$"0.975quant")

results <- data.frame(id = id, rr = rr, rr.lci=rr.lci,rr.uci = rr.uci )
# Assuming your data is in a data frame called 'results'
interaction_data <- results[grep("ES[0-9][0-9][0-9].[1-7].[5-8]", results$id), ]
# Extract the NUTS3 code using str_extract and a regex that captures the pattern
interaction_data$NUTS3 <- str_extract(interaction_data$id, "ES\\d+")




lancet_plot <- 
  interaction_data %>% 
  group_by(rr,rr.lci,rr.uci,NUTS3) %>%
  # add the predefined state grid by state code
  left_join(grid, by = c("NUTS3" = "code_num")) %>%
  ggplot(aes(x = id, y = rr, group = NUTS3)) +
  geom_point(size=0.2, colour=muted("black"), alpha=1, show.legend=FALSE) +  # Optimal size and slightly transparent for overlap
  geom_errorbar(aes(ymin=rr.lci, ymax=rr.uci,group = NUTS3), colour="black",size=0.02, width=0.3, linewidth=0.02) +  # More professional color and thickness
  geom_hline(yintercept = 1 , linetype = "dashed", color = "black", linewidth=1) +  # Prominent reference line
  coord_flip() +
  theme_bw() +  # Increased base font size for better clarity
  theme(
    axis.text.y = element_text(size = 5, colour = "black"),  # Enhanced y-axis tex  # Cleaner y-axis
    axis.text.x = element_text(size = 11, angle = 0, colour = "black"),  # Enhanced x-axis text readability
    panel.grid.major = element_blank(),  # Clean major grid lines
    panel.grid.minor = element_blank(),  # Clean minor grid lines
    panel.background = element_rect(fill = "white", colour = "black"),  # High-contrast background
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold")  # Prominent title
  ) + 
  labs(
    title = "Relative Risk by Month and Year Interaction",
    x = "Relative Risk (RR)",
    y = ""
  ) +
  # organise by state name in grid file
  facet_geo( ~name, grid = grid, scales = "free_y")

ggsave("C:\\Users\\u271201\\Downloads\\months_nuts.eps", height = 70, width = 100, units = "cm")
