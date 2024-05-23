install.packages("officer")
library(officer)
install.packages("readtext")
library(readtext)
install.packages("docxtractr")
library(docxtractr)
##

month_breaks <- c(1,5,9,14,18,22,27,31, 35, 40, 44, 48)  # Week numbers where months approximately start
month_labels <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

# plot mortality incidence rate (per 100,000 inhabitants) heat maps (month and year) per state 
dir_facet <- 
 data %>% 
  group_by(year, week,month, NUTS3) %>%
  # calculate state level incidence rate
  mutate(var = MaxTm) %>% 
  # add the predefined state grid by state code
  left_join(grid, by = c("NUTS3" = "code_num")) %>% 
  ggplot(aes(x = week, y = year, fill = var)) + 
  geom_tile() +
  ylab("Year") + 
  xlab("Week") +
  scale_fill_gradientn(name = "Temperatures", colours=brewer.pal(9, "PuRd"), trans = "log1p", breaks = c(0, 10, 20, 30, 40, 50), labels = c(0, 10, 20, 30, 40, 50) ) + 
  scale_y_continuous(breaks = c(2017, 2018, 2019,2020, 2021, 2022, 2023)) +
  scale_x_continuous(breaks= c(1,5,9,14,18,22,27,31, 35, 40, 44, 48), labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec") ) +
  theme_bw() + 
  # organise by state name in grid file
  facet_geo( ~name, grid = grid)

ggsave("C:\\Users\\u271201\\Downloads\\fig_01_dir_facet.eps", height = 60, width = 100, units = "cm")




# plot mortality incidence rate (per 100,000 inhabitants) heat maps (month and year) per state 
dir_facet <- 
  data %>% 
  group_by(year, week,month, NUTS3) %>%
  # calculate state level incidence rate
  mutate(var = NO2) %>% 
  # add the predefined state grid by state code
  left_join(grid, by = c("NUTS3" = "code_num")) %>% 
  ggplot(aes(x = week, y = year, fill = var)) + 
  geom_tile() +
  ylab("Year") + 
  xlab("week") +
  scale_fill_gradientn(name = "NO2 (µg/m³)", colours=brewer.pal(9, "PuRd"), trans = "log1p", breaks = c(0, 10, 40), labels = c(0, "10(WHO)", "40(EU)") ) + 
  scale_y_continuous(breaks = c(2017, 2018, 2019,2020, 2021, 2022, 2023)) +
  scale_x_continuous(breaks= c(1,5,9,14,18,22,27,31, 35, 40, 44, 48), labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec") ) +
  theme_bw() + 
  # organise by state name in grid file
  facet_geo( ~name, grid = grid)

ggsave("C:\\Users\\u271201\\Downloads\\fig_01_dir_facet.eps", height = 60, width = 100, units = "cm")

# plot mortality incidence rate (per 100,000 inhabitants) heat maps (month and year) per state 
dir_facet <- 
  data %>% 
  group_by(year, week,month, NUTS3) %>%
  # calculate state level incidence rate
  mutate(var = O3) %>% 
  # add the predefined state grid by state code
  left_join(grid, by = c("NUTS3" = "code_num")) %>% 
  ggplot(aes(x = week, y = year, fill = var)) + 
  geom_tile() +
  ylab("Year") + 
  xlab("week") +
  scale_fill_gradientn(name = "O3 levels (µg/m³)", colours=brewer.pal(9, "PuRd"), trans = "log1p", breaks = c(0, 100, 120), labels = c(0, "100(WHO)", "120(EU)") ) + 
  scale_y_continuous(breaks = c(2017, 2018, 2019,2020, 2021, 2022, 2023)) +
  scale_x_continuous(breaks= c(1,5,9,14,18,22,27,31, 35, 40, 44, 48), labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec") ) +
  theme_bw() + 
  # organise by state name in grid file
  facet_geo( ~name, grid = grid)

ggsave("C:\\Users\\u271201\\Downloads\\fig_01_dir_facet.eps", height = 60, width = 100, units = "cm")




# plot mortality incidence rate (per 100,000 inhabitants) heat maps (month and year) per state 
dir_facet <- 
  data %>% 
  group_by(year, week,month, NUTS3) %>%
  # calculate state level incidence rate
  mutate(var = PM2.5) %>% 
  # add the predefined state grid by state code
  left_join(grid, by = c("NUTS3" = "code_num")) %>% 
  ggplot(aes(x = week, y = year, fill = var)) + 
  geom_tile() +
  ylab("Year") + 
  xlab("week") +
  scale_fill_gradientn(name = "PM2.5 levels (µg/m³)", colours=brewer.pal(9, "PuRd"), trans = "log1p", breaks = c(0, 5, 25), labels = c(0, "5(WHO)", "25(EU)") ) + 
  scale_y_continuous(breaks = c(2017, 2018, 2019,2020, 2021, 2022, 2023)) +
  scale_x_continuous(breaks= c(1,5,9,14,18,22,27,31, 35, 40, 44, 48), labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec") ) +
  theme_bw() + 
  # organise by state name in grid file
  facet_geo( ~name, grid = grid)

ggsave("C:\\Users\\u271201\\Downloads\\fig_01_dir_facet.eps", height = 60, width = 100, units = "cm")

# plot mortality incidence rate (per 100,000 inhabitants) heat maps (month and year) per state 
dir_facet <- 
  data %>% 
  group_by(year, week,month, NUTS3) %>%
  # calculate state level incidence rate
  mutate(var = PM10) %>% 
  # add the predefined state grid by state code
  left_join(grid, by = c("NUTS3" = "code_num")) %>% 
  ggplot(aes(x = week, y = year, fill = var)) + 
  geom_tile() +
  ylab("Year") + 
  xlab("week") +
  scale_fill_gradientn(name = "PM10 levels (µg/m³)", colours=brewer.pal(9, "PuRd"), trans = "log1p", breaks = c(0, 15, 40), labels = c(0, "15 (WHO)", "40(EU)") ) + 
  scale_y_continuous(breaks = c(2017, 2018, 2019,2020, 2021, 2022, 2023)) +
  scale_x_continuous(breaks= c(1,5,9,14,18,22,27,31, 35, 40, 44, 48), labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec") ) +
  theme_bw() + 
  # organise by state name in grid file
  facet_geo( ~name, grid = grid)

ggsave("C:\\Users\\u271201\\Downloads\\fig_01_dir_facet.eps", height = 60, width = 100, units = "cm")


#WHO limit
result <- data[, .(Percentage = 100 * sum(O3 > 100) / .N), by = .(year)]
result <- data[, .(Percentage = 100 * sum(NO2 > 10) / .N), by = .(year)]
result <- data[, .(Percentage = 100 * sum(PM25 > 5) / .N), by = .(year)]
result <- data[, .(Percentage = 100 * sum(PM10 > 1) / .N), by = .(month,year)]

result
#EU limit
result <- data[, .(Percentage = 100 * sum(O3 > 120) / .N), by = .(year)]
result <- data[, .(Percentage = 100 * sum(NO2 > 40) / .N), by = .(year)]
result <- data[, .(Percentage = 100 * sum(PM25 > 25) / .N), by = .(year)]
result <- data[, .(Percentage = 100 * sum(PM10 > 40) / .N), by = .(year)]


#percentage of weeks above different temperatures (Table in Annex)
# Ensure 'data' is a data.table
doc <- read_docx("C:\\Users\\u271201\\Downloads\\Figures analysis\\Descriptives\\Tables\\Temperatures.docx")
tables <- docxtractr::read_docx_tbl(doc, guess_header = TRUE)
# Compute temperatures thresholds
Temperatures <- data[, .(
  "40°C" = 100 * sum(MaxTm > 40, na.rm = TRUE) / .N,
  "40°C(w)" = sum(MaxTm > 40, na.rm = TRUE),
  "37°C" = 100 * sum(MaxTm > 37, na.rm = TRUE) / .N,
  "37°C(w)" = sum(MaxTm > 37, na.rm = TRUE),
  "32°C" = 100 * sum(MaxTm > 32, na.rm = TRUE) / .N,
  "32°C(w)" = sum(MaxTm > 32, na.rm = TRUE),
  "30°C"= 100 * sum(MaxTm > 30, na.rm = TRUE) / .N,  # Note: Shouldn't this be `MaxTm > 30`?
  "30°C(w)"= sum(MaxTm > 30, na.rm = TRUE)
), by = .(year, NUTS3.Name)]

write.csv(Temperatures, "C:\\Users\\u271201\\Downloads\\Figures analysis\\Descriptives\\Tables\\Temperatures.csv", row.names = FALSE)

# Filter data where days above certain thresholds are greater than 0
filtered_data <- Temperatures[ `40°C(w)` > 0 & `37°C(w)` > 0 & `32°C(w)` > 0]
summary <- docx_summary(doc)

# Select columns for the final table
Temp_table <- filtered_data[, .(year, NUTS3.Name, `40°C`, `40°C(w)`, `37°C`, `37°C(w)`,`32°C`,`32°C(w)` )]
write.csv(Temp_table, "C:\\Users\\u271201\\Downloads\\Figures analysis\\Descriptives\\Tables\\Temperatures1.csv", row.names = FALSE)




summer_weeks <- data[data$week >= 22 & data$week <= 35, ]
# Calculate the percentages for each pollutant and condition
results <- data[, .(
  O3_WHO = sum(O3 > 100, na.rm = TRUE),
  O3_EU = sum(O3 > 120, na.rm = TRUE),
  PM25_WHO = sum(PM2.5 > 5, na.rm = TRUE),
  PM25_EU = sum(PM2.5 > 25, na.rm = TRUE),
  PM10_WHO = sum(PM10 > 15, na.rm = TRUE),
  PM10_EU = sum(PM10 > 40, na.rm = TRUE),
  NO2_WHO = sum(NO2 > 10, na.rm = TRUE),
  NO2_EU = sum(NO2 > 40, na.rm = TRUE)
  ), by = .(year, NUTS3.Name)]
results


write.csv(results, "C:\\Users\\u271201\\Downloads\\Figures analysis\\Descriptives\\Tables\\Pollutants.csv", row.names = FALSE)




ggplot(summer_weeks, aes(x = week, y = Mort_male , group = Age_Group, color = Age_Group)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_x_continuous(breaks = 2000:2005) +  # Custom x-axis breaks
  scale_y_continuous(labels = scales::percent) +  # Convert y-axis to percentage
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "Mortality Rate by Age Group Over Time",
    x = "Year",
    y = "Mortality Rate",
    color = "Age Group"
  ) +
  facet_wrap(~ Group, scales = "free_y") +  # Facet by Group, allowing free y-axis scales
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    legend.position = "right"
  )

mort <- summer_weeks %>% 
  group_by(year,week,month, NUTS3) %>%
  # calculate state level incidence rate
  summarise(cases1 = sum(Mort_Male65_79),
            pop1 = sum(Popu_Male65_79)) %>% 
  mutate(var1 = cases1 / pop1 * 10^5) %>% 
  summarise(cases2 = sum(Mort_Female65_79),
            pop2 = sum(Popu_Female65_79)) %>% 
  mutate(var2 = cases2 / pop2 * 10^5) %>% 
  summarise(cases3 = sum(Mort_Female80_),
            pop3 = sum(Popu_Male80_)) %>% 
  mutate(var3 = cases3 / pop3 * 10^5) %>% 
  summarise(cases4 = sum(Mort_Male80_),
            pop4 = sum(Popu_Male80_)) %>% 
  mutate(var4 = cases4 / pop4 * 10^5) %>% 
  # add the predefined state grid by state code
  left_join(grid, by = c("NUTS3" = "code_num")) %>% 
  ggplot(aes(x = week, y = year, group = .groups)) +
  geom_line( geom_line(aes(linetype = var1))) +
  geom_line( geom_line(aes(linetype = var2))) +
  geom_line( geom_line(aes(linetype = var3))) +
  geom_line( geom_line(aes(linetype = var4))) +
  geom_tile() +
  ylab("Year") + 
  xlab("week") +
  scale_fill_gradientn(name = "NO2 (µg/m³)", colours=brewer.pal(9, "PuRd"), trans = "log1p", breaks = c(0, 10, 40), labels = c(0, "10(WHO)", "40(EU)") ) + 
  scale_y_continuous(breaks = c(2017, 2018, 2019,2020, 2021, 2022, 2023)) +
  scale_x_continuous(breaks= c(1,5,9,14,18,22,27,31, 35, 40, 44, 48), labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec") ) +
  theme_bw() + 
  # organise by state name in grid file
  facet_geo( ~name, grid = grid)

ggsave("C:\\Users\\u271201\\Downloads\\mort.eps", height = 60, width = 100, units = "cm")









mort <- summer_weeks %>%
  group_by(year, week) %>%
  mutate(
    var1 = sum(Mort_Male65_79),
    var2 = sum(Mort_Female65_79),
    var3 = sum(Popu_Female80_),
    var4 = sum(Mort_Male80_)
  )
p <- mort %>% left_join(grid, by = c("NUTS3" = "code_num")) %>% 
ggplot( aes(x = week, group = year, color = factor(year) )) +
  geom_line(aes(y = var1, linetype = "Male65_79")) +
  geom_line(aes(y = var2, linetype = "Female65_79")) +
  geom_line(aes(y = var3, linetype = "Female80_")) +
  geom_line(aes(y = var4, linetype = "Male80_")) +
  geom_tile() +
  ylab("Year") + 
  xlab("Week") +
  scale_linetype_manual(name = "Age Group",
                        values = c("Male65_79" = "solid", 
                                   "Female65_79" = "dashed",
                                   "Female80_" = "dotted",
                                   "Male80_" = "dotdash"),
                        labels = c("Male 65-79", 
                                   "Female 65-79",
                                   "Female 80+", 
                                   "Male 80+")) +
  scale_x_continuous(breaks = c(1, 5, 9, 14, 18, 22, 27, 31, 35, 40, 44, 48),
                     labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  theme_bw() + 
  # organise by state name in grid file
  facet_geo( ~name, grid = grid) 
ggsave("C:\\Users\\u271201\\Downloads\\mort.eps", height = 60, width = 100, units = "cm")


mort <- summer_weeks %>% 
  group_by(year,week) %>%
  # calculate state level incidence rate
  summarise(cases1 = sum(Mort_Total),
            pop1 = sum(Popu_Total)) %>% 
  mutate(var1 = cases1 / pop1 * 10^5)
    

g <- ggplot(mort, aes(x = week, y = var1, group = year, color = factor(year))) +
  geom_line(size = 2) +
  scale_color_brewer(type = 'qual', palette = "Accent") +
  theme_minimal(base_size = 14) +
  labs(
    title = "Weekly Mortality by Year",
    x = "Months",
    y = "Number of Deaths",
    color = "Year"
  ) +
  scale_x_continuous(breaks = c(1, 5, 9, 14, 18, 22, 27, 31, 35, 40, 44, 48),
                     labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )


write.csv(mort, "C:\\Users\\u271201\\Downloads\\Figures analysis\\Descriptives\\Tables\\Mortality in summer.csv", row.names = FALSE)




# Define the first mort dataframe
g <- summer_weeks %>% 
  group_by(year, week, NUTS3) %>%
  summarise(
    cases1 = sum(Mort_Total),
    pop1 = sum(Popu_Total),) %>% 
  mutate(
    var1 = cases1 / pop1 * 10^5,
  )%>%
  left_join(grid, by = c("NUTS3" = "code_num")) %>%
ggplot( aes(x = week, y= var1 , group = year, color = factor(year)),linetype = "Female")+
  geom_line(size = 2) +
  scale_color_brewer(type = 'qual', palette = "Accent") +
  theme_minimal(base_size = 14) +
  labs(
    title = "Weekly Mortality by Year",
    x = "Months",
    y = "Number of Deaths",
    color = "Year"
  ) +
  scale_x_continuous(breaks = c(1, 5, 9, 14, 18, 22, 27, 31, 35, 40, 44, 48),
                     labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )+
    theme_bw() + 
    # organise by state name in grid file
    facet_geo( ~name, grid = grid) 
  ggsave("C:\\Users\\u271201\\Downloads\\mort.eps", height = 60, width = 100, units = "cm")

  write.csv(g, "C:\\Users\\u271201\\Downloads\\Figures analysis\\Descriptives\\Tables\\Mortality in summer_NUTS3.csv", row.names = FALSE)
  
  
  
#Descriptives
  install.packages("summarytools")
  library(summarytools)
  
  # For numerical variables
  descriptive_stats <- summarytools::descr(data[, c( "MaxTm", "PM2.5", "PM10", "O3", "NO2", "Mort_Male", "Mort_Female", "Mort_Total", "Popu_Male", "Popu_Female", "Popu_Total")], stats = "common")
  
  # For categorical variables
  frequency_tables <- summarytools::freq(data$NUTS3.Name)
  
  # Print results
  print(descriptive_stats)
  print(frequency_tables)
  
  summarytools::view(descriptive_stats)