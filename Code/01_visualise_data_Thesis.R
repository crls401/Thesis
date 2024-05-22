#************************************************************************************************#

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











#percentage of weeks above different temperatures 
Temperatures <- data[, .(
  "40°C" = 100 * sum(MaxTm > 40, na.rm = TRUE) / .N,
  "40°C" = sum(MaxTm > 40, na.rm = TRUE),
  "37°C" = 100 * sum(MaxTm > 37, na.rm = TRUE) / .N,
  "37°C" = sum(MaxTm > 37, na.rm = TRUE),
  "35°C" = 100 * sum(MaxTm > 35, na.rm = TRUE) / .N,
  "35°C" = sum(MaxTm > 35, na.rm = TRUE),
  "30°C"= 100 * sum(MaxTm > 25, na.rm = TRUE) / .N,
  "30°C"= sum(MaxTm > 25, na.rm = TRUE)
), by = .(year, NUTS3.Name )]
Temperatures
view(Temperatures)


# Calculate the percentages for each pollutant and condition
results <- data[, .(
  O3_WHO = 100 * sum(O3 > 100, na.rm = TRUE) / .N,
  O3_EU = 100 * sum(O3 > 120, na.rm = TRUE) / .N,
  PM25_WHO = 100 * sum(PM2.5 > 5, na.rm = TRUE) / .N,
  PM25_EU = 100 * sum(PM2.5 > 25, na.rm = TRUE) / .N,
  PM10_WHO = 100 * sum(PM10 > 15, na.rm = TRUE) / .N,
  PM10_EU = 100 * sum(PM10 > 40, na.rm = TRUE) / .N,
  NO2_WHO = 100 * sum(NO2 > 10, na.rm = TRUE) / .N,
  NO2_EU = 100 * sum(NO2 > 40, na.rm = TRUE) / .N
  ), by = .(year, NUTS3.Name)]
results
# Print the combined results
results1 <- data[, .(
  O3_WHO = 100 * sum(O3 > 100, na.rm = TRUE) / .N,
  O3_EU = 100 * sum(O3 > 120, na.rm = TRUE) / .N,
  PM25_WHO = 100 * sum(PM2.5 > 5, na.rm = TRUE) / .N,
  PM25_EU = 100 * sum(PM2.5 > 25, na.rm = TRUE) / .N,
  PM10_WHO = 100 * sum(PM10 > 15, na.rm = TRUE) / .N,
  PM10_EU = 100 * sum(PM10 > 40, na.rm = TRUE) / .N,
  NO2_WHO = 100 * sum(NO2 > 10, na.rm = TRUE) / .N,
  NO2_EU = 100 * sum(NO2 > 40, na.rm = TRUE) / .N
), by = .(year, week, NUTS3.Name)]
