install.packages("tidyr")
install.packages("eurostat")
install.packages("lubridate")
install.packages("scales")
install.packages("hrbrthemes")
install.packages("gfonts")
install.packages("tidyr")
install.packages("dplyr")
install.packages("ISOweek")
library(remotes)
library(tidyverse)
library(tidyr)
library(lubridate)
library(stringr)
library(scales)
library(ggpubr)
library(ggthemes)
library(hrbrthemes)
library(gfonts)
library(dplyr)
library(dbplyr)
library(purrr)
library(reshape2)
library(openxlsx)
library(dlnm)
library(data.table)

################################NUTS############################################
path7 <- "C:\\Users\\u271201\\Downloads\\Data\\nuts\\estat_demo_r_mweek3_filtered_en.csv.gz"
nuts <- read.csv(path7)
str(nuts)

nuts <- nuts[, !(names(nuts) %in% c("LAST.UPDATE","DATAFLOW", "sex","TIME_PERIOD", "freq","unit", "OBS_FLAG", "OBS_VALUE"))]
nuts <- separate(nuts, geo, into = c("NUTS3", "NUTS3.Name"), sep = ":", remove = FALSE)
nuts <- nuts[, !(names(nuts) %in% c("geo","age"))]

#NUTS reference dataframe
path8 <- "C:\\Users\\u271201\\Downloads\\Data\\nuts\\\\Book3.csv"
nuts_Struct <- read.csv(path8)

###############################MORTALITY########################################
# Windows file path example
path <- "C:\\Users\\u271201\\Downloads\\Data\\Deaths\\estat_demo_r_mweek3_filtered_en.csv.gz"

# read the CSV file
death <- read.csv(path)

# separate the TIME_PERIOD column into two columns
death1 <- separate(death, TIME_PERIOD, into = c("year", "week"), sep = "-") 
death1$year <- as.integer(death1$year)

# Create new columns for NUTS
death1$NUTS1 <- ifelse(nchar(death1$geo) == 2, death1$geo, NA)
death1$NUTS2 <- ifelse(nchar(death1$geo) == 4, death1$geo, NA)
death1$NUTS3 <- ifelse(nchar(death1$geo) == 5, death1$geo, NA)

death1 <- death1[, !(names(death1) %in% c("NUTS0", "STRUCTURE","STRUCTURE_ID", "freq","unit", "OBS_FLAG "))]

# Assuming 'data' is your initial dataset
data_transformed <- death1 %>%
  # Keep rows that contribute to age-sex categories including TOTALs
  filter((age == "TOTAL" | age %in% c("Y60-64", "Y65-69", "Y70-74", "Y75-79", "Y80-84", "Y85-89", "Y_GE90")) & (sex %in% c("M", "F", "T"))) %>%
  # Create a new identifier combining age and sex
  mutate(age_sex = paste(age, sex, sep = "_")) %>%
  # Spread the data into a wide format with one column per age_sex category
  pivot_wider(names_from = age_sex, values_from = OBS_VALUE, values_fill = list(OBS_VALUE = 0)) %>%
  # Ensure one row per combination of geo, year, week, and NUTS3
  group_by(geo, year, week, NUTS3) %>%
  # Summarise the data to avoid duplicates, summing OBS_VALUE for any potential duplicates
  summarise(across(starts_with("Y"), sum, na.rm = TRUE), .groups = "drop")

# View the transformed dataset
print(data_transformed)

mort_def <- data_transformed %>%
filter(!is.na(NUTS3))

mort_def$geo <- NULL
############################## POPOLATION ######################################
path2 <- "C:\\Users\\u271201\\Downloads\\Data\\Population\\demo_r_pjangrp3__custom_10704305_linear.csv.gz"

# read the CSV file
pop <- read.csv(path2)
names(pop)[names(pop) == "TIME_PERIOD"] <- "year"

# Create new columns for NUTS
pop$NUTS1 <- ifelse(nchar(pop$geo) == 2, pop$geo, NA)
pop$NUTS2 <- ifelse(nchar(pop$geo) == 4, pop$geo, NA)
pop$NUTS3 <- ifelse(nchar(pop$geo) == 5, pop$geo, NA)
pop <- pop[, !(names(pop) %in% c( "STRUCTURE","STRUCTURE_ID", "freq","unit", "OBS_FLAG "))]



# Assuming 'data' is your initial dataset
data_transformed1 <- pop %>%
  # Keep rows that contribute to age-sex categories including TOTALs
  filter((age == "TOTAL" | age %in% c("Y60-64", "Y65-69", "Y70-74", "Y75-79", "Y80-84", "Y85-89", "Y_GE90")) & (sex %in% c("M", "F", "T"))) %>%
  # Create a new identifier combining age and sex
  mutate(age_sex = paste(age, sex, sep = "_")) %>%
  # Spread the data into a wide format with one column per age_sex category
  pivot_wider(names_from = age_sex, values_from = OBS_VALUE, values_fill = list(OBS_VALUE = 0)) %>%
  # Ensure one row per combination of geo, year, week, and NUTS3
  group_by(geo, year, NUTS3) %>%
  # Summarise the data to avoid duplicates, summing OBS_VALUE for any potential duplicates
  summarise(across(starts_with("Y"), sum, na.rm = TRUE), .groups = "drop")

pop_def <- data_transformed1 %>%
  filter(!is.na(NUTS3))


pop_def$geo <- NULL
###############################POLLUTION##########################################


path3 <- "C:\\Users\\u271201\\Downloads\\Data\\Pollution Air Spain\\Graph_week_data NO2.csv"
NO2 <- read.csv(path3)
path4 <- "C:\\Users\\u271201\\Downloads\\Data\\Pollution Air Spain\\Graph_week_data O3.csv"
O3 <- read.csv(path4)
path5 <- "C:\\Users\\u271201\\Downloads\\Data\\Pollution Air Spain\\Graph_week_data pm2.5.csv"
PM2.5 <- read.csv(path5)
path6 <- "C:\\Users\\u271201\\Downloads\\Data\\Pollution Air Spain\\Graph_week_data PM10.csv"
PM10 <- read.csv(path6)


NO2 <- NO2 %>%
       filter(Year.of.datebegin != 2016)
O3 <- O3 %>%
  filter(Year.of.datebegin != 2016)
PM2.5 <- PM2.5 %>%
  filter(Year.of.datebegin != 2016)
PM10 <- PM10 %>%
  filter(Year.of.datebegin != 2016)


NO2$Year.of.datebegin <- NULL
O3$Year.of.datebegin <- NULL
PM2.5$Year.of.datebegin <- NULL
PM10$Year.of.datebegin <- NULL

NO2 <- NO2%>%
       group_by(City, Week.of.datebegin) %>%
       slice_max(order_by = Avg..AirQualityLevel, n = 1, with_ties = FALSE) %>%
       ungroup()
O3 <- O3%>%
  group_by(City, Week.of.datebegin) %>%
  slice_max(order_by = Avg..AirQualityLevel, n = 1, with_ties = FALSE) %>%
  ungroup()
PM2.5 <- PM2.5%>%
  group_by(City, Week.of.datebegin) %>%
  slice_max(order_by = Avg..AirQualityLevel, n = 1, with_ties = FALSE) %>%
  ungroup()
PM10 <- PM10%>%
  group_by(City, Week.of.datebegin) %>%
  slice_max(order_by = Avg..AirQualityLevel, n = 1, with_ties = FALSE) %>%
  ungroup()


# Create a data frame with city names and their corresponding provinces
city_names <- c("A Coruña", "Albacete", "Alcalá de Guadaíra", "Alcalá de Henares", "Alcoy", "Alicante/Alacant", "Almería", "Arrecife", "Ávila", "Avilés", "Badajoz", "Barcelona", "Benidorm", "Bilbao", "Burgos", "Cáceres", "Cádiz", "Cartagena", "Castellón de la Plana/Castelló de la Plana", "Ceuta", "Ciudad Real", "Collado Villalba", "Córdoba", "Cuenca", "Dos Hermanas", "Elche/Elx", "Elda", "Ferrol", "Gandia", "Gijón", "Girona", "Granada", "Granollers", "Guadalajara", "Huelva", "Jaén", "Jerez de la Frontera", "Las Palmas", "León", "Línea de la Concepción, La", "Lleida", "Logroño", "Lugo", "Madrid", "Málaga", "Manresa", "Marbella", "Mataró", "Mérida", "Mollet del Vallès", "Murcia", "Ourense", "Oviedo", "Palencia", "Palma de Mallorca", "Pamplona/Iruña", "Pontevedra", "Reus", "Sagunto", "Salamanca", "San Fernando", "San Sebastián/Donostia", "Santa Cruz de Tenerife", "Santander", "Santiago de Compostela", "Sevilla", "Talavera de la Reina", "Tarragona", "Telde", "Toledo", "Torrejón de Ardoz", "Torrelavega", "Torrevieja", "Valdemoro", "Valencia", "Valladolid", "Vigo", "Vilanova i la Geltrú", "Vitoria/Gasteiz", "Zamora", "Zaragoza")
provinces <- c("A Coruña", "Albacete", "Sevilla", "Madrid", "Alicante", "Alicante", "Almería", "Las Palmas", "Ávila", "Asturias", "Badajoz", "Barcelona", "Alicante", "Biscay", "Burgos", "Cáceres", "Cádiz", "Murcia", "Castellón", "Ceuta", "Ciudad Real", "Madrid", "Córdoba", "Cuenca", "Seville", "Alicante", "Alicante", "A Coruña", "Valencia", "Asturias", "Girona", "Granada", "Barcelona", "Guadalajara", "Huelva", "Jaén", "Cádiz", "Las Palmas", "León", "Cádiz", "Lleida", "La Rioja", "Lugo", "Madrid", "Málaga", "Barcelona", "Málaga", "Barcelona", "Badajoz", "Barcelona", "Murcia", "Ourense", "Asturias", "Palencia", "Balearic Islands", "Navarra", "Pontevedra", "Tarragona", "Valencia", "Salamanca", "Cádiz", "Gipuzkoa", "Santa Cruz de Tenerife", "Cantabria", "A Coruña", "Sevilla", "Toledo", "Tarragona", "Las Palmas", "Toledo", "Madrid", "Cantabria", "Alicante", "Madrid", "Valencia", "Valladolid", "Pontevedra", "Barcelona", "Álava", "Zamora", "Zaragoza")

# Create a data frame
city_province_df <- data.frame(City = city_names, Province = provinces)


# Merge the city_province_df with your dataset based on the 'City' column
NO2<- merge(NO2, city_province_df, by = "City", all.x = TRUE)
O3<- merge(O3, city_province_df, by = "City", all.x = TRUE)
PM2.5<- merge(PM2.5, city_province_df, by = "City", all.x = TRUE)
PM10<- merge(PM10, city_province_df, by = "City", all.x = TRUE)

duplicate_check1 <-NO2 %>%
            group_by(City, Week.of.datebegin) %>%
           summarise(Count = n(), .groups = "drop") %>%
        filter(Count > 1)
duplicate_check12 <-O3 %>%
  group_by(City, Week.of.datebegin) %>%
  summarise(Count = n(), .groups = "drop") %>%
  filter(Count > 1)
duplicate_check123 <-PM2.5 %>%
  group_by(City, Week.of.datebegin) %>%
  summarise(Count = n(), .groups = "drop") %>%
  filter(Count > 1)
duplicate_check1234 <-PM10 %>%
  group_by(City, Week.of.datebegin) %>%
  summarise(Count = n(), .groups = "drop") %>%
  filter(Count > 1)


############################################################################################


NO2 <- NO2 %>%
  mutate(
    Date = as.Date(Week.of.datebegin, format = "%B %d, %Y"),
    ISOWeek = isoweek(Date),
    # Directly use the 'year()' function on 'Date'
    Year = year(Date),
    # Adjust year for ISO week numbering around the new year
    ISOYear = ifelse(ISOWeek == 1 & month(Date) == 12, Year + 1,
                     ifelse(ISOWeek > 50 & month(Date) == 1, Year - 1, Year)),
    Week.of.datebegin = paste0("W", ISOWeek)
  )

O3 <- O3 %>%
  mutate(
    Date = as.Date(Week.of.datebegin, format = "%B %d, %Y"),
    ISOWeek = isoweek(Date),
    # Directly use the 'year()' function on 'Date'
    Year = year(Date),
    # Adjust year for ISO week numbering around the new year
    ISOYear = ifelse(ISOWeek == 1 & month(Date) == 12, Year + 1,
                     ifelse(ISOWeek > 50 & month(Date) == 1, Year - 1, Year)),
    Week.of.datebegin = paste0("W", ISOWeek)
  )

PM2.5 <- PM2.5 %>%
  mutate(
    Date = as.Date(Week.of.datebegin, format = "%B %d, %Y"),
    ISOWeek = isoweek(Date),
    # Directly use the 'year()' function on 'Date'
    Year = year(Date),
    # Adjust year for ISO week numbering around the new year
    ISOYear = ifelse(ISOWeek == 1 & month(Date) == 12, Year + 1,
                     ifelse(ISOWeek > 50 & month(Date) == 1, Year - 1, Year)),
    Week.of.datebegin = paste0("W", ISOWeek)
  )

PM10 <- PM10 %>%
  mutate(
    Date = as.Date(Week.of.datebegin, format = "%B %d, %Y"),
    ISOWeek = isoweek(Date),
    # Directly use the 'year()' function on 'Date'
    Year = year(Date),
    # Adjust year for ISO week numbering around the new year
    ISOYear = ifelse(ISOWeek == 1 & month(Date) == 12, Year + 1,
                     ifelse(ISOWeek > 50 & month(Date) == 1, Year - 1, Year)),
    Week.of.datebegin = paste0("W", ISOWeek)
  )

NO2 <- NO2 %>%
  select( City, Week.of.datebegin, Avg..AirQualityLevel, Province, ISOYear)
O3 <- O3 %>%
  select( City, Week.of.datebegin, Avg..AirQualityLevel, Province, ISOYear)
PM2.5 <- PM2.5 %>%
  select( City, Week.of.datebegin, Avg..AirQualityLevel, Province, ISOYear)
PM10 <- PM10 %>%
  select( City, Week.of.datebegin, Avg..AirQualityLevel, Province, ISOYear)

NO2 <- NO2 %>%
  filter(ISOYear != 2016)
O3 <- O3 %>%
  filter(ISOYear != 2016)
PM2.5 <- PM2.5 %>%
  filter(ISOYear != 2016)
PM10 <- PM10 %>%
  filter(ISOYear != 2016)


##########################################################################################

colnames(PM10) <- c("city", "week","PM10","NUTS3.Name", "year")
colnames(PM2.5) <-c("city", "week","PM2.5","NUTS3.Name", "year")
colnames(O3) <- c("city", "week","O3","NUTS3.Name", "year")
colnames(NO2) <- c("city", "week","NO2","NUTS3.Name", "year")

# Assuming your data frame is named "data" with columns city, year, week, NO2, and NUTS3.Name

# Calculate city-level averages for each year and week (if needed)
# Calculate the average NO2 level per province and week
averagesNO2 <- NO2 %>%
  group_by(NUTS3.Name, year, week) %>%
  summarize(NO2_NUTS3 = mean(NO2), .groups = 'drop')

averagesO3 <- O3%>%
  group_by(NUTS3.Name, year, week) %>%
  summarize(O3_NUTS3 = mean(O3), .groups = 'drop')

averagesPM10 <-PM10 %>%
  group_by(NUTS3.Name, year, week) %>%
  summarize(PM10_NUTS3 = mean(PM10), .groups = 'drop')

averagesPM2.5 <- PM2.5 %>%
  group_by(NUTS3.Name, year, week) %>%
  summarize(PM2.5_NUTS3 = mean(PM2.5), .groups = 'drop')

# Join the averages back to the original dataset to include the aver_Province column
NO2W <- NO2 %>%
  left_join(averagesNO2, by = c("NUTS3.Name", "year", "week"))
O3W <- O3 %>%
  left_join(averagesO3, by = c("NUTS3.Name", "year", "week"))
PM10W  <- PM10  %>%
  left_join(averagesPM10 , by = c("NUTS3.Name", "year", "week"))
PM2.5W <- PM2.5 %>%
  left_join(averagesPM2.5, by = c("NUTS3.Name", "year", "week"))

NO2W1 <- NO2W %>%
  select( year, week, NUTS3.Name, NO2_NUTS3)
O3W1 <- O3W %>%
  select( year, week, NUTS3.Name, O3_NUTS3)
PM10W1 <- PM10W %>%
  select( year, week, NUTS3.Name, PM10_NUTS3)
PM2.5W1 <- PM2.5W %>%
  select( year, week, NUTS3.Name, PM2.5_NUTS3)

poll <- averagesNO2 %>%
  left_join(averagesO3, by = c("NUTS3.Name", "year", "week")) %>%
  left_join(averagesPM10, by = c("NUTS3.Name", "year", "week")) %>%
  left_join(averagesPM2.5, by = c("NUTS3.Name", "year", "week"))


poll <- poll %>%
      mutate(week = if_else(str_detect(week, "^W[1-9]$"), 
                                                         str_replace(week, "W(\\d)$", "W0\\1"),  week))
#########################################################Temperature#######################################
# Set the working directory to where your files are located
setwd("C:\\Users\\u271201\\Downloads\\Data\\Temperature\\DatosPorEstacion2023-12-04\\DatosPorEstacion - 2023-12-04")

# import files
files = list.files(pattern="*.csv")

# Updated code with fill=TRUE for handling rows with varying number of fields
dataset = do.call(rbind, lapply(files, function(x) fread(x, fill=TRUE)))

# Clean up the environment
rm(files)
# Transform data to df
dataset <- as.data.frame(unclass(dataset))

# Ensure the dataset is a data.table
setDT(dataset)

# Convert the FECHA column to Date type
dataset[, FECHA := as.Date(FECHA, format = "%Y-%m-%d")]

# Filter for dates from January 1, 2017, onwards
dataset_filtered <- dataset[FECHA >= as.Date("2017-01-01")]


# Load necessary libraries
library(dplyr)
library(lubridate)
dataset_filtered1 <- dataset_filtered %>% 
  mutate(Date = as.Date(FECHA, format = "%Y-%m-%d")) %>%  # Ensure Date is in Date format
  filter(Date >= as.Date("2017-01-02")) %>%  # Filter dates from January 2, 2017
  mutate(week = paste0("W", format(Date, "%V")))  # Create week variable in "W[WeekNumber]" format

dataset_clean <- dataset_filtered1 %>%
  mutate(across(where(is.character), ~str_replace_all(., "\xd1", "N")))

dataset_filtered2 <- dataset_clean %>%
  select(FECHA, NOMBRE, PROVINCIA, TMEDIA, TMIN, TMAX,Date, week)


dataset_filtered2 <- dataset_filtered2 %>%
  mutate(Year = lubridate::year(as.Date(FECHA, format = "%Y-%m-%d")))

dataset_filtered2  <- dataset_filtered2  %>%
  mutate(TMIN = as.numeric(TMIN),
         TMAX = as.numeric(TMAX),
         TMEDIA = as.numeric(TMEDIA))

# Assuming your dataset is named 'dataset'
# Step 1: Calculate overall weekly averages for TMIN and TMAX
weekly_averages <- dataset_filtered2 %>%
  group_by(week,Year,NOMBRE) %>%
  summarise(TMID_average_per_week = mean(TMEDIA, na.rm = TRUE),
            TMIN_average_per_week = mean(TMIN, na.rm = TRUE),
            TMAX_average_per_week = mean(TMAX, na.rm = TRUE),
            .groups = "drop")

dataset_enriched2 <- dataset_filtered2 %>%
  left_join(weekly_averages, by = c("week", "Year","NOMBRE"))


# Step 2: Calculate weekly averages for TMIN and TMAX by PROVINCIA
weekly_averages_by_provincia <- dataset_enriched2 %>%
  group_by(week, PROVINCIA, Year) %>%
  summarise(TMID_average_per_week_by_PROVINCIA = mean(TMID_average_per_week, na.rm = TRUE),
            TMIN_average_per_week_by_PROVINCIA = mean(TMIN_average_per_week, na.rm = TRUE),
            TMAX_average_per_week_by_PROVINCIA = mean(TMAX_average_per_week, na.rm = TRUE),
            .groups = "drop")

# To merge these summaries back to the original dataset, you can use a join operation
# This is assuming you want these averages as new columns for each corresponding row in the original dataset
dataset_enriched2 <- dataset_enriched2 %>%
  left_join(weekly_averages_by_provincia, by = c("week", "PROVINCIA", "Year"))




Temp_sp <- dataset_enriched2 %>%
  select(PROVINCIA, Date, week, Year,TMID_average_per_week_by_PROVINCIA, TMIN_average_per_week_by_PROVINCIA,TMAX_average_per_week_by_PROVINCIA)





lookup_table_poll <- data.frame(
  OriginalName = c("A Coruña", "Albacete", "Alicante", "Almería", "Asturias", "Badajoz",
                   "Balearic Islands", "Barcelona", "Biscay", "Burgos", "Cantabria", "Castellón",
                   "Ceuta", "Ciudad Real", "Cuenca", "Cáceres", "Cádiz", "Córdoba", "Gipuzkoa",
                   "Girona", "Granada", "Guadalajara", "Huelva", "Jaén", "La Rioja", "Las Palmas",
                   "León", "Lleida", "Lugo", "Madrid", "Murcia", "Málaga", "Navarra", "Ourense",
                   "Palencia", "Pontevedra", "Salamanca", "Santa Cruz de Tenerife", "Sevilla", "Seville",
                   "Tarragona", "Toledo", "Valencia", "Valladolid", "Zamora", "Zaragoza", "Álava", "Ávila"),
  StandardName = c("A Coruña", "Albacete", "Alicante/Alacant", "Almería", "Asturias", "Badajoz",
                   "Mallorca", "Barcelona", "Bizkaia", "Burgos", "Cantabria", "Castellón/Castelló",
                   "Ceuta", "Ciudad Real", "Cuenca", "Cáceres", "Cádiz", "Córdoba", "Gipuzkoa",
                   "Girona", "Granada", "Guadalajara", "Huelva", "Jaén", "La Rioja", "Gran Canaria",
                   "León", "Lleida", "Lugo", "Madrid", "Murcia", "Málaga", "Navarra", "Ourense",
                   "Palencia", "Pontevedra", "Salamanca", "Tenerife", "Sevilla", "Sevilla",
                   "Tarragona", "Toledo", "Valencia/València", "Valladolid", "Zamora", "Zaragoza",
                   "Araba/Álava", "Ávila"),
  stringsAsFactors = FALSE
)


# Replace names in the 'poll$NUTS3.Name' column
poll$NUTS3.Name <- with(lookup_table_poll, StandardName[match(poll$NUTS3.Name, OriginalName)])


lookup_table_temp_sp <- data.frame(
  OriginalName = c("TARRAGONA", "BARCELONA", "GIRONA", "NAVARRA", "GIPUZKOA", "BIZKAIA",
                   "CANTABRIA", "ASTURIAS", "A CORUNA", "PONTEVEDRA", "LUGO", "LEON",
                   "OURENSE", "SORIA", "BURGOS", "SEGOVIA", "PALENCIA", "VALLADOLID",
                   "AVILA", "MADRID", "SALAMANCA", "ZAMORA", "GUADALAJARA", "CUENCA",
                   "TOLEDO", "CACERES", "CIUDAD REAL", "BADAJOZ", "CORDOBA", "HUELVA",
                   "CEUTA", "JAEN", "GRANADA", "SEVILLA", "CADIZ", "MELILLA", "MALAGA",
                   "ALMERIA", "MURCIA", "ALBACETE", "ALICANTE", "VALENCIA", "TERUEL",
                   "CASTELLON", "ARABA/ALAVA", "LA RIOJA", "HUESCA", "ZARAGOZA",
                   "LLEIDA", "ILLES BALEARS", "LAS PALMAS", "STA. CRUZ DE TENERIFE"),
  StandardName = c("Tarragona", "Barcelona", "Girona", "Navarra", "Gipuzkoa", "Bizkaia",
                   "Cantabria", "Asturias", "A Coruña", "Pontevedra", "Lugo", "León",
                   "Ourense", "Soria", "Burgos", "Segovia", "Palencia", "Valladolid",
                   "Ávila", "Madrid", "Salamanca", "Zamora", "Guadalajara", "Cuenca",
                   "Toledo", "Cáceres", "Ciudad Real", "Badajoz", "Córdoba", "Huelva",
                   "Ceuta", "Jaén", "Granada", "Sevilla", "Cádiz", "Melilla", "Málaga",
                   "Almería", "Murcia", "Albacete", "Alicante/Alacant", "Valencia/València", "Teruel",
                   "Castellón/Castelló", "Araba/Álava", "La Rioja", "Huesca", "Zaragoza",
                   "Lleida", "Mallorca", "Gran Canaria", "Tenerife"),
  stringsAsFactors = FALSE
)


# Do similar replacements for 'Temp_sp$PROVINCIA' and any other datasets or columns
Temp_sp$PROVINCIA <- with(lookup_table_temp_sp, StandardName[match(Temp_sp$PROVINCIA, OriginalName)])





poll1 <- poll %>%
  left_join(nuts, by = c("NUTS3.Name" = "NUTS3.Name"))

Temp_sp1 <- Temp_sp %>%
 left_join(nuts, by = c("PROVINCIA" = "NUTS3.Name"))

mort_def1 <- mort_def %>%
  left_join(nuts, by = c("NUTS3" = "NUTS3"))

pop_def1 <- pop_def %>%
  left_join(nuts, by = c("NUTS3" = "NUTS3"))


names(Temp_sp1)[names(Temp_sp1) == "PROVINCIA"] <- "NUTS3.Name"
names(Temp_sp1)[names(Temp_sp1) == "Year"] <- "year"
Temp_sp1$Date <- NULL
Temp_sp1 <- unique(Temp_sp1)

#Merge
data <- poll1 %>%
 left_join(Temp_sp1, by = c("NUTS3.Name", "year", "week", "NUTS3")) %>%
  left_join(mort_def1, by = c("NUTS3.Name", "year", "week", "NUTS3" )) %>%
  left_join(pop_def1, by = c("NUTS3.Name", "year", "NUTS3" ))
# Original column names
original_names <- c("NUTS3.Name", "year", "week", "NO2", "PM10", "PM2.5", "O3", "NUTS3", 
                    "TMID_average_per_week_by_PROVINCIA", "MinTm", "MaxTm", "Y60-64_F.x", 
                    "Y65-69_F.x", "Y70-74_F.x", "Y75-79_F.x", "Y80-84_F.x", "Y85-89_F.x", 
                    "Y_GE90_F.x", "Y60-64_M.x", "Y65-69_M.x", "Y70-74_M.x", "Y75-79_M.x", 
                    "Y80-84_M.x", "Y85-89_M.x", "Y_GE90_M.x", "Y60-64_T.x", "Y65-69_T.x", 
                    "Y70-74_T.x", "Y75-79_T.x", "Y80-84_T.x", "Y85-89_T.x", "Y_GE90_T.x", 
                    "Y60-64_F.y", "Y65-69_F.y", "Y70-74_F.y", "Y75-79_F.y", "Y80-84_F.y", 
                    "Y85-89_F.y","Y_GE90_F.y", "Y60-64_M.y", "Y65-69_M.y", "Y70-74_M.y", "Y75-79_M.y", 
                    "Y80-84_M.y", "Y85-89_M.y","Y_GE90_M.y", "Y60-64_T.y", "Y65-69_T.y", "Y70-74_T.y", 
                    "Y75-79_T.y", "Y80-84_T.y", "Y85-89_T.y","Y_GE90_T.y")

# New column names according to the expected outcome
new_names <- c("NUTS3.Name","year", "week", "NO2","O3", "PM10", "PM2.5", "NUTS3",
               "Tmid","MinTm","MaxTm" ,  "mort_Y60_64F", "mort_Y65_69F", 
               "mort_Y70_74F", "mort_Y75_79F", "mort_Y80_84F", "mort_Y85_89F", "mort_Y_GE90F", "mort_Y60_64M", 
               "mort_Y65_69M", "mort_Y70_74M", "mort_Y75_79M", "mort_Y80_84M", "mort_Y85_89M", "mort_Y_GE90M", 
               "mort_Y60_64T", "mort_Y65_69T", "mort_Y70_74T", "mort_Y75_79T", "mort_Y80_84T", "mort_Y85_89T", 
               "mort_Y_GE90T", "popu_Y60_64F", "popu_Y65_69F", "popu_Y70_74F", "popu_Y75_79F", "popu_Y80_84F", 
               "popu_Y85_89F","popu_Y_GE90F", "popu_Y60_64M", "popu_Y65_69M", "popu_Y70_74M", "popu_Y75_79M", "popu_Y80_84M", 
               "popu_Y85_89M","popu_Y_GE90M", "popu_Y60_64T", "popu_Y65_69T", "popu_Y70_74T", "popu_Y75_79T", "popu_Y80_84T", 
               "popu_Y85_89T", "popu_Y_GE90T")

# Replace original column names with new names
colnames(data) <- new_names

data <- data %>%
  mutate(week = sub("^W", "", week))
data$week <- as.numeric(data$week)



################################################################################
############################# WITH DENSITY AND LAND USE#########################
################################################################################


path9 <- "C:\\Users\\u271201\\Downloads\\Data\\Density_\\demo_r_d3dens__custom_10703552_linear.csv.gz"
density <- read.csv(path9)

density
density<- density[, !(names(density) %in% c( "STRUCTURE","STRUCTURE_ID", "freq","unit", "OBS_FLAG", "LAST.UPDATE","DATAFLOW"))]
density

density <- density %>%
  rename(NUTS3 = geo, 
         year = TIME_PERIOD, 
         pop_density = OBS_VALUE)


data <- data %>%
  left_join(density, by = c("year","NUTS3"))

# Fill the NA values in pop_density for 2023 with the corresponding values from 2022
data<- data %>%
  group_by(NUTS3.Name) %>%
  fill(pop_density, .direction = "down")

################################################################################
############################ Mortality above 65 ################################
################################################################################


data <- data %>%
  mutate(Mort_Male = mort_Y65_69M + mort_Y70_74M + mort_Y75_79M + mort_Y80_84M + mort_Y85_89M + mort_Y_GE90M,
         Mort_Female = mort_Y65_69F + mort_Y70_74F + mort_Y75_79F + mort_Y80_84F + mort_Y85_89F + mort_Y_GE90F,
         Mort_Total = mort_Y65_69T + mort_Y70_74T + mort_Y75_79T + mort_Y80_84T + mort_Y85_89T + mort_Y_GE90T,
         Popu_Male = popu_Y65_69M + popu_Y70_74M + popu_Y75_79M + popu_Y80_84M + popu_Y85_89M + popu_Y_GE90M,
         Popu_Female = popu_Y65_69F + popu_Y70_74F + popu_Y75_79F + popu_Y80_84F + popu_Y85_89F + popu_Y_GE90F,
         Popu_Total = popu_Y65_69T + popu_Y70_74T + popu_Y75_79T + popu_Y80_84T + popu_Y85_89T + popu_Y_GE90T)

data <- data %>%
  mutate(Mort_Male65_79 = mort_Y65_69M + mort_Y70_74M + mort_Y75_79M,
         Mort_Female65_79 = mort_Y65_69F + mort_Y70_74F + mort_Y75_79F,
         Mort_Total65_79 = mort_Y65_69T + mort_Y70_74T + mort_Y75_79T ,
         Popu_Male65_79 = popu_Y65_69M + popu_Y70_74M + popu_Y75_79M,
         Popu_Female65_79 = popu_Y65_69F + popu_Y70_74F + popu_Y75_79F,
         Popu_Total65_79 = popu_Y65_69T + popu_Y70_74T + popu_Y75_79T)

data <- data %>%
  mutate(Mort_Male80_ =  mort_Y80_84M + mort_Y85_89M + mort_Y_GE90M,
         Mort_Female80_ =  mort_Y80_84F + mort_Y85_89F + mort_Y_GE90F,
         Mort_Total80_ =  mort_Y80_84T + mort_Y85_89T + mort_Y_GE90T,
         Popu_Male80_ =  popu_Y80_84M + popu_Y85_89M + popu_Y_GE90M,
         Popu_Female80_ =  popu_Y80_84F + popu_Y85_89F + popu_Y_GE90F,
         Popu_Total80_ =  popu_Y80_84T + popu_Y85_89T + popu_Y_GE90T)


#######################Cleaning pollutants duplicates###########################
max_na <- function(x) {
       if (all(is.na(x))) {
             return(NA)
         } else {
               return(max(x, na.rm = TRUE))
           }
   } 
   cleaned_data <- data %>%
       group_by(NUTS3.Name, year, week) %>%
       summarise(
            NO2 = max_na(NO2),
             O3 = max_na(O3),
             PM10 = max_na(PM10),
             PM2.5 = max_na(PM2.5),
             .groups = 'drop'
         )

data <- data %>%
          select(-NO2, -O3, -PM10, -PM2.5) %>% # Exclude the old pollutant columns
          left_join(cleaned_data, by = c("NUTS3.Name", "year", "week")) # Join with the cleaned data
 
   data <- final_data %>%
     relocate(NO2, .after = MaxTm) %>%
     relocate(O3, .after = MaxTm) %>%
     relocate(PM10, .after = MaxTm) %>%
    relocate(PM2.5, .after = MaxTm)



write.csv(data, file = "C:\\Users\\u271201\\Downloads\\data.csv", row.names = FALSE)

