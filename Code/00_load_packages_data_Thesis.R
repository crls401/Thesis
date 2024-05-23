# R script to prepare data and lagged variables for INLA-DLNM modelling

# Install and load INLA
install.packages("INLA", repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/testing"), dep=TRUE)
install.packages("data.table")
install.packages("tidyverse")
install.packages("sf")
install.packages("sp")
install.packages("spdep")
install.packages("dlnm")
install.packages("tsModel")
install.packages("Metrics")
install.packages("RColorBrewer")
install.packages("geofacet")
install.packages("ggpubr")
install.packages("ggthemes")
install.packages("caret")
install.packages("gfonts")
install.packages("eurostat")
install.packages("lubridate")
install.packages("scales")
install.packages("hrbrthemes")
install.packages("remotes")
install.packages("openxlsx")

library(data.table)
library(INLA)
library(tidyverse)
library(sf)
library(sp)
library(spdep)
library(dlnm)
library(tsModel)
library(Metrics)
library(RColorBrewer)
library(geofacet)
library(ggpubr)
library(ggthemes)
library(caret)
library(remotes)
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




################################NUTS############################################
path7 <- "C:\\Users\\u271201\\Downloads\\Data\\nuts\\estat_demo_r_mweek3_filtered_en.csv.gz"
nuts <- read.csv(path7)
str(nuts)

nuts <- nuts[, !(names(nuts) %in% c("LAST.UPDATE","DATAFLOW", "sex","TIME_PERIOD", "freq","unit", "OBS_FLAG", "OBS_VALUE"))]
nuts <- separate(nuts, geo, into = c("NUTS3", "NUTS3.Name"), sep = ":", remove = FALSE)
nuts <- nuts[, !(names(nuts) %in% c("geo","age"))]
###############################Data##############################################
# load pre-defined grid of Spain regions for geofacet plots
# note: could use pre-loaded grid = "spain_prov_grid2" in geofacet package, would need match state names
grid<- spain_prov_grid2

# Manual mapping of 'poll' labels to 'nuts' labels
name_mapping <- data.frame(
  poll_name = c("Coruña", "Gipuzkoa", "Lugo", "Asturias", "Cantabria", "Bizkaia", "Álava", "Girona", "Huesca", "León", "Lleida", "Navarra", "Ourense", "Palencia", "Pontevedra", "Barcelona", "Burgos", "Rioja", "Soria", "Valladolid", "Zamora", "Zaragoza", "Ávila", "Guadalajara", "Salamanca", "Segovia", "Tarragona", "Teruel", "Cáceres", "Castellón", "Cuenca", "Madrid", "Toledo", "Valencia", "Balears", "Albacete", "Alicante", "Badajoz", "Ciudad Real", "Córdoba", "Almería", "Huelva", "Jaén", "Murcia", "Sevilla", "Cádiz", "Granada", "Málaga", "Palmas", "S.C. Tenerife", "Ceuta", "Melilla"),
  nuts_name = c("A Coruña", "Gipuzkoa", "Lugo", "Asturias", "Cantabria", "Bizkaia", "Araba/Álava", "Girona", "Huesca", "León", "Lleida", "Navarra", "Ourense", "Palencia", "Pontevedra", "Barcelona", "Burgos", "La Rioja", "Soria", "Valladolid", "Zamora", "Zaragoza", "Ávila", "Guadalajara", "Salamanca", "Segovia", "Tarragona", "Teruel", "Cáceres", "Castellón/Castelló", "Cuenca", "Madrid", "Toledo", "Valencia/València", "Mallorca", "Albacete", "Alicante/Alacant", "Badajoz", "Ciudad Real", "Córdoba", "Almería", "Huelva", "Jaén", "Murcia", "Sevilla", "Cádiz", "Granada", "Málaga", "Gran Canaria", "Tenerife", "Ceuta", "Melilla"),
  stringsAsFactors = FALSE
)
# Do similar replacements for 'Temp_sp$PROVINCIA' and any other datasets or columns
nuts$NUTS3.Name <- with(name_mapping, poll_name [match(nuts$NUTS3.Name,nuts_name)])

grid <- grid %>%
  left_join(nuts, by = c("name" = "NUTS3.Name"))

names(grid)[names(grid) == "NUTS3"] <- "code_num"

write.csv(grid, file = "C:\\Users\\u271201\\Downloads\\grid.csv", row.names = FALSE)

# load data
# note Mortality data available from 2017 
# Climate data included for 2000 to obtained lagged values prior to 2001
data <- fread("C:\\Users\\u271201\\Downloads\\data.csv", header = T)
head(data)

# load shape file for Spain
map <- read_sf("C:/Users/u271201/Downloads/NUTS_RG_20M_2021_3035.shp/NUTS_RG_20M_2021_3035.shp")
# dim(map)
map <- map %>%
  filter(LEVL_CODE == 3, CNTR_CODE == "ES") %>%
  select(NUTS_ID, geometry)

map <- map %>% 
  filter(NUTS_ID %in% unique(data$NUTS3))

# Create adjacency matrix
nb.map <- poly2nb(as_Spatial(map$geometry))
g.file <- "C:\\Users\\u271201\\Downloads\\Data\\map.graph"
if (!file.exists(g.file)) nb2INLA(g.file, nb.map)

########################## CODE ###########################################
# Remove year 2020 from lagged climate variables

data <- data[data$year!= 2020,]

# set maximum lag
nlag = 3  # Number of lags
nlagpo = 2 # Number of lags
# No2
lag_NO2 <- tsModel::Lag(data$NO2, group = data$week, k = 0:nlagpo)
#PM10
lag_PM10 <- tsModel::Lag(data$PM10, group = data$week, k = 0:nlagpo)
#PM25
lag_PM25 <- tsModel::Lag(data$PM2.5, group = data$week, k = 0:nlagpo)
#O3
lag_O3 <- tsModel::Lag(data$O3, group = data$week, k = 0:nlagpo)
# Minimum temperature (Tmin)
lag_tmin <- tsModel::Lag(data$MinTm, group = data$week, k = 0:nlag)
# Maximum temperature (Tmax)
lag_tmax <- tsModel::Lag(data$MaxTm, group = data$week, k = 0:nlag)
#Mean Temperature
lag_Tmid<- tsModel::Lag(data$Tmid, group = data$week, k = 0:nlag)



# Add a new column for month, assuming each month has exactly 4 weeks. This way of defining months is for convenience, due to the different sources of data the merge without overlapping in years and weeks has been imposible.  
data[, month := ceiling(week / 4.3485)]  # 52 weeks divided by 12 months

# define dimensions
#Creating time column
# Assuming the first year in your dataset is the base year
data$week <- as.integer(sub("W", "", data$week))

data$time <- (data$year - min(data$year)) * 52 + data$week

ntime <- length(unique(data$time))
# total number of years
nyear <- length(unique(data$year))

# total number of weeks
nweek <- length(unique(data$week))

# total number of NUTS3 regions (assuming NUTS3 is a region identifier)
nnuts3 <- length(unique(data$NUTS3))

# define cross-basis matrix (combining nonlinear exposure and lag functions)
# set lag knots
lagknotpo = equalknots(0:nlagpo, 2)
lagknot = equalknots(0:nlag, 2)
basis_NO2 <- crossbasis(lag_NO2, argvar = list(fun = "ns", knots = quantile(data$NO2, c(50,70,90) / 100, na.rm = TRUE)), arglag = list(fun = "ns", knots = nlag/3))
basis_PM10 <- crossbasis(lag_PM10, argvar = list(fun = "ns", knots = quantile(data$PM10, c(50,70,90)/ 100, na.rm = TRUE)), arglag = list(fun = "ns", knots = nlag/3))
basis_PM25 <- crossbasis(lag_PM25, argvar = list(fun = "ns", knots = quantile(data$PM2.5, c(50,70,90) / 100, na.rm = TRUE)), arglag = list(fun = "ns", knots = nlag/3))
basis_O3 <- crossbasis(lag_O3, argvar = list(fun = "ns", knots = quantile(data$O3,c(50,70,90) / 100, na.rm = TRUE)), arglag = list(fun = "ns", knots = nlag/3))
basis_Temperature <- crossbasis(lag_Tmid, argvar = list(fun = "ns", knots = quantile(data$Tmid,c(10,50,90) / 100, na.rm = TRUE)), arglag = list(fun = "ns", knots = nlag/4))
basis_MaxTm <- crossbasis(lag_tmax, argvar = list(fun = "ns", knots = quantile(data$MaxTm,c(10,50,90) / 100, na.rm = TRUE)), arglag = list(fun = "ns", knots = nlag/4))
basis_MinTm <- crossbasis(lag_tmin, argvar = list(fun = "ns", knots = quantile(data$MinTm,c(10,50,90) / 100, na.rm = TRUE)), arglag = list(fun = "ns", knots = nlag/4))

# test linear interaction with % of land use by province 
#summary(data$land_use)
# Set indicator to zero at the point of interest (centering point)
# Re-parameterize model to extract different predictions
#land_use_ind1 <- data$land_use - quantile(data$land_use, p = 0.75) # Highly urbanized
#land_use_ind2 <- data$land_use - quantile(data$land_use, p = 0.5)  # Intermediate
#land_use_ind3 <- data$land_use - quantile(data$land_use, p = 0.25) # More rural

# test linear interaction with density of population by province 
summary(data$pop_density)
# Set indicator to zero at the point of interest (centering point)
# Re-parameterize model to extract different predictions
pop_density_ind1 <- data$pop_density - quantile(data$pop_density, p = 0.90) # High density
pop_density_ind2 <- data$pop_density - quantile(data$pop_density, p = 0.75)  # Intermediate density
pop_density_ind3 <- data$pop_density - quantile(data$pop_density, p = 0.50) # Low density

# Interaction terms between % of land use and  density of population by province 
#interaction1 <- land_use_ind1 * pop_density_ind1
#interaction2 <- land_use_ind2 * pop_density_ind2
#interaction3 <- land_use_ind3 * pop_density_ind3


# Creating a ineraction variable
#data$interaction <- data$land_use * data$pop_density

# Multiply each cross-basis variable by the linear terms (see Gasparrini et al. EHP 2015)
# note: exploit the column by column product 

# Highly urbanized areas
#basis_NO2_land_use1 <- basis_NO2 * land_use_ind1
#basis_PM10_land_use1 <- basis_PM10 * land_use_ind1
#basis_PM25_land_use1 <- basis_PM25 * land_use_ind1
#basis_O3_land_use1 <- basis_O3 * land_use_ind1
#basis_MaxTm_land_use1 <- basis_MaxTm * land_use_ind1
#basis_MinTm_land_use1 <- basis_MinTm * land_use_ind1

# Intermediate urbanization
#basis_NO2_land_use2 <- basis_NO2 * land_use_ind2
#basis_PM10_land_use2 <- basis_PM10 * land_use_ind2
#basis_PM25_land_use2 <- basis_PM25 * land_use_ind2
#basis_O3_land_use2 <- basis_O3 * land_use_ind2
#basis_MaxTm_land_use2 <- basis_MaxTm * land_use_ind2
#basis_MinTm_land_use2 <- basis_MinTm * land_use_ind2

# More rural areas
#basis_NO2_land_use3 <- basis_NO2 * land_use_ind3
#basis_PM10_land_use3 <- basis_PM10 * land_use_ind3
#basis_PM25_land_use3 <- basis_PM25 * land_use_ind3
#basis_O3_land_use3 <- basis_O3 * land_use_ind3
#basis_MaxTm_land_use3 <- basis_MaxTm * land_use_ind3
#basis_MinTm_land_use3 <- basis_MinTm * land_use_ind3


# High density
basis_NO2_pop_density1 <- basis_NO2*pop_density_ind1
basis_PM10_pop_density1 <- basis_PM10*pop_density_ind1
basis_PM25_pop_density1 <- basis_PM25*pop_density_ind1
basis_O3_pop_density1 <- basis_O3*pop_density_ind1
basis_MaxTm_pop_density1 <- basis_MaxTm*pop_density_ind1
basis_MinTm_pop_density1 <- basis_MinTm*pop_density_ind1

# Intermediate density
basis_NO2_pop_density2 <- basis_NO2*pop_density_ind2
basis_PM10_pop_density2 <- basis_PM10*pop_density_ind2
basis_PM25_pop_density2 <- basis_PM25*pop_density_ind2
basis_O3_pop_density2 <- basis_O3*pop_density_ind2
basis_MaxTm_pop_density2 <- basis_MaxTm*pop_density_ind2
basis_MinTm_pop_density2 <- basis_MinTm*pop_density_ind2

# Low density
basis_NO2_pop_density3 <- basis_NO2*pop_density_ind3
basis_PM10_pop_density3 <- basis_PM10*pop_density_ind3
basis_PM25_pop_density3 <- basis_PM25*pop_density_ind3
basis_O3_pop_density3 <- basis_O3*pop_density_ind3
basis_MaxTm_pop_density3 <- basis_MaxTm*pop_density_ind3
basis_MinTm_pop_density3 <- basis_MinTm*pop_density_ind3

#Iteraction effects 
# Multiply cross-basis variables by interaction terms
# Interaction with first level (highly urbanized and high density)
#basis_NO2_interaction1 <- basis_NO2 * interaction1
#basis_PM10_interaction1 <- basis_PM10 * interaction1
#basis_PM25_interaction1 <- basis_PM25 * interaction1
#basis_O3_interaction1 <- basis_O3 * interaction1
#basis_MaxTm_interaction1 <- basis_MaxTm * interaction1
#basis_MinTm_interaction1 <- basis_MinTm * interaction1

# Interaction with second level (intermediate urbanization and intermediate density)
#basis_NO2_interaction2 <- basis_NO2 * interaction2
#basis_PM10_interaction2 <- basis_PM10 * interaction2
#basis_PM25_interaction2 <- basis_PM25 * interaction2
#basis_O3_interaction2 <- basis_O3 * interaction2
#basis_MaxTm_interaction2 <- basis_MaxTm * interaction2
#basis_MinTm_interaction2 <- basis_MinTm * interaction2

# Interaction with third level (more rural and low density)
#basis_NO2_interaction3 <- basis_NO2 * interaction3
#basis_PM10_interaction3 <- basis_PM10 * interaction3
#basis_PM25_interaction3 <- basis_PM25 * interaction3
#basis_O3_interaction3 <- basis_O3 * interaction3
#basis_MaxTm_interaction3 <- basis_MaxTm * interaction3
#basis_MinTm_interaction3 <- basis_MinTm * interaction3




# assign unique column names to cross-basis matrix for inla() model
# note: not necessary for glm(), gam() or glm.nb() models
# Assign unique column names to cross-basis matrices
colnames(basis_NO2) <- paste0("basis_NO2.", colnames(basis_NO2))
colnames(basis_PM10) <- paste0("basis_PM10.", colnames(basis_PM10))
colnames(basis_PM25) <- paste0("basis_PM25.", colnames(basis_PM25))
colnames(basis_O3) <- paste0("basis_O3.", colnames(basis_O3))
colnames(basis_Temperature) <- paste0("basis_Temperature.", colnames(basis_Temperature))
colnames(basis_MaxTm) <- paste0("basis_MaxTm.", colnames(basis_MaxTm))
colnames(basis_MinTm) <- paste0("basis_MinTm.", colnames(basis_MinTm))


# Assign unique column names to interaction cross-basis matrices. This i necessary for INLA MODELS.
# Urbanized areas
#colnames(basis_NO2_land_use1) <- paste0("basis_NO2_land_use1.", colnames(basis_NO2_land_use1))
#colnames(basis_PM10_land_use1) <- paste0("basis_PM10_land_use1.", colnames(basis_PM10_land_use1))
#colnames(basis_PM25_land_use1) <- paste0("basis_PM25_land_use1.", colnames(basis_PM25_land_use1))
#colnames(basis_O3_land_use1) <- paste0("basis_O3_land_use1.", colnames(basis_O3_land_use1))
#colnames(basis_MaxTm_land_use1) <- paste0("basis_MaxTm_land_use1.", colnames(basis_MaxTm_land_use1))
#colnames(basis_MinTm_land_use1) <- paste0("basis_MinTm_land_use1.", colnames(basis_MinTm_land_use1))

# Intermediate urbanization
#colnames(basis_NO2_land_use2) <- paste0("basis_NO2_land_use2.", colnames(basis_NO2_land_use2))
#colnames(basis_PM10_land_use2) <- paste0("basis_PM10_land_use2.", colnames(basis_PM10_land_use2))
#colnames(basis_PM25_land_use2) <- paste0("basis_PM25_land_use2.", colnames(basis_PM25_land_use2))
#colnames(basis_O3_land_use2) <- paste0("basis_O3_land_use2.", colnames(basis_O3_land_use2))
#colnames(basis_MaxTm_land_use2) <- paste0("basis_MaxTm_land_use2.", colnames(basis_MaxTm_land_use2))
#colnames(basis_MinTm_land_use2) <- paste0("basis_MinTm_land_use2.", colnames(basis_MinTm_land_use2))
# More rural areas
#colnames(basis_NO2_land_use3) <- paste0("basis_NO2_land_use3.", colnames(basis_NO2_land_use3))
#colnames(basis_PM10_land_use3) <- paste0("basis_PM10_land_use3.", colnames(basis_PM10_land_use3))
#colnames(basis_PM25_land_use3) <- paste0("basis_PM25_land_use3.", colnames(basis_PM25_land_use3))
#colnames(basis_O3_land_use3) <- paste0("basis_O3_land_use3.", colnames(basis_O3_land_use3))
#colnames(basis_MaxTm_land_use3) <- paste0("basis_MaxTm_land_use3.", colnames(basis_MaxTm_land_use3))
#colnames(basis_MinTm_land_use3) <- paste0("basis_MinTm_land_use3.", colnames(basis_MinTm_land_use3))

# High density
colnames(basis_NO2_pop_density1) <- paste0("basis_NO2_pop_density1.", colnames(basis_NO2_pop_density1))
colnames(basis_PM10_pop_density1) <- paste0("basis_PM10_pop_density1.", colnames(basis_PM10_pop_density1))
colnames(basis_PM25_pop_density1) <- paste0("basis_PM25_pop_density1.", colnames(basis_PM25_pop_density1))
colnames(basis_O3_pop_density1) <- paste0("basis_O3_pop_density1.", colnames(basis_O3_pop_density1))
colnames(basis_MaxTm_pop_density1) <- paste0("basis_MaxTm_pop_density1.", colnames(basis_MaxTm_pop_density1))
colnames(basis_MinTm_pop_density1) <- paste0("basis_MinTm_pop_density1.", colnames(basis_MinTm_pop_density1))


# Intermediate density
colnames(basis_NO2_pop_density2) <- paste0("basis_NO2_pop_density2.", colnames(basis_NO2_pop_density2))
colnames(basis_PM10_pop_density2) <- paste0("basis_PM10_pop_density2.", colnames(basis_PM10_pop_density2))
colnames(basis_PM25_pop_density2) <- paste0("basis_PM25_pop_density2.", colnames(basis_PM25_pop_density2))
colnames(basis_O3_pop_density2) <- paste0("basis_O3_pop_density2.", colnames(basis_O3_pop_density2))
colnames(basis_MaxTm_pop_density2) <- paste0("basis_MaxTm_pop_density2.", colnames(basis_MaxTm_pop_density2))
colnames(basis_MinTm_pop_density2) <- paste0("basis_MinTm_pop_density2.", colnames(basis_MinTm_pop_density2))


# Low density
colnames(basis_NO2_pop_density3) <- paste0("basis_NO2_pop_density3.", colnames(basis_NO2_pop_density3))
colnames(basis_PM10_pop_density3) <- paste0("basis_PM10_pop_density3.", colnames(basis_PM10_pop_density3))
colnames(basis_PM25_pop_density3) <- paste0("basis_PM25_pop_density3.", colnames(basis_PM25_pop_density3))
colnames(basis_O3_pop_density3) <- paste0("basis_O3_pop_density3.", colnames(basis_O3_pop_density3))
colnames(basis_MaxTm_pop_density3) <- paste0("basis_MaxTm_pop_density3.", colnames(basis_MaxTm_pop_density3))
colnames(basis_MinTm_pop_density3) <- paste0("basis_MinTm_pop_density3.", colnames(basis_MinTm_pop_density3))


# Interaction effects
#colnames(basis_NO2_interaction1) <- paste0("basis_NO2_interaction1.", colnames(basis_NO2_interaction1))
#colnames(basis_PM10_interaction1) <- paste0("basis_PM10_interaction1.", colnames(basis_PM10_interaction1))
#colnames(basis_PM25_interaction1) <- paste0("basis_PM25_interaction1.", colnames(basis_PM25_interaction1))
#colnames(basis_O3_interaction1) <- paste0("basis_O3_interaction1.", colnames(basis_O3_interaction1))
#colnames(basis_MaxTm_interaction1) <- paste0("basis_MaxTm_interaction1.", colnames(basis_MaxTm_interaction1))
#colnames(basis_MinTm_interaction1) <- paste0("basis_MinTm_interaction1.", colnames(basis_MinTm_interaction1))

#colnames(basis_NO2_interaction2) <- paste0("basis_NO2_interaction2.", colnames(basis_NO2_interaction2))
#colnames(basis_PM10_interaction2) <- paste0("basis_PM10_interaction2.", colnames(basis_PM10_interaction2))
#colnames(basis_PM25_interaction2) <- paste0("basis_PM25_interaction2.", colnames(basis_PM25_interaction2))
#colnames(basis_O3_interaction2) <- paste0("basis_O3_interaction2.", colnames(basis_O3_interaction2))
#colnames(basis_MaxTm_interaction2) <- paste0("basis_MaxTm_interaction2.", colnames(basis_MaxTm_interaction2))
#colnames(basis_MinTm_interaction2) <- paste0("basis_MinTm_interaction2.", colnames(basis_MinTm_interaction2))

#colnames(basis_NO2_interaction3) <- paste0("basis_NO2_interaction3.", colnames(basis_NO2_interaction3))
#colnames(basis_PM10_interaction3) <- paste0("basis_PM10_interaction3.", colnames(basis_PM10_interaction3))
#colnames(basis_PM25_interaction3) <- paste0("basis_PM25_interaction3.", colnames(basis_PM25_interaction3))
#colnames(basis_O3_interaction3) <- paste0("basis_O3_interaction3.", colnames(basis_O3_interaction3))
#colnames(basis_MaxTm_interaction3) <- paste0("basis_MaxTm_interaction3.", colnames(basis_MaxTm_interaction3))
#colnames(basis_MinTm_interaction3) <- paste0("basis_MinTm_interaction3.", colnames(basis_MinTm_interaction3))





#Create an Index for NUTS3 Regions (assuming NUTS3 is a region identifier similar to microregions)
# Assuming the number of unique NUTS3 regions is previously calculated as nnuts3
# Create a factor variable for NUTS3 regions and get the integer codes
data$NUTS3_factor <- as.integer(as.factor(data$NUTS3))

# Map each NUTS3 region to a unique index number
NUTS3_levels <- levels(factor(data$NUTS3))
NUTS3_mapping <- setNames(seq_along(NUTS3_levels), NUTS3_levels)

# Apply the mapping to create the nuts3_index column
data$nuts3_index <- rep(1:nnuts3, length.out = nrow(data))

#Create a Year Index:
#Assuming the earliest year in your dataset is 2017 and you want to set it to 1:

data$year_index <- data$year - 2016  # Subtract 2016 to make 2017 as 1




# set data for models 
Y  <- data$Mort_Total80_ # response variable
N  <- length(Y) # total number of data points
E  <- data$Popu_Total80_/10^5 # model offset so that response is equivalent to an incidence rate per 100,000 people
T1 <- data$week # for random effect to account for annual cycle (seasonality)
T2 <- data$year_index # for random effect to account for inter-annual variability
S1 <- data$nuts3_index # for micro region spatial random effect
S2 <- data$nuts3_index # for state interaction with month random effect
Vw <- data$pop_density # include population density
M  <- data$month

NUTS3 <- as.factor(data$NUTS3)

# create dataframe for model testing
df <- data.frame(Y, E, T1, T2, S1, S2, Vw, M, NUTS3)

df$Month <- as.factor(df$M)
df$Year <- as.factor(df$T2)

# define priors
precision.prior <- list(prec = list(prior = "pc.prec", param = c(0.5, 0.01)))

# inla model function

# include formula and set defaults for data, family (to allow other prob dist models e.g. Poisson) and config (to allow for sampling)
mymodel <- function(formula, data = df, family = "nbinomial", config = FALSE)
  
{
  model <- inla(formula = formula, data = data, family = family, offset = log(E),
                control.inla = list(strategy = 'adaptive'), 
                control.compute = list(dic = TRUE, config = config, 
                                       cpo = TRUE, return.marginals = FALSE),
                control.fixed = list(correlation.matrix = TRUE, 
                                     prec.intercept = 1, prec = 1),
                control.predictor = list(link = 1, compute = TRUE), 
                verbose = FALSE)
  model <- inla.rerun(model)
  return(model)
}