install.packages("scales")
install.packages("hrbrthemes")
install.packages("gfonts")
library(tidyverse)
library(eurostat)
library(scales)
library(lubridate)
library(ggpubr)
library(ggthemes)
library(hrbrthemes)
library(gfonts)
library(dplyr)
library(purrr)
library(lubridate)
hrbrthemes::import_roboto_condensed()


str(pop_mort)

pop_mort <-inner_join(death1, pop, by = c("geo", "NUTS3", "NUTS2","NUTS1", "sex", "age","year")) %>%
  rename(mort = OBS_VALUE.x, popu = OBS_VALUE.y)
pop_mort <- pop_mort[, !(names(pop_mort) %in% c("OBS_FLAG.x", "OBS_FLAG.y"))]