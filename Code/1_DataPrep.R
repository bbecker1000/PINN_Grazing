library(tidyverse)
library(readr)
Wide_2 <- read_csv("Data/Wide_2.csv")
Wide_2

unique(Wide_2$Subtreatment)
unique(Wide_2$Treatment)

#Check that all rows have data?
Wide_2[-c(1:4)] %>% filter(if_all(everything(.), ~. != 0))


Soil <- read_csv("Data/Soil_2023.csv")
Soil
