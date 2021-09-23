#Step 1
library(haven)
library(tidyr)
library(dplyr)
library(StatMeasures)
library(pollster)

#Step 2
#Select relevant columns
dwelling_unit_details <- read_dta("/media/abhinav/Data/NSSO/NSS76/level7_dwelling.dta")
individual_member_details <- read_dta("/media/abhinav/Data/NSSO/NSS76/level2_individualLevel.dta")
Level3 <- read_dta("data/Level3.dta")
level5_wash <- read_dta("data/level5_wash.dta")
level6_health <- read_dta("data/level6_health.dta")

#Step 3
source("R/prepare_indi_mem_data.R")

#Step 4
house_cat_base_data=prepare_base_house_data(dwelling_unit_details,level6_health,Level3,level5_wash,individual_member_details)

#Step 5
HH_eco_group= merge(add_occugrp_col(individual_member_details,2),add_skill_col(individual_member_details,2),by.x="hh_id",by.y="hh_id",all.x=TRUE)%>%
  select(1,9,15)

#Step 6
house_cat_base_data = merge(house_cat_base_data, HH_eco_group,by.x="ID",by.y="hh_id",all.x=TRUE)
