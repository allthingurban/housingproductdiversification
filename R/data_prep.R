house_cat_base_data=prepare_base_house_data(dwelling_unit_details)
library(haven)

#Select relevant columns
dwelling_unit_details <- read_dta("/media/abhinav/Data/NSSO/NSS76/level7_dwelling.dta")
individual_member_details <- read_dta("/media/abhinav/Data/NSSO/NSS76/level2_individualLevel.dta")
Level3 <- read_dta("Level3.dta")
level5_wash <- read_dta("level5_wash.dta")
level6_health <- read_dta("level6_health.dta")

house_cat_base_data=prepare_base_house_data(dwelling_unit_details,level6_health,Level3,level5_wash,individual_member_details)
