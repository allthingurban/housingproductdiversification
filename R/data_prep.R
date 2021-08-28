library(haven)

#Select relevant columns
dwelling_unit_details <- read_dta("/media/abhinav/Data/NSSO/NSS76/level7_dwelling.dta")
individual_member_details <- read_dta("/media/abhinav/Data/NSSO/NSS76/level2_individualLevel.dta")
Level3 <- read_dta("Level3.dta")
level5_wash <- read_dta("level5_wash.dta")
level6_health <- read_dta("level6_health.dta")
