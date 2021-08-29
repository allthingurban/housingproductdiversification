library(StatMeasures)

#[1] "ID"                     "FSU"                    "Second_stage_stratum"   "Sample_hhld"            "Centre"                 "Round"                 
#[7] "Schedule"               "Sample"                 "Sector"                 "NSS_region"             "District"               "Stratum"               
#[13] "Sub_stratum"            "Sub_Round"              "FOD_Sub_Region"         "Level"                  "Filler.x"               "dwelling_type"         
#[19] "living_rooms"           "other_rooms"            "area_living"            "area_other"             "area_covered_veranda"   "area_uncovered_veranda"
#[25] "total"                  "ventilation"            "married_couples"        "married_separateroom"   "Kitchen_type"           "fuel"                  
#[31] "Floor_type"             "Wall_type"              "Roof_type"              "rent"                   "Blank"                  "NSC"                   
#[37] "Multiplier"             "Filler.y"               "Plinth"                 "Floors"                 "Use_of_house"           "Period_built"          
#[43] "start_year"             "End_year"               "Condition"              "electricity"            "elec_type"              "drainage"              
#[49] "WW_disposal"            "garbage_disposal"       "garbage_agency"         "garbage_freq"           "Hhsize"                 "Religion"              
#[55] "Social_group"           "Land_possessed"         "exp_tot"                "tenure"                 "area_type"              "amtspent_last365days"  
#[61] "amt_spent"              "fin_source1"            "fin_source2"            "fin_source3"            "fin_source4"            "distance_male"         
#[67] "distance_female"        "distance_TG"            "DWsource"               "DWsufficient"           "DW_access"              "DW_distance"           
#[73] "freq_watersupply"       "metered_water"          "amt_paid"               "avg_amt"                "bath_distance"          "latrine_access"        
#[79] "water_nearlatrine"      "hh_head"                "age_group"              "activity_group"         "skill_group"            "is_student_hh"         
#[85] "mpce_quintile"


prepare_base_house_data = function(dwelling_unit_details,level6_health,Level3,level5_wash,individual_member_details){
  return(dwelling_unit_details %>%
           filter(Sector==2)%>%
           merge(select(level6_health,48,16:30),by.x = "ID", by.y = "ID",all.x = TRUE)%>%
           merge(select(Level3,2,13,14,17:20,25:36),by.x = c("FSU", "Second_stage_stratum","Sample_hhld"),by.y = c("FSU", "Second_stage_stratum","Sample_hhld"),all.x = TRUE)%>%
           merge(select(level5_wash,68,17,18,31,32,46:49,52,53,62),by.x = "ID", by.y = "ID",all.x = TRUE)%>%
           merge(select(add_student_hhhead(individual_member_details),16,17,21), by.x = "ID", by.y="hh_id",all.x = TRUE)%>%
           mutate(mpce_quintile=pentile(exp_tot/Hhsize)))
}

colnames(house_cat_base_data)

recode_dwelling_room = function(house_cat_base_data){
  return(house_cat_base_data %>% mutate(housing_unit_type=case_when((living_rooms==0 | is.na(living_rooms)) & is_student_hh==1 ~ "Hostel",
                                            (living_rooms==0 | is.na(living_rooms)) & is_student_hh==0 ~ "No Living Room",
                                            living_rooms==1 ~"1R",
                                            living_rooms==2 ~"2R",
                                            living_rooms>=3 ~"3plusR"),
                                        housing_unit_type = factor(housing_unit_type, 
                                                                   levels= c("No Living Room","Hostel","1R","2R","3plusR"),
                                                                   labels = c("No Living Room","Hostel","1R","2R","3plusR"))))
}


recode_area_cat = function(house_cat_base_data){
  return(house_cat_base_data %>% mutate(area_cat=case_when(total<100~"Very Small",
                                                           total>=100 & total < 300 ~ "Small",
                                                           total>=300 & total <600 ~ "Comfortable",
                                                           total>=600~"Large")))
}
#distance: not required to travel - 1; travelled a distance of: less than 1 k.m. - 2, 1 to 5 k.m. - 3,
#5 to 10 k.m. - 4, 10 to 15 k.m. - 5, 15 to 30 k.m. - 6, 30 k.m. or more - 7
recode_commute_cat = function(house_cat_base_data){
  return(house_cat_base_data %>% mutate(commute=case_when(distance_male ==9 & (distance_female !=9 & distance_TG!=9) ~ pmax(distance_female,distance_TG),
                                                          distance_female ==9 & (distance_male !=9 & distance_TG!=9) ~ pmax(distance_male,distance_TG),
                                                          distance_TG ==9 & (distance_male !=9 & distance_female!=9) ~ pmax(distance_female,distance_male),
                                                          distance_male !=9 & (distance_female ==9 & distance_TG==9) ~ distance_male,
                                                          distance_female !=9 & (distance_male ==9 & distance_TG==9) ~ distance_female,
                                                          distance_TG !=9 & (distance_male ==9 & distance_female==9) ~ distance_TG,
                                                          distance_TG !=9 & distance_male !=9 & distance_female!=9~ pmax(distance_female,distance_TG,distance_male),
                                                          distance_TG ==9 & distance_male ==9 & distance_female==9~9),
                                        commute_cat=case_when(commute==1~"No Commute",
                                                              commute %in% c(2,3)~"Upto 5 Km",
                                                              commute ==4~"5 to 10 Km",
                                                              commute ==5~"More than 10Km",
                                                              commute %in% c(6,7)~"More than 10Km",
                                                              commute==9~"Not Working"),
                                        commute_cat = factor(commute_cat, 
                                                                   levels= c("No Commute","Upto 5 Km","5 to 10 Km","More than 10Km","Not Working"),
                                                                   labels = c("No Commute","Upto 5 Km","5 to 10 Km","More than 10Km","Not Working"))))
}

recode_tenure_status = function(house_cat_base_data){
  return(house_cat_base_data %>% 
           mutate(tenure_status=case_when(tenure %in% c(1,2)~"Ownership",
                                          tenure %in% c(3,4)~"Formal Rental",
                                          tenure %in% c(5)~"Informal Rental",
                                          tenure %in% c(9)~"Others")))
}


recode_dw_access=function(house_cat_base_data){
  return(house_cat_base_data %>% mutate(dw_access_recoded=case_when(DW_access %in% c("1")~"Exclusive",
                                                                         DW_access %in% c("2","3")~"Shared",
                                                                         DW_access %in% c("5","4","6","7")~"Public",
                                                                         DW_access %in% c("9")~"Others")))
}

recode_floor_type=function(house_cat_base_data){
  return(house_cat_base_data %>% mutate(floor_type_recoded=case_when(Floor_type %in% c("1","2","3")~"Non-Cemented",
                                                                 Floor_type %in% c("4","5","6")~"Cemented",
                                                                 Floor_type %in% c("9")~"Others")))
}

recode_wall_type=function(house_cat_base_data){
  return(house_cat_base_data %>% mutate(wall_type_recoded=case_when(Wall_type %in% c("1","2","3","4","5","7")~"Kacha",
                                                                Wall_type %in% c("8","6",9)~"Pucca")))
}

recode_roof_type=function(house_cat_base_data){
  return(house_cat_base_data %>% mutate(roof_type_recoded=case_when(Roof_type %in% c("1","2","3","4")~"Temporary",
                                                                Roof_type %in% c("5","7")~"Semi_Permanent",
                                                                Roof_type %in% c("6","8","9")~"Permanent")))
}

#access of the household to latrine
#(exclusive use of household - 1, common use of households in the building - 2, public/community
#latrine without payment - 3, public/community latrine with payment - 4, others - 9, no latrine - 5)

recode_latrine_access=function(house_cat_base_data){
  return(house_cat_base_data %>% mutate(latrine_access_recoded=case_when(latrine_access %in% c("1")~"Exclusive",
                                                                    latrine_access %in% c("2")~"Shared",
                                                                    latrine_access %in% c("3","4","9")~"Public",
                                                                    latrine_access %in% c("5")~"No_Toilet")))
}

#(disposed to: bio-gas plant or manure pit - 1, household’s individual dumping
#spot(s) - 2, community dumping spot (vat, container, etc.) - 3, common place other
#than community dumping spot (open area/street/open drain) - 4, others - 9;
#not known - 5)

#agency made arrangement for collection of garbage of the household
#(panchayet/municipality/corporation - 1, resident/group of residents - 2,
#  others - 9, not known - 3; no arrangement - 4)

#for 3 and 4 in item 12, how frequently garbage is cleared?
#  (daily - 1, not daily but at least once in a week - 2, not even once in a week - 3,
#   not known - 4)

recode_solid_waste = function(house_cat_base_data){
  house_cat_base_data%>%
    mutate(solid_waste=case_when(garbage_disposal %in% c("1","2")~"Recycled",
                                 garbage_freq=="1"~ "Daily",
                                 garbage_freq=="2"~ "Weekly",
                                 garbage_freq %in% c("3","4")~"No Cleaning",
                                 garbage_disposal %in% c("9","5")~"others"))%>%
    filter(is.na(solid_waste))
}

recode_numoffloor=function(house_cat_base_data){
  return(house_cat_base_data%>%
    mutate(Floors_cat=case_when(Floors==1~"Single Floor",
                                Floors>1~"Multi_Floor")))
}

#Only for landlords
recode_house_age=function(house_cat_base_data){
  return(house_cat_base_data%>%
           mutate(case_when(Period_built %in% c(1:5)~"Less than 5",
                            Period_built %in% c(6)~"5 to 10 Years",
                            Period_built %in% c(7)~"10 to 20 Years",
                            Period_built %in% c(8,10,11,12)~"More than 20")))
}

recode_marrie_couple_room=function(house_cat_base_data){
  return(house_cat_base_data%>%
    mutate(is_couple_noroom=case_when(married_couples!=0 & married_couples == married_separateroom~1,#All married couples have room
                                      married_couples!=0 & married_couples > married_separateroom~2,#Some married couples do not have room
                                      married_couples==0~3)))#No married couples
}





