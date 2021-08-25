library(tidyr)
library(dplyr)
#Functions for preparing household groups data based on details of individual members

create_age_group = function(individual_detail){
  return(individual_detail %>%
    mutate(age_group=case_when(Age<=14~"Child",
                               Age>14 & Age<=24~"Youth",
                               Age>24 & Age<=40~"Young_Adult",
                               Age>40 & Age<=60~"Adult",
                               Age>60~"Senior")))
}

create_activity_group = function(individual_detail){
  return(individual_detail %>%
    mutate(activity_group=case_when(Principal_activity %in% c("11","21","12")~"Self_Employed",
                                     Principal_activity %in% c("31")~"Regular_Salaried",
                                     Principal_activity %in% c("41","51")~"Casual_wage",
                                     Principal_activity %in% c("81")~"Unemployed",
                                     Principal_activity %in% c("91")~"Student",
                                     Principal_activity %in% c("92","93","95")~"Non_Working",
                                     Principal_activity %in% c("94","97")~"Others")))
}

create_skill_group = function(individual_detail){
  return(individual_detail %>%
    mutate(skill_group = case_when(substr(occupation,1,1)%in% c("9")~"Skill_1",
                                   substr(occupation,1,1)%in% c("3","4","5","6","7","8")~"Skill_2",
                                   substr(occupation,1,1)%in% c("2")~"Skill_3",
                                   substr(occupation,1,1)%in% c("1")~"Skill_4")))
}

create_hhid_hhead= function(individual_detail){
  return(individual_detail %>%
           mutate(hh_id=as.numeric(paste(FSU,Second_stage_stratum,Sample_hhld,sep = "")),
                  hh_head=ifelse(Relationship_to_head==1,1,0)))
}
         