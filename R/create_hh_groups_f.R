library(dplyr)
library(tidyr)
#Functions for preparing household groups data based on details of individual members

#Create age-groups
create_age_group = function(individual_detail){
  return(individual_detail %>%
    mutate(age_group=case_when(Age<=14~"Child",
                               Age>14 & Age<=24~"Youth",
                               Age>24 & Age<=40~"Young_Adult",
                               Age>40 & Age<=60~"Adult",
                               Age>60~"Senior")))
}

#Recode Occupational Activity Groups to all individuals
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

#Assign Skill groups to all individuals
create_skill_group = function(individual_detail){
  return(individual_detail %>%
    mutate(skill_group = case_when(substr(occupation,1,1)%in% c("9")~"Skill_1",
                                   substr(occupation,1,1)%in% c("3","4","5","6","7","8")~"Skill_2",
                                   substr(occupation,1,1)%in% c("2")~"Skill_3",
                                   substr(occupation,1,1)%in% c("1")~"Skill_4")))
}

#Create household id and a column to recognizze hh head
create_hhid_hhead= function(individual_detail){
  return(individual_detail %>%
           mutate(hh_id=as.numeric(paste(FSU,Second_stage_stratum,Sample_hhld,sep = "")),
                  hh_head=ifelse(Relationship_to_head==1,1,0)))
}

# If the household head is senior citizen then;
#1. Check if there is young adult or adult (male and female) who is working, if yes, then consider him/her as household head
#for categorizing age group
#2. Check if the only other household member in the working population is his/her spouse, if yes, then age group remains senior 
#2. If there are none then the age group for the household remains Senior
check_senior_household = function(individual_member_details,senior_household_member){
  return(individual_member_details %>%
    filter(hh_id %in% senior_household_member$hh_id & age_group %in% c("Youth","Young_Adult","Adult") & 
             Principal_activity %in% c("11","21","12","31","41","81","51","93")))
}

#For each household recognize the number of people in each occupational group
create_occupation_hh = function(individual_member_details,sector){
  return(individual_member_details%>%
           filter(Sector==sector)%>%
    group_by(hh_id, activity_group)%>%
    count()%>%
    spread(key = activity_group,value = n)%>%
    replace(is.na(.),0))
}

#Determine the occupational group of the household
#If there is at least one member with a regular salaried/wage : HH is Regular Salaried
#If there is at no member with regular income, number of casual workers is greater than number of self-employed: HH is casual wage
#If there is at no member with regular income, number of casual workers is less than number of self-employed: HH is self employed
#If both casual wage and self employed are equal and non-zero: HH is self-employed
#In all other cases household is non-working
add_occugrp_col = function(individual_member_details,sector){
  return(create_occupation_hh(individual_member_details,sector)%>%
           mutate(occupation_group=case_when(Regular_Salaried!=0~"Regular_Salaried",
                                             Regular_Salaried==0 & Casual_wage > Self_Employed~"Casual_Wage",
                                             Regular_Salaried==0 & Casual_wage < Self_Employed~"Self_Employed",
                                             Casual_wage !=0 & Regular_Salaried==0 & Self_Employed!=0 & Casual_wage == Self_Employed ~ "Self_Employed",
                                             Casual_wage ==0 & Regular_Salaried==0 & Self_Employed==0~"No_Working")))
}

##For each household recognize the number of people in each skill group
create_skill_hh = function(individual_member_details,sector){
  return(individual_member_details%>%
           filter(Sector==sector)%>%
           group_by(hh_id, skill_group)%>%
           count()%>%
           spread(key = skill_group,value = n)%>%
           replace(is.na(.),0))
}  

#Determine the skill group of the household
#If there is at least one member with a skill level 4 : HH is Skill Level 4
#If there is no member with skill level 4, and at least one member with skill level 3: HH is skill level 3
#If there is no member with skill level 4 and Skill level 3, and at least one member with skill level 2: HH is skill level 2
#If there is no member with skill level 4, Skill level 3 and Skill level 2, and at least one member with skill level 1: HH is skill level 1
add_skill_col = function(individual_member_details){
  return(create_skill_hh(individual_member_details)%>%
           mutate(hh_skill_group=case_when(Skill_4 !=0~"Skill_4",
                                           Skill_4 ==0 & Skill_3!=0 ~"Skill_3",
                                           Skill_4 ==0 & Skill_3==0 & Skill_2!=0~"Skill_2",
                                           Skill_4 ==0 & Skill_3==0 & Skill_2==0 & Skill_1!=0~"Skill_1",
                                           Skill_4 ==0 & Skill_3==0 & Skill_2==0 & Skill_1==0~"No_Skill")))
}

add_student_hhhead = function(individual_member_details){
  return(individual_member_details%>%
           filter(hh_head==1)%>%
           mutate(is_student_hh=ifelse(hh_head==1 & Principal_activity==91,1,0)))
}
