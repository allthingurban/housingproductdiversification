library(tidyr)
library(dplyr)
#Categorize all individual members according to their age, skill, principal econoimc activity, and marital status

#[1] "Centre"                     "FSU"                        "Round"                      "Schedule"                  
#[5] "Sample"                     "Sector"                     "NSS_region"                 "District"                  
#[9] "Stratum"                    "Sub_stratum"                "Sub_Round"                  "FOD_Sub_Region"            
#[13] "Second_stage_stratum"       "Sample_hhld"                "Level"                      "Filler"                    
#[17] "Person_srl_no"              "Relationship_to_head"       "Gender"                     "Age"                       
#[21] "Marital_status"             "Education"                  "Principal_activity"         "Industry"                  
#[25] "occupation"                 "use_of_latrine"             "latrine_type"               "exclusive_latrine_forhouse"
#[29] "reason_notusing_latrine"    "blank"                      "NSC"                        "Multiplier"                
#[33] "ID" 

#Select all relevant columns
individual_member_details = individual_member_details %>%
  select(33,2,6,7,8,13,14,18:23,25,32)

#Add Household Id and column for recognising who is household head
individual_member_details=create_hhid_hhead(individual_member_details)

#Add coumns for activity group
individual_member_details = create_activity_group(individual_member_details)

#Add columns for skill group
individual_member_details = create_skill_group(individual_member_details)

#Add columns if hhhead is student
individual_member_details = add_student_hhhead(individual_member_details)

#[1] "ID"                   "FSU"                  "Sector"               "NSS_region"           "District"            
#[6] "Second_stage_stratum" "Sample_hhld"          "Relationship_to_head" "Gender"               "Age"                 
#[11] "Marital_status"       "Education"            "Principal_activity"   "occupation"           "Multiplier"          
#[16] "hh_id"                "hh_head"              "age_group"            "activity_group"       "skill_group" 




  
