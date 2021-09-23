library(pollster)
library(ggplot2)

base_style_graph=function(graph_style,x_title,y_title,legend_title){
  return(graph_style+
           xlab(x_title)+
           ylab(y_title)+
           labs(fill=legend_title)+
           theme(legend.position = "bottom")+
           scale_x_discrete(labels = function(x) lapply(strwrap(x, width = 10, simplify = FALSE), paste, collapse="\n")))
}

#Kitchen
graph_kitchen_housetype_quintile=recode_dwelling_room(house_cat_base_data) %>%
  filter(!housing_unit_type %in% c("Hostel"))%>%
  crosstab_3way(x=housing_unit_type,y=Kitchen_type,z=mpce_quintile,weight = Multiplier/100,format = "long")%>%
  ggplot(aes(x=housing_unit_type, y=pct, fill=Kitchen_type))+
  geom_col()+
  scale_fill_manual(values= c("1"="#D0AD5E", "2"="#6dab6b","3"="#006666"), limits = c("1", "2", "3"),
                    labels = c("Kitchen with Water", "Kitchen without water", "No Kitchen"))+
  facet_wrap(~mpce_quintile)+
  geom_text(mapping=aes(label = round(pct,1)),
            position = position_stack(vjust = 0.7),size=3)



#Per capita median area
graph_mepcarea_housetype_quintile=recode_dwelling_room(house_cat_base_data) %>%
  filter(!housing_unit_type %in% c("Hostel"))%>%
  group_by(housing_unit_type,mpce_quintile)%>%
  summarise(median_pcarea=median(total/Hhsize))%>%
  ggplot(aes(x=housing_unit_type, y=median_pcarea))+
  geom_col()+
  facet_wrap(~mpce_quintile)+
  geom_text(mapping=aes(label = round(median_pcarea,0)),
            position = position_stack(vjust = 0.7),size=3)

#Type of latrine access
graph_latrine_housetype_quintile=  recode_latrine_access(recode_dwelling_room(house_cat_base_data))%>%
  filter(!housing_unit_type %in% c("Hostel"))%>%
  crosstab_3way(x=housing_unit_type,y=latrine_access_recoded,z=mpce_quintile,weight = Multiplier/100,format = "long")%>%
  ggplot(aes(x=housing_unit_type, y=pct, fill=latrine_access_recoded))+
  geom_col()+
  scale_fill_manual(values= c("Exclusive"="#D0AD5E", "Shared"="#6dab6b","Public"="#006666","No_Toilet"="#5C899B"))+
  facet_wrap(~mpce_quintile)+
  geom_text(mapping=aes(label = round(pct,1)),
            position = position_stack(vjust = 0.7),size=3)
  

#Type of water access
graph_water_housetype_quintile=  recode_dw_access(recode_dwelling_room(house_cat_base_data))%>%
  filter(!housing_unit_type %in% c("Hostel"))%>%
  crosstab_3way(x=housing_unit_type,y=dw_access_recoded,z=mpce_quintile,weight = Multiplier/100,format = "long")%>%
  ggplot(aes(x=housing_unit_type, y=pct, fill=dw_access_recoded))+
  geom_col()+
  scale_fill_manual(values= c("Exclusive"="#D0AD5E", "Shared"="#6dab6b","Public"="#006666","Others"="#5C899B"))+
  facet_wrap(~mpce_quintile)+
  geom_text(mapping=aes(label = round(pct,1)),
            position = position_stack(vjust = 0.7),size=3)


#type of roof
graph_roof_housetype_qquintile = recode_roof_type(recode_dwelling_room(house_cat_base_data))%>%
  filter(!housing_unit_type %in% c("Hostel"))%>%
  crosstab_3way(x=housing_unit_type,y=roof_type_recoded,z=mpce_quintile,weight = Multiplier/100,format = "long")%>%
  ggplot(aes(x=housing_unit_type, y=pct, fill=roof_type_recoded))+
  geom_col()+
  scale_fill_manual(values= c("Temporary"="#D0AD5E", "Semi_Permanent"="#6dab6b","Permanent"="#006666"))+
  facet_wrap(~mpce_quintile)+
  geom_text(mapping=aes(label = round(pct,1)),
            position = position_stack(vjust = 0.7),size=3)

#type of wall
graph_wall_housetype_qquintile = recode_wall_type(recode_dwelling_room(house_cat_base_data))%>%
  filter(!housing_unit_type %in% c("Hostel"))%>%
  crosstab_3way(x=housing_unit_type,y=wall_type_recoded,z=mpce_quintile,weight = Multiplier/100,format = "long")%>%
  ggplot(aes(x=housing_unit_type, y=pct, fill=wall_type_recoded))+
  geom_col()+
  scale_fill_manual(values= c("Kacha"="#D0AD5E", "Pucca"="#6dab6b"))+
  facet_wrap(~mpce_quintile)+
  geom_text(mapping=aes(label = round(pct,1)),
            position = position_stack(vjust = 0.7),size=3)

#married couple
graph_couple_separate_quintile=recode_marrie_couple_room(recode_dwelling_room(house_cat_base_data))%>%
  filter(!housing_unit_type %in% c("Hostel"))%>%
  crosstab_3way(x=housing_unit_type,y=is_couple_noroom,z=mpce_quintile,weight = Multiplier/100,format = "long")%>%
  ggplot(aes(x=housing_unit_type, y=pct, fill=is_couple_noroom))+
  geom_col()+
  facet_wrap(~mpce_quintile)+
  geom_text(mapping=aes(label = round(pct,1)),
            position = position_stack(vjust = 0.7),size=3)+
  scale_fill_manual(values= c("1"="#D0AD5E", "2"="#6dab6b","3"="#006666"), limits = c("1", "2", "3"),
                    labels = c("All", "Some", "No Married Couuple"))

graph_dwellingtype_quintile=recode_dwelling_room(house_cat_base_data)%>%
  filter(!housing_unit_type %in% c("Hostel"))%>%
  crosstab_3way(x=housing_unit_type,y=dwelling_type,z=mpce_quintile,weight = Multiplier/100,format = "long")%>%
  ggplot(aes(x=housing_unit_type, y=pct, fill=dwelling_type))+
  geom_col()+
  facet_wrap(~mpce_quintile)+
  geom_text(mapping=aes(label = round(pct,1)),
            position = position_stack(vjust = 0.7),size=3)+
  scale_fill_manual(values= c("1"="#D0AD5E", "2"="#6dab6b","9"="#006666"), limits = c("1", "2", "9"),
                    labels = c("Independent", "Flat", "Others"))

graph_floor_quintile=recode_numoffloor(recode_dwelling_room(house_cat_base_data))%>%
  filter(!housing_unit_type %in% c("Hostel"))%>%
  crosstab_3way(x=housing_unit_type,y=Floors_cat,z=mpce_quintile,weight = Multiplier/100,format = "long")%>%
  ggplot(aes(x=housing_unit_type, y=pct, fill=Floors_cat))+
  geom_col()+
  facet_wrap(~mpce_quintile)+
  geom_text(mapping=aes(label = round(pct,1)),
            position = position_stack(vjust = 0.7),size=3)+
  scale_fill_manual(values= c("Single Floor"="#D0AD5E", "Multi_Floor"="#006666"), limits = c("Single Floor", "Multi_Floor"),
                    labels = c("Single Floor", "Multi Floor"))


graph_commute_quintile=recode_commute_cat(recode_dwelling_room(house_cat_base_data))%>%
  filter(!housing_unit_type %in% c("Hostel"))%>%
  crosstab_3way(x=housing_unit_type,y=commute_cat,z=mpce_quintile,weight = Multiplier/100,format = "long")%>%
  ggplot(aes(x=housing_unit_type, y=pct, fill=commute_cat))+
  geom_col()+
  facet_wrap(~mpce_quintile)+
  geom_text(mapping=aes(label = round(pct,1)),
            position = position_stack(vjust = 0.7),size=3)+
  scale_fill_manual(values= c("No Commute"="#D0AD5E","Upto 5 Km"="#006666","5 to 10 Km"="#6dab6b","More than 10Km"="#5C899B","Not Working"="#989B5C"))

graph_area_quintile=recode_dwelling_room(house_cat_base_data)%>%
  filter(!housing_unit_type %in% c("Hostel"))%>%
  crosstab_3way(x=housing_unit_type,y=area_type,z=mpce_quintile,weight = Multiplier/100,format = "long")%>%
  ggplot(aes(x=housing_unit_type, y=pct, fill=area_type))+
  geom_col()+
  facet_wrap(~mpce_quintile)+
  geom_text(mapping=aes(label = round(pct,1)),
            position = position_stack(vjust = 0.7),size=3)+
  scale_fill_manual(values= c("1"="#D0AD5E", "2"="#6dab6b",'3'="#989B5C","9"="#006666"), limits = c("1", "2","3","9"),
                    labels = c("Notified Slum", "Non-Notified Slum", "Squatter","Non-Slum"))



recode_dwelling_room(house_cat_base_data)%>%
  filter(!housing_unit_type %in% c("Hostel"))%>%
  crosstab_3way(x=housing_unit_type,y=area_type,z=mpce_quintile,weight = Multiplier/100,format = "long")%>%
  rename(x_axis="housing_unit_type",
         y_axis="pct",
         fill_col="area_type",
         z_axis="mpce_quintile")


create_graphs=function(data_graph,values_df,limits_df=NULL,labels_df=NULL,x_title=NULL,y_title=NULL,legend_title=NULL,facet_labels){
  return(ggplot(data = data_graph,aes(x=x_axis, y=y_axis, fill=fill_col))+
    geom_col()+
    facet_wrap(~z_axis,labeller = facet_labels)+
    geom_text(mapping=aes(label = round(y_axis,1)),
              position = position_stack(vjust = 0.7),size=3)+
    scale_fill_manual(values= values_df, limits = limits_df,
                      labels = labels_df)+
    xlab(x_title)+
    ylab(y_title)+
    labs(fill=legend_title)+
    theme(legend.position = "bottom")+
    scale_x_discrete(labels = function(x) lapply(strwrap(x, width = 10, simplify = FALSE), paste, collapse="\n")))
}

print(create_graphs(recode_dwelling_room(house_cat_base_data)%>%
                      filter(!housing_unit_type %in% c("Hostel"))%>%
                      crosstab_3way(x=housing_unit_type,y=area_type,z=mpce_quintile,weight = Multiplier/100,format = "long")%>%
                      rename(x_axis="housing_unit_type",
                             y_axis="pct",
                             fill_col="area_type",
                             z_axis="mpce_quintile"),
                values_df = values_areatype,limits_df = limits_areatype,labels_df = labels_areatype,x_title = "Type of Houses",y_title = "Households(In %)",legend_title = "Roof Type:"))


