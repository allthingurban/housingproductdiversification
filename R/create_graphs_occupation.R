facet_labels=as_labeller(c("Casual_Wage"="Casual Wage","No_Working"="Not Working","Regular_Salaried"="Regular Salaried","Self_Employed"="Self Employed"))

#Graph for showing proportion of households living in different areas according to the type of houses. Separate graphs will be built
#based on the occupational group of the household.

create_graphs(recode_dwelling_room(house_cat_base_data)%>%
                crosstab_3way(x=housing_unit_type,y=area_type,z=occupation_group,weight = Multiplier/100,format = "long")%>%
                rename(x_axis="housing_unit_type",
                       y_axis="pct",
                       fill_col="area_type",
                       z_axis="occupation_group"),
              values_df = values_areatype,
              limits_df = limits_areatype,
              labels_df = labels_areatype,
              x_title = "Type of Houses",
              y_title = "Households(In %)",
              legend_title = "Area Type:",
              facet_labels = facet_labels)

print(create_graphs(recode_dwelling_room(house_cat_base_data) %>%
                crosstab_3way(x=housing_unit_type,y=Kitchen_type,z=occupation_group,weight = Multiplier/100,format = "long")%>%
                rename(x_axis="housing_unit_type",
                       y_axis="pct",
                       fill_col="Kitchen_type",
                       z_axis="occupation_group"),
              values_df = values_kitchentype,
              limits_df = limits_kitchentype,
              labels_df = labels_kitchentype,
              x_title = "Type of Houses",
              y_title = "Households(In %)",
              legend_title = "Kitchen Type:",
              facet_labels = facet_labels))

print(create_graphs(recode_latrine_access(recode_dwelling_room(house_cat_base_data))%>%
                      crosstab_3way(x=housing_unit_type,y=latrine_access_recoded,z=occupation_group,weight = Multiplier/100,format = "long")%>%
                      rename(x_axis="housing_unit_type",
                             y_axis="pct",
                             fill_col="latrine_access_recoded",
                             z_axis="occupation_group"),
                    values_df = values_latrine,
                    limits_df = limits_latrine,
                    labels_df = labels_latrine,
                    x_title = "Type of Houses",
                    y_title = "Households(In %)",
                    legend_title = "Latrine Type:",
                    facet_labels = facet_labels))


print(create_graphs(recode_dw_access(recode_dwelling_room(house_cat_base_data))%>%
                      crosstab_3way(x=housing_unit_type,y=dw_access_recoded,z=occupation_group,weight = Multiplier/100,format = "long")%>%
                      rename(x_axis="housing_unit_type",
                             y_axis="pct",
                             fill_col="dw_access_recoded",
                             z_axis="occupation_group"),
                    values_df = values_water,
                    limits_df = limits_water,
                    labels_df = labels_water,
                    x_title = "Type of Houses",
                    y_title = "Households(In %)",
                    legend_title = "Access to Water:",
                    facet_labels = facet_labels))

print(create_graphs(recode_roof_type(recode_dwelling_room(house_cat_base_data))%>%
                      crosstab_3way(x=housing_unit_type,y=roof_type_recoded,z=occupation_group,weight = Multiplier/100,format = "long")%>%
                      rename(x_axis="housing_unit_type",
                             y_axis="pct",
                             fill_col="roof_type_recoded",
                             z_axis="occupation_group"),
                    values_df = values_roof,
                    limits_df = limits_roof,
                    labels_df = labels_roof,
                    x_title = "Type of Houses",
                    y_title = "Households(In %)",
                    legend_title = "Roof Type:",
                    facet_labels = facet_labels))

print(create_graphs(recode_wall_type(recode_dwelling_room(house_cat_base_data))%>%
                      crosstab_3way(x=housing_unit_type,y=wall_type_recoded,z=occupation_group,weight = Multiplier/100,format = "long")%>%
                      rename(x_axis="housing_unit_type",
                             y_axis="pct",
                             fill_col="wall_type_recoded",
                             z_axis="occupation_group"),
                    values_df = values_wall,
                    limits_df = limits_wall,
                    labels_df = labels_wall,
                    x_title = "Type of Houses",
                    y_title = "Households(In %)",
                    legend_title = "Wall Type:",
                    facet_labels = facet_labels))

print(create_graphs(recode_marrie_couple_room(recode_dwelling_room(house_cat_base_data))%>%
                      crosstab_3way(x=housing_unit_type,y=is_couple_noroom,z=occupation_group,weight = Multiplier/100,format = "long")%>%
                      rename(x_axis="housing_unit_type",
                             y_axis="pct",
                             fill_col="is_couple_noroom",
                             z_axis="occupation_group"),
                    values_df = values_maritalstatus,
                    limits_df = limits_maritalstatus,
                    labels_df = labels_maritalstatus,
                    x_title = "Type of Houses",
                    y_title = "Households(In %)",
                    legend_title = "Couple with room:",
                    facet_labels = facet_labels))

print(create_graphs(recode_dwelling_room(house_cat_base_data)%>%
                      crosstab_3way(x=housing_unit_type,y=dwelling_type,z=occupation_group,weight = Multiplier/100,format = "long")%>%
                      rename(x_axis="housing_unit_type",
                             y_axis="pct",
                             fill_col="dwelling_type",
                             z_axis="occupation_group"),
                    values_df = values_housetype,
                    limits_df = limits_housetype,
                    labels_df = labels_housetype,
                    x_title = "Type of Houses",
                    y_title = "Households(In %)",
                    legend_title = "Type of Dwelling:",
                    facet_labels = facet_labels))

print(create_graphs(recode_numoffloor(recode_dwelling_room(house_cat_base_data))%>%
                      crosstab_3way(x=housing_unit_type,y=Floors_cat,z=occupation_group,weight = Multiplier/100,format = "long")%>%
                      rename(x_axis="housing_unit_type",
                             y_axis="pct",
                             fill_col="Floors_cat",
                             z_axis="occupation_group"),
                    values_df = values_floor,
                    limits_df = limits_floor,
                    labels_df = labels_floor,
                    x_title = "Type of Houses",
                    y_title = "Households(In %)",
                    legend_title = "Floors:",
                    facet_labels = facet_labels))


