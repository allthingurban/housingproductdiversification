

print(base_style_graph(graph_kitchen_housetype_quintile,x_title = "Type of Houses",y_title = "Household(in %)",legend_title = "Kitchen type"))

print(base_style_graph(graph_mepcarea_housetype_quintile,x_title = "Type of Houses",y_title = "Median Per capita Area",legend_title = ""))

print(base_style_graph(graph_latrine_housetype_quintile,x_title = "Type of Houses",y_title = "Households(In %)",legend_title = "Latrine Access:"))

print(base_style_graph(graph_water_housetype_quintile,x_title = "Type of Houses",y_title = "Households(In %)",legend_title = "Water Access:"))

print(base_style_graph(graph_roof_housetype_qquintile,x_title = "Type of Houses",y_title = "Households(In %)",legend_title = "Roof Type:"))

print(base_style_graph(graph_wall_housetype_qquintile,x_title = "Type of Houses",y_title = "Households(In %)",legend_title = "Wall Type:"))

print(base_style_graph(graph_couple_separate_quintile, x_title = "Type of Houses", y_title = "Households(In %)",legend_title = "Couple with room:"))

print(base_style_graph(graph_dwellingtype_quintile, x_title = "Type of Houses", y_title = "Households(In %)",legend_title = "Type of Dwelling:"))

print(base_style_graph(graph_floor_quintile, x_title = "Type of Houses", y_title = "Households(In %)",legend_title = "Type of Building(By no. of Floor):"))

print(base_style_graph(graph_commute_quintile, x_title = "Type of Houses", y_title = "Households(In %)",legend_title = "Commute Distance:"))


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
              legend_title = "Area Type:")))

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
              legend_title = "Area Type:")))





print(base_style_graph(graph_area_quintile, x_title = "Type of Houses", y_title = "Households(In %)",legend_title = "Area Type:"))
