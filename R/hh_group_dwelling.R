
house_cat_base_data=prepare_base_house_data()
head(merge(house_cat_base_data,select(add_occugrp_col(individual_member_details,2),1,9), by.x="ID",by.y="hh_id"))
