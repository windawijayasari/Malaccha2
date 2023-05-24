
#selected_years = 2001:2019
Calculate_transition_matrix_water = function(reclassified_table,
                                         all_jcr_table,
                                         year_from, year_to){
  #year_from = year
  #year_to = year_from + 1
  
  the_two_years = reclassified_table %>%
    filter(Year %in% c(year_from, year_to)) %>%
    dplyr::select(Year, X, Y, LC_class)
  
  from_table = reclassified_table %>% 
    filter(Year == year_from) %>%
    inner_join(all_jcr_table %>%
                 rename( SurfaceWater = value) %>%
                 mutate(SurfaceWater = if_else(SurfaceWater > 0.7315335, "High", "Low"))) %>%
    dplyr::select(X, Y, LC_class, SurfaceWater) %>%
    unite("FromClass", c(LC_class, SurfaceWater)) %>%
    
    mutate(FromClass = factor(FromClass,
                              levels = c("Forest_High", "Forest_Low",
                                         "Agriculture_High", "Agriculture_Low",
                                         "Developed_High", "Developed_Low"))) %>%
    print()
  
  to_table = reclassified_table %>% 
    filter(Year == year_to) %>%
    dplyr::select(X, Y, LC_class) %>%
    rename(ToClass = LC_class) %>%
    print()
  
  from_to_table = from_table %>% inner_join(to_table) %>%
    print()
  
  summary_table = from_to_table %>%
    group_by(FromClass, ToClass) %>%
    summarize(count = n()) %>%
    print()
  
  
  pivot_table = summary_table %>%
    pivot_wider(names_from = ToClass, values_from = count, values_fill = 0) %>%
    print() 
  
  summary_to_table = from_to_table %>%
    group_by(FromClass) %>%
    summarize(totalcount = n()) %>%
    print()
  
  # summary_to_table_2 = from_to_table %>%
  #   group_by(ToClass) %>%
  #   summarize(totalcount = n()) %>%
  #   print()
  
  markov_transition_matrix = summary_table %>% 
    inner_join(summary_to_table) %>%
    mutate(prob = count / totalcount) %>%
    dplyr::select(FromClass, ToClass,  prob) %>%
    pivot_wider(names_from = ToClass, values_from = prob, values_fill = 0) %>%
    print()
  
  changes_two_years_water = markov_transition_matrix %>%
    column_to_rownames("FromClass")
  
  
} 
  


