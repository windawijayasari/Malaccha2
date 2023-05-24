selected_years = 2001:2019
water_transition_matrices2 = function(reclassified_table,
                                     all_jcr_table, 
                                     selected_years){
  
  selected_years = 2001:2019
  
  water_transition_matrices = lapply(selected_years, function(year){
    
    
    year_from = year
    year_to = year_from + 1
    
    from_table = reclassified_table %>% 
      filter(Year == year_from) %>%
      inner_join(all_jcr_table %>%
                   rename(SurfaceWater = value) %>%
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
    
  }) %>%
    `names<-`(selected_years)
  
  
  collected_transition_values = water_transition_matrices %>%
    lapply(function(transition_matrix){
      a = transition_matrix %>%
        gather("ToClass", "Prob", -FromClass)
    }) %>%
    bind_rows(.id = "FromYear") %>%
    mutate(ToClass = factor(ToClass, levels = c("Forest", "Agriculture", "Developed"))) %>%
    print()
  
  water_average_transition_matrix = collected_transition_values %>%
    group_by(FromClass, ToClass) %>%
    summarize(Prob = mean(Prob)) %>%
    spread("ToClass", "Prob") %>%
    print()
}
  
  