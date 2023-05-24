Get_matrix_by_year = function(reclassified_table, all_jcr_table, year_selected){
  # Filter reclassified_table by year_selected
  selected_table = reclassified_table %>% 
    filter(Year == year_selected) %>%
    dplyr::select(X, Y, LC_class)
  
  # Unite X and Y columns into a single "Cell" column
  spread_table = selected_table %>%
    tidyr::unite("Cell", c(X, Y)) %>%
    spread(LC_class, -Cell)
  
  # Convert non-NA values to 1 in binary matrix
  only_vals = spread_table %>%
    dplyr::select(-Cell) %>%
    mutate_all(.funs = function(x){ as.integer(!is.na(x)) })
  
  # Unite X and Y columns into a single "Cell" column for JCR
  jcr_unite_XY = all_jcr_table %>%
    tidyr::unite("Cell", c(X, Y))
  
  # Add JCR value to the binary matrix
  combined_matrix = inner_join(matrix_by_year, all_jcr_table, by = c("X" = "X", "Y" = "Y"))
  
  
  # Set row names to Cell values and remove Cell column
  as.matrix(output %>% dplyr::select(-Cell)) %>% `rownames<-`(output$Cell)
}
