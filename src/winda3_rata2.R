average_LC_water = function(all_transition_matrices_water){
  
  # Initialize an empty matrix to store the sum of all matrices
  sum_matrix <- matrix(0, nrow = nrow(all_transition_matrices_water[[1]]), ncol = ncol(all_transition_matrices_water[[1]]))
  
  # Loop over each matrix and add it to the sum_matrix
  for (i in 1:length(all_transition_matrices_water)) {
    sum_matrix <- sum_matrix + all_transition_matrices_water[[i]]
  }
  
  # Print the resulting sum matrix
  print(sum_matrix)
  
  average_matrix_water = sum_matrix/ length(all_transition_matrices_water)
  print(average_matrix_water)
  
}

 