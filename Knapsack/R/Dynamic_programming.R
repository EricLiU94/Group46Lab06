knapsack_dynamic <- function(x, W) {
  # Start time and memory measurement
  Rprof(assign("tmp", tempfile(), envir = .GlobalEnv), line.profiling = TRUE , memory.profiling = TRUE)
  # Check input parameters
  stopifnot(is.data.frame(x), is.numeric(W), W > 0, colnames(x) == c("w", "v"), min(x$w) > 0, min(x$v) > 0)
  # Initialize number of objects
  number_of_objects <- nrow(x)
  # Initialize the matrix with dynamic values
  m <- matrix(0, nrow = number_of_objects + 1, ncol = W + 1)
  rownames(m) <- c(0:number_of_objects)
  colnames(m) <- c(0:W)
  
  # Fill matrix recursively
  for (i in 1:number_of_objects) {
    for (j in 0:W) {
      # Check if the current weight is larger than the current weight in the matrix
      if (x$w[i] > j) {
        m[as.character(i), as.character(j)] <- m[as.character(i - 1), as.character(j)]
      } else {
        m[as.character(i), as.character(j)] <- max(m[as.character(i - 1), as.character(j)], 
                                                   m[as.character(i - 1), as.character(j - x$w[i])] + x$v[i])
      }
    }
  }

  # Initialize variables forbest combination of elements
  current_object <- number_of_objects
  current_weight <- W
  best_combination <- numeric()
  
  # While we are in the object and weight range find the next object to add
  while (current_object > 0 & current_weight > 0) {
    while (m[as.character(current_object), as.character(current_weight)] == m[as.character(current_object - 1), as.character(current_weight)]) {
      current_object <- current_object - 1
      # Check bounds
      if (current_object == 0) {
        break
      }
    }
    
    # Check bounds
    if (current_object < 1 | current_weight < 1) {
      break
    } else {
      # Store the object in the best combination
      best_combination <- append(best_combination, current_object)
      current_weight <- current_weight - x$w[current_object]
      current_object <- current_object - 1
    }
  }

  # Stop time and memory measurement, get summary with: summaryRprof(tmp, lines = "show", memory = "both")
  Rprof()
  # Return the best combination
  return(list(value = round(m[as.character(number_of_objects), as.character(W)], 0),
              elements = rev(best_combination)))
}
