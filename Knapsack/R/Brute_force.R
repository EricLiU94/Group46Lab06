#' Brute force knapsack
#'
#' @export brute_force_knapsack
#' @importFrom utils Rprof
#' @description A brute force approach for solving knapsack problems. The function will not work for more than 32 items.
#' @param x A data.frame containing with variables \code{w} and \code{v}
#' @param W A numeric value stating the maximum allowed weight of selected objects
#' @return A list containg the best value and the elements selected
#' @examples
#' n <- 2000
#' knapsack_objects <- data.frame(w=sample(1:4000, size = n, replace = TRUE), v=runif(n = n, 0, 10000))
#' brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500)
#' brute_force_knapsack(x = knapsack_objects[1:12,], W = 3500)
#' brute_force_knapsack(x = knapsack_objects[1:8,], W = 2000)
#' brute_force_knapsack(x = knapsack_objects[1:12,], W = 2000)
#' @references \url{https://en.wikipedia.org/wiki/Knapsack_problem}

brute_force_knapsack <- function (x, W) {
  # Start time and memory measurement
  # Rprof(assign("tmp", tempfile(), envir = .GlobalEnv), line.profiling = TRUE , memory.profiling = TRUE)
  # Check input parameters
  stopifnot(is.data.frame(x), is.numeric(W), W > 0, colnames(x) == c("w", "v"), min(x$w) < W, min(x$v) > 0, nrow(x) <= 32)
  
  # Initialize best value and number of objects
  best_value <- 0
  number_of_objects <- nrow(x)
  
  # Run for 2^(n-1) configurations, which corresponds to all binary combinations
  for (i in 1:(2^number_of_objects - 1)) {
    # Calculate the current weight and value based on the binary combination
    current_weight <- sum(as.numeric(intToBits(i))[1:number_of_objects] * x$w)
    current_value <- sum(as.numeric(intToBits(i))[1:number_of_objects] * x$v)
   
    # Check if the weight is allowed and the value improved the best value
    if (current_weight <= W & current_value > best_value) {
      # Set the combination as the best combination
      best_value <- current_value
      best_combination <- which(as.vector(as.numeric(intToBits(i))) %in% 1)
    }
  }
  
  # Stop time and memory measurement, get summary with: summaryRprof(tmp, lines = "show", memory = "both")
  # Rprof()
  # Return the best combination
  return(list(value = round(best_value, 0), elements = best_combination))
}
