#' Greedy heuristic knapsack
#'
#' @export greedy_knapsack
#' @importFrom utils Rprof
#' @description A greedy heuristic for solving knapsack problems.
#' @param x A data.frame containing with variables \code{w} and \code{v}
#' @param W A numeric value stating the maximum allowed weight of selected objects
#' @return A list containg the best value and the elements selected
#' @examples
#' n <- 2000
#' knapsack_objects <- data.frame(w=sample(1:4000, size = n, replace = TRUE), v=runif(n = n, 0, 10000))
#' greedy_knapsack(x = knapsack_objects[1:800,], W = 3500)
#' greedy_knapsack(x = knapsack_objects[1:1200,], W = 2000)
#' @references \url{https://en.wikipedia.org/wiki/Knapsack problem#Greedy approximation algorithm}

greedy_knapsack <- function(x, W) {
  # Start time and memory measurement
  # Rprof(assign("tmp", tempfile(), envir = .GlobalEnv), line.profiling = TRUE , memory.profiling = TRUE)
  # Check input parameters
  stopifnot(is.data.frame(x), is.numeric(W), W > 0, colnames(x) == c("w", "v"), min(x$w) > 0, min(x$v) > 0)

   # Initialize number of objects
  number_of_objects <- nrow(x)

  # Add the variable
  x$c <- x$v / x$w

  # Order the data frame based on the criterion in decreasing order
  x <- x[order(-x$c),]

  # Initialize parameters
  current_weight <- 0
  current_value <- 0
  best_combination <- numeric()

  # Add the items in the order of the data frame
  for (current_object in 1:number_of_objects) {

    # Check if there is space left for the object and add the object
    if (current_weight + x$w[current_object] <= W) {
      current_weight <- current_weight + x$w[current_object]
      current_value <- current_value + x$v[current_object]
      best_combination <- append(best_combination, as.numeric(row.names(x)[current_object]))
    }
    else {
      break
    }
  }

  # Stop time and memory measurement, get summary with: summaryRprof(tmp, lines = "show", memory = "both")
  # Rprof()
  # Return the best combination
  return(list(value = round(current_value, 0), elements = best_combination))
}
