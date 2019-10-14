#' Dynamic programming knapsack
#'
#' @export knapsack_dynamic
#' @importFrom utils Rprof
#' @description A dynamic programming approach for solving knapsack problems.
#' @param x A data.frame containing with variables \code{w} and \code{v}
#' @param W A numeric value stating the maximum allowed weight of selected objects
#' @return A list containg the best value and the elements selected
#' @examples
#' n <- 2000
#' knapsack_objects <- data.frame(w=sample(1:4000, size = n, replace = TRUE), v=runif(n = n, 0, 10000))
#' knapsack_dynamic(x = knapsack_objects[1:8,], W = 3500)
#' knapsack_dynamic(x = knapsack_objects[1:12,], W = 3500)
#' knapsack_dynamic(x = knapsack_objects[1:8,], W = 2000)
#' knapsack_dynamic(x = knapsack_objects[1:12,], W = 2000)
#' @references \url{https://en.wikipedia.org/wiki/Knapsack problem#0.2F1 knapsack problem}

knapsack_dynamic <- function(x, W) {
  # Start time and memory measurement
  # Rprof(assign("tmp", tempfile(), envir = .GlobalEnv), line.profiling = TRUE , memory.profiling = TRUE)

  # Check input parameters
  stopifnot(is.data.frame(x), is.numeric(W), W > 0, colnames(x) == c("w", "v"), min(x$w) > 0, min(x$v) > 0)

  # Initialize number of objects and round column of weights
  number_of_objects <- nrow(x)
  x$w <- round(x$w, 0)

  # Initialize the matrix with dynamic values
  m <- matrix(0, nrow = number_of_objects + 1, ncol = W + 1)
  rownames(m) <- c(0:number_of_objects)
  colnames(m) <- c(0:W)

  # Fill matrix recursively
  for (i in 1:number_of_objects) {
    for (j in 0:W) {

      # Check if the current weight is larger than the current weight in the matrix
      object_weight <- x$w[i]
      if (object_weight > j) {
        m[i + 1, j + 1] <- m[i, j + 1]
      } else {
        m[i + 1, j + 1] <- max(m[i, j + 1], m[i, j + 1 - object_weight] + x$v[i])
      }
    }
  }

  # Initialize variables forbest combination of elements
  current_object <- number_of_objects
  current_weight <- W
  best_combination <- numeric()

  # While we are in the object and weight range find the next object to add
  while (current_object > 0 & current_weight > 0) {
    while (m[current_object + 1, current_weight + 1] == m[current_object, current_weight + 1]) {
      current_object <- current_object - 1

      # Check bounds
      if (current_object == 0) {
        break
      }
    }

    # Check bounds
    if (current_object < 1 | current_weight < 1) {
      break

      # Store the object in the best combination
    } else {
      best_combination <- append(best_combination, current_object)
      current_weight <- current_weight - x$w[current_object]
      current_object <- current_object - 1
    }
  }

  # Stop time and memory measurement, get summary with: summaryRprof(tmp, lines = "show", memory = "both")
  # Rprof()

  # Return the best combination
  return(list(value = round(m[as.character(number_of_objects), as.character(W)], 0),
              elements = rev(best_combination)))
}
