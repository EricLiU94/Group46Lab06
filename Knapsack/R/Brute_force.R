brute_force_knapsack <- function (x, W) {
  stopifnot(is.data.frame(x), is.numeric(W), W > 0, colnames(x) == c("w", "v"), min(x$w) > 0, min(x$v) > 0)
  best_value <- 0
  for (i in 1:(nrow(x) - 1)) {
    for (j in (i + 1):nrow(x)) {
      current_weight <- x$w[i] + x$w[j]
      current_value <- x$v[i] + x$v[j]
      if (current_weight <= W & current_value > best_value) {
        best_value <- current_value
        best_combination <- c(i, j)
      }
    }
  }
  return(list(value = best_value, elements = best_combination))
}
