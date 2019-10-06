brute_force_knapsack <- function (x, W) {
  stopifnot(is.data.frame(x), is.numeric(W), W > 0, colnames(x) == c("w", "v"), min(x$w) > 0, min(x$v) > 0)

}
