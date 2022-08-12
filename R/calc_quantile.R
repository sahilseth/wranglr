# http://stats.stackexchange.com/questions/50080/estimate-quantile-of-value-in-a-vector
calc_quantile <- function(x){
  # get a quantile generation function
  pc = ecdf(x)
  # get quantile, for each value
  y <- sapply(x, pc)
  y
}
