

#' eval_overlap
#'
#' @param x char vector, set 1
#' @param y char vector, set 2
#'
#' @export
#'
eval_overlap <- function(x, y){
  a = setdiff(x, y) %>% length()
  ab = intersect(x, y) %>% length()
  b = setdiff(y, x) %>% length()
  a_b = union(x, y) %>% length()
  return(c(a = a, ab = ab, b = b, a_b = a_b))
}


create_comb_mat <- function(df){
  out = make_comb_mat(df)
  out
  dim(out)
  class(out)

  df = as.data.frame(out) %>% t() %>% as.data.frame()
  df = cbind(df, code = names(comb_size(out)), size = comb_size(out))
}
