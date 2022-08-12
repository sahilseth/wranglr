#' convert mat to df
#'
#' rownames become a id column
#'
#' @param x matrix
#'
#' @export
to_df <- function(x, ...){
  data.frame(id = rownames(x), x, stringsAsFactors = F, ...)

}

#' convert df to mat
#'
#' @param x data.frame
#' @param rowname_var column ID to use as rowname, default 1
#'
#' @export
to_mat <- function(x, rowname_var = 1, to_numeric = T){
  if(class(x)[1] == "tbl_df")
    x = data.frame(x, stringsAsFactors = FALSE, check.names = F, check.rows = F)
  x2 = x[, -rowname_var, drop = F]
  rownames(x2) = x[, rowname_var]
  if(to_numeric)
    x2 = data.matrix(x2)
  x2
}



#' transform a named list
#'
#' @param x convert to list
#'
#' @export
to_df.list <- function(x){

  nms = names(x)
  ret = lapply(nms, function(xi){
    dplyr::mutate(x[[xi]], .nm = xi)
  }) %>% dplyr::bind_rows()
  ret
}




# END
