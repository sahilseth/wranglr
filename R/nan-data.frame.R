
## http://stackoverflow.com/questions/18142117/how-to-replace-nan-value-with-zero-in-a-huge-data-frame
## this comes from: http://stackoverflow.com/users/474349/hong-ooi

#' Checks for NaNs in a data.frame
#'
#' @param x a data.frame
#'
#' @export
#'
is.nan.data.frame <- function(x){
	do.call(cbind, lapply(x, is.nan))
}

