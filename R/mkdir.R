#' mkdir
#'
#' check and create a dir
#'
#' @param x dirname or path
#'
#' @export
#'
mkdir <- function(x){
  if(!dir.exists(x))
    dir.create(x, recursive = T)
}
