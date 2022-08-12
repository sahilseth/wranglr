


## fetch value of x, if missing replace with y: default
#' @export
fetch_env <- function(x, y, verbose=TRUE){
    val <- ""
    val <- Sys.getenv(x)
    if(val == ""){
        val = y
    }
    message(x, " has been set with value: ", val)
    names(val) = x
    return(val)
}
