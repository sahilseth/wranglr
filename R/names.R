

#' Fix names of the data.frame
#' @param x vector of sample names to be fixed
#' @param char The character seperator desired
#' @export
fix_names <- function(x, char = "-"){
    ##x <- unlist(sapply(as.character(x), function(y){
    ## remove underscrores, spaces, .
    y <- gsub("_", char, as.character(x))
    y <- gsub(" ", char, as.character(y))
    y <- gsub("\\.", char, as.character(y))
                                        #}))
    return(y)
}
