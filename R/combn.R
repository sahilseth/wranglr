


#' expand_grid
#' @export
expand_grid <- function(x, y, remove_diag = TRUE){
    x1 <- seq_along(x)
    y1 <- seq_along(y)
    ex <- expand.grid(x1, y1)
    ex <- ex[ex[,2]>=ex[,1],]
    grd <- data.frame(x = x[ex[,1]], y = y[ex[,2]])
    
    # remove where both are the same
    if(remove_diag)
      grd <- grd[!grd$x == grd$y,]
    
    return(grd)
}
