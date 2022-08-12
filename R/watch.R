
#' watch and knit files, and check every 30 seconds
#' @export
#' @import rmarkdown
watch <- function(x, sleep = 30, func = render, ...){
    stime <- file.info(x)$mtime # modification time of the file when initiating
    tmp <- try(func(x, ...))
    while(TRUE) {
        ctime <- file.info(x)$mtime
        if(stime < ctime) {
            tmp <- try(func(x, ...))
            if(class == "try-error"){
                message("Ahh... Error. I will wait and re-run this")
                Sys.sleep(sleep)
                next
            }
        }else{
            message("ZZZzzzzzz......")
        }
        Sys.sleep(sleep)
    }
}
