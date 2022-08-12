
#' A Persistent way to load packages. If they are not found in the library, this downloads them.
#' This ensures that say a knitr document would work across systems.
#'
#' @param x a list of R packages
#' @export
need_pkgs <- function(x, update_pkgs = FALSE){

  ins = installed.packages()[,1] #find out which packages are installed

  # check if the needed packages are installed
  need = x[which(is.na(match(x,ins)))]

  gits = grepl("/", need)

  # install the needed packages if they are not-installed
  if(length(need)>0){
    # install.packages(need)
    source("http://bioconductor.org/biocLite.R")
    biocLite(need)
  }
  message(paste(x, collapse = " "))

  if(update_pkgs)
    update.packages(x)

  # load the needed packages
  eval(parse(text = paste("library(", x, ")")))

}



if(FALSE){
  x = c("devtools", "covr", "sahilseth/params", "sahilseth/staticdocs", "sahilseth/pacakgedocs")

}
