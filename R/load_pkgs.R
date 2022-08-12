

#' install packages which need to be installed
lib <- function(x){
    #find out which packages are installed
    ins = installed.packages()[,1]

    # check if the needed packages are installed
    get = x[which(is.na(match(x,ins)))]

    #install the needed packages if they are not-installed
    if(length(get)>0){install.packages(get)}
    eval(parse(text = paste("library(", x, ")")))#load the needed packages
}
