#' @title Runs multicore lapply with progress indicator and transformation to
#'
#' @description mclapply2
#'
#' @param X a list or vector, to loop over
#' @param FUN a function
#' @param ... other arguments
#' @export
#' @examples \dontrun{
#' mclapply2(X = X, FUN = FUN, ... = ...)
#' mclapply2(X = cmds, FUN = function(x, mc.cores = 5){system(x)})
#'}
mclapply2 <- function(X, FUN, ...) {
  #
  # Arguments (same as lapply):
  # X:   Vector
  # FUN: Function to apply to each value of X
  # Output: data.table stack of each mclapply return value
  #         Note FUN is transformed to a data.frame return if necessary
  # Progress bar code based on http://stackoverflow.com/a/10993589
  MC = length(X)
  local({ #evaluates in local environment only
    f <- fifo(tempfile(), open = "w+b", blocking = TRUE) # open fifo connection
    assign(x='f', value = f, envir = .GlobalEnv)
    pb <- txtProgressBar(min = 1, max = MC, style = 3)
    #if (inherits(fork(), "masterProcess")) { #progress tracker
    if (inherits(parallel:::mcfork(), "masterProcess")) { #progress tracker
      # Child
      progress <- 0.0
      #cat("here1"); print(f)
      while (progress < MC && !isIncomplete(f)){
        #cat("here2")
        msg <- readBin(f, "double")
        progress <- progress + as.numeric(msg)
        # Updating the progress bar.
        #cat(progress, "\n")
        setTxtProgressBar(pb, progress)
        #flush()
      }
      parallel:::mcexit()
    }
    .mcfunc <- function(...){
      writeBin(1, f)
      #writeBin(1, f)
      return(FUN(...))
    }
    result <- mclapply(X, .mcfunc, ...)
    assign(x = 'result', value = result, envir = .GlobalEnv)
    close(f)
  })
  cat("\n")
  return(result)
}

#' Function which prints a message using shell echo; useful for printing messages from inside mclapply when running in Rstudio
message_parallel <- function(...){
  system(sprintf('echo "\n%s\n"', paste0(..., collapse="")))
}


