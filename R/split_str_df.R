
#p_load(whisker)
#' split_str_df
#'
#' @param x a fastq file
#' @param format naming format for the file
#' @param strict_format_checking TRUE (error)/FALSE(warning)
#'
#' @export
#'
split_str_df <- function(x,
                               format = "{{samplename}}_{{index}}_L00{{lane}}_R{{read}}_{{num}}.fastq.gz",
                               # regex pattern for each piece
                               lst_patterns = list(
                                 samplename = "(.*)",
                                 index = "([ATGC]*|NoIndex)",
                                 lane = "([0-9]*)",
                                 read = "([0-9]*)", ## ideally would be 1 OR 2
                                 num = "([0-9]*)"),
                               strict_format_checking = FALSE,
                               verbose = T){


  ## --- replace and get final pattern !
  tmp = whisker_render(format, lst_patterns)
  fmt = tmp$out
  vars = tmp$vars
  if(verbose)
    message(fmt)

  ## get the parse matrix
  repl <- paste("\\",1:length(vars), sep="",collapse=",")
  mat <- gsub(fmt, repl, basename(x))

  # longer, but more robust
  mat = lapply(1:length(mat), function(i){
    out = strsplit(mat[i], ",")[[1]]
    add_vars = length(vars) - length(out)
    if(add_vars > 0){
      out = c(out, rep(NA, add_vars))
      warning("there was a issue parsing this str: ", basename(x[i]))
    }
    return(out)
  })
  warnings()

  mat <- do.call(rbind, mat)
  df <- data.frame(mat, x, stringsAsFactors = FALSE)
  colnames(df) <- c(vars, "str")


  return(df)
}

