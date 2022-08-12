#' expect_columns
#'
#' check if expected columns are in the df,
#' else fail with a descriptive message.
#'
#' @param trk a df
#' @param columns columns we epect to have in the df
#'
#' @export
expect_columns <- function(trk, columns){
  cols_trk = colnames(trk)
  cols_expect = columns
  col_missing = setdiff(cols_expect, cols_trk)
  if(length(col_missing) > 0 ){
    message("some columns are missing from the metadata (trk):\n",
            paste0(col_missing, collapse = "\n"))
    testit::assert("all required columns are present", {
      length(col_missing) == 0
    })
  }

}
