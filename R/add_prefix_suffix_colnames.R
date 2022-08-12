


#' add_prefix_suffix_column
#'
#' @param df
#' @param prefix
#' @param suffix
#' @param skip_cols
#'
#' @export
add_prefix_suffix_column <- function(df, prefix = "", suffix = "", skip_cols = "path_patient_id"){
  colnms = colnames(df)

  colnms_new = paste0(prefix, colnms, suffix)

  colnms_new = ifelse(colnms %in% skip_cols, colnms, colnms_new)

  colnames(df) = colnms_new

  df
}
