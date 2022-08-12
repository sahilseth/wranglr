#' xml_to_df
#'
#' @param doc
#' @param top
#'
#' @export
#'
#' @details
#' # county_df <- xtrct_df(doc, "COUNTY")
#' https://rpubs.com/hrbrmstr/xml2power
#' <AOC_LOCATION_STATS>
# - <COUNTY_BLOCK>
#   - <COUNTY>
#   <COUNTY_NAME>Kern </COUNTY_NAME>
#   <NBR_INCIDENTS>1</NBR_INCIDENTS>
#   <NBR_CUST_AFFECTED>54</NBR_CUST_AFFECTED>
#   <CENTROID_X>-118.729077873263</CENTROID_X>
#   <CENTROID_Y>35.34357933177560</CENTROID_Y>
#   </COUNTY>
xml_to_df_type1 <- function(doc, top) {
  xml_find_first(doc, sprintf(".//%s", top)) %>%
    xml_children() %>%
    xml_name() %>%
    map(~{
      xtrct(doc, sprintf(".//%s/%s", top, .x)) %>%
        list() %>%
        set_names(tolower(.x))
    }) %>%
    flatten_df() %>%
    readr::type_convert()
}

# different types might be labelled as NULL/NA
replace_to_na <- function(x){
  gsub("\"NONE\"", "", x)
}

xml_to_df_type2 <- function(x, top = ".//GENESET", cores = 16) {
  # get all GENESET
  x_gs = xml_find_all(x, top)
  x_gs[[1]]
  p_load(furrr, tictoc)
  plan(multicore, workers = cores)

  message("flattening ", length(x_gs), " elements into a df")

  tic()
  df_attr = future_map(x_gs, ~{
    # this way each element can have a different set of attributes
    # get all attr names:
    attr_names = xml_attrs(.x) %>% names()

    # extract each of them, and store in a df:
    df_attr = lapply(attr_names, function(a){
      val = xml_attr(.x, a)
      # message(val)
      val %<>% replace_to_na()
      attr(val, "names") = a
      val
    }) %>% flatten_df()
    df_attr
  }, .progress = TRUE) %>% bind_rows() %>% type_convert()
  toc()

  return(df_attr)


}
