bind_rows2 <- function(..., nms){
  lst <- rlang::list2(...)
  testthat::expect_length(lst, length(nms))

  lapply(seq_along(lst), function(i){
    lst[[i]] %>% mutate(.lst_name = nms[i]) %>%
      dplyr::select(.lst_name, everything())
  }) %>% bind_rows()

}
