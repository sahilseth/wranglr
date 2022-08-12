clean_names2 <- function (old_names, make_unique){
  new_names <- old_names %>% gsub("'", "", .) %>% gsub("\"",
                                                       "", .) %>% gsub("%", "percent", .) %>% make.names(.) %>%
    gsub("[.]+", "_", .) %>% gsub("[_]+", "_", .) %>% tolower(.) %>%
    gsub("_$", "", .)

  if(make_unique){
    dupe_count <- sapply(1:length(new_names), function(i) {
      sum(new_names[i] == new_names[1:i])
    })
    new_names[dupe_count > 1] <- paste(new_names[dupe_count >1], dupe_count[dupe_count > 1], sep = "_")
  }

  new_names
}
