


# x = tab$path[1]

file_size <- function(x){
  message(".", appendLF = FALSE)
  sz = sprintf("du -s %s", x) %>%
    system(intern = TRUE) %>%
    strsplit(split = "\t") %>%
    unlist() %>%
    head(1)

}
