library(tidyverse)

complete <- function(directory, id = 1:332) {
  dir_content = dir(directory)
  lengths = integer(length = length(id))
  for (i in seq_along(id)){
    filename <- paste(directory, "/", dir_content[id[i]], sep = "")
    n_full_entries <- read.csv(file = filename) %>% drop_na() %>% nrow()
    lengths[i] <- n_full_entries
  }
  data.frame(id, lengths)
}

x <- complete("specdata", c(2, 4, 10, 8))
print(x)