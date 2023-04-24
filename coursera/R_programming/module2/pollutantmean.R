library(tidyverse)

pollutantmean <- function(directory, pollutant, id = 1:332) {
  dir_content = dir(directory)
  value <- 0
  counts <- 0
  for (i in id){
    filename <- paste(directory, "/", dir_content[i], sep = "")
    data <- read.csv(file = filename)
    data <- select(data, all_of(pollutant))
    use <- !is.na(data)
    value <- value + sum(data[use])
    counts <- counts + length(data[use])
  }
  value / counts
}

x <- pollutantmean("specdata", "nitrate", 23)
print(x)
