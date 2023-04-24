corr <- function(directory, threshold = 0) {
  dir_content = dir(directory)
  id = seq(332)
  correlations = numeric(length=length(id))
  for (i in id) {
    filename <- paste(directory, "/", dir_content[i], sep = "")
    data <- read.csv(file = filename) %>% drop_na()
    if (nrow(data) > threshold){
      correlations[i] <- cor(x = data$sulfate, y = data$nitrate)
    } else {
      correlations[i] <- NA
    }
  }
  use <- !is.na(correlations)
  correlations[use]
}

x <- corr("specdata", threshold = 400)
print(head(x))