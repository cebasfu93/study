library(tidyverse)

best <- function(state, outcome){
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  # get the unique states
  valid_states = unique(data$State)
  # check if the provided state appears in the dataset
  if(!(state %in% valid_states)){
    stop("invalid state")
  }
  # keep only data from the parsed state
  data <- filter(data, State == state)
  
  # check if the outcome given is supported
  valid_outcomes = c("heart attack", "heart failure", "pneumonia")
  if(!(outcome %in% valid_outcomes)){
    stop("invalid outcome")
  }
  
  # build the name of the column from the base
  outcome_base = "Hospital.30.Day.Death..Mortality..Rates.from"
  
  outcome_clean <- str_split(outcome, pattern = " ", simplify = TRUE)  # convert the parsed outcome into a list of strings
  outcome_clean <- str_to_title(outcome_clean)  # capitalize first letters
  outcome_clean <- c(outcome_base, outcome_clean)  # put into the vector the base name and the parsed outcome
  outcome_clean <- paste(outcome_clean, collapse = ".")  # concatenate with a period
  
  # extract the data of interest
  mortality_data <- as.numeric(data[[outcome_clean]])
  # compute the best mortality
  min_mortality <- min(mortality_data, na.rm = TRUE)
  # keep the hospitals with best mortality
  mask <- mortality_data == min_mortality 
  mask[is.na(mask)] <- FALSE
  # extract the hospital name only
  best_hospitals <- data$Hospital.Name[mask]

  # if there is a tie, sort alphabetically and return the first
  if (length(best_hospitals) == 1){
    return(best_hospitals)
  }
  else {
    sort(best_hospitals)[1]
  }
}

x <- best("TX", "heart attack")
print(x)