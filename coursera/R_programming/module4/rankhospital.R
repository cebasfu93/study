rankhospital <- function(state, outcome, num = "best"){
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
  
  # make outcome column numeric
  data[, outcome_clean] <- as.numeric(data[, outcome_clean])
  # keep entries with outcome data
  data <- drop_na(data)
  
  # if the parsed num is more than the number of entries, return NA
  if(num == "best"){
    num <- 1
  }
  else if(num == "worst"){
    num <- nrow(data)
  }
  
  if(num > nrow(data)){
    return(NA)
  }

  
  # sort dataframe by mortality and hospital name
  order_colnames <- c(outcome_clean, "Hospital.Name")
  order_ndxs <- order(data[, order_colnames[1]], data[, order_colnames[2]]) 
  data <- data[order_ndxs, order_colnames]
  data$Hospital.Name[num]
}

x <- rankhospital(state = "TX", outcome = "heart failure", num = 4)
print(x)