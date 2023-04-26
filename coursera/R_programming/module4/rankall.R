get_rank_hospital_in_state <- function(df, outcome_col = NA, num = "best"){
  # sort dataframe by mortality and hospital name
  order_colnames <- c(outcome_col, "Hospital.Name", "State")
  order_ndxs <- order(df[, order_colnames[1]], df[, order_colnames[2]]) 
  df <- df[order_ndxs,]
  
  state <- c(df[1,]$State)
  # if the parsed num is more than the number of entries, return NA
  if(num == "best"){
    num <- 1
  }
  else if(num == "worst"){
    num <- nrow(df)
  }
  if(num > nrow(df)){
    hospital <- NA
  }
  else {
    hospital <- c(df[num, ]$Hospital.Name)
  }
  best_hospital <- data.frame(state, hospital)
  best_hospital
}

rankall <- function(outcome, num = "best"){
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
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
  # extract the relevant columns
  keep_colnames <- c(outcome_clean, "Hospital.Name", "State")
  data <- data[, keep_colnames]
  # keep entries with outcome data
  data <- drop_na(data)
  
  outcomes_by_state <- split(data, data[, "State"])
  result <- lapply(outcomes_by_state, get_rank_hospital_in_state, num = num, outcome_col = outcome_clean)
  result <- bind_rows(result)
  rownames(result) <- result$state
  result
}

# print(head(rankall("heart attack", 20), 10))
# print(tail(rankall("pneumonia", "worst"), 3))
print(tail(rankall("heart failure"), 10))