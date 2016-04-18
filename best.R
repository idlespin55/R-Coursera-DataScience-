#****************************************COURSE NOTES****************************************************
#Generating random numbers
#set.seed(1): setting the random number with set.seed ensures reproducibility
#rnorm(): generate random numbers  // e.g. rnorm(n, mean = 0, sd = 1)
#dnorm(): evaluate the Normal probability density  // e.g. dnorm(n, mean = 0, sd = 1, log = FALSE)
#pnorm(): evaluate the cummulative distribution function for a normal distribution
#rpois(): generate random Poisson variates with a given rate
#rbinom(): generate random binomial distribution
#sample(): sample(1:10, 4)  // select four random numbers from 1:10, replace option

#Profiling
#system.time() //
#rprof()  // keeps track of the function call stack and tabulates how much time is spent in each function

#********************************************SETUP********************************************************
#Setup
setwd("/Users/idlespin55/Documents/06_Programming/R/Coursera/Course2(R)_w4(simulation&profiling)")

#****************************************BEST FUNCTION****************************************************
#The function reads the outcome-of-care-measures.csv file and returns a character vector with the name
#of the hospital that has the best (i.e. lowest) 30-day mortality for the specified outcome in that state.
#The hospital name is the name provided in the Hospital.Name variable. The outcomes can be one of “heart
#attack”, “heart failure”, or “pneumonia”. Hospitals that do not have data on a particular outcome should
#be excluded from the set of hospitals when deciding the rankings.

# helper function for getting the hospital name
helper <- function(data, col_num, state) {
  state_subset <- data[data[, 7]==state, ]
  outcome_arr <- state_subset[, col_num]
  min <- min(outcome_arr, na.rm=T)
  min_index <- which(outcome_arr == min)
  hosp_name <- state_subset[min_index, 2]
  return(hosp_name)
}

best <- function(state, outcome) {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  
  # read the data file
  directory <- "outcome-of-care-measures.csv"
  data <- read.csv(directory, colClasses="character")
  # change data type from character to numeric
  data[, 11] <- as.numeric(data[, 11]) # heart attack
  data[, 17] <- as.numeric(data[, 17]) # heart failure
  data[, 23] <- as.numeric(data[, 23]) # pneumonia
  valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
  if (!state %in% data$State) {
    stop("invalid state")
  } else if(!outcome %in% valid_outcomes) {
    stop("invalid outcome")
  } else {
    if(outcome == "heart attack") {
      hosp_name <- helper(data, 11, state)
    } else if(outcome == "heart failure") {
      hosp_name <- helper(data, 17, state)
    } else {
      hosp_name <- helper(data, 23, state)
    }
    result <- hosp_name
    return(result)
  }
}

# tests
best("AK", "pneumonia")
best("TX", "heart failure")
best("MD", "heart attack")
best("MD", "pneumonia")
best("BB", "heart attack")