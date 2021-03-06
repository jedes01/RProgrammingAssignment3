## Part 2, Programming Assignment 3, R Programming on Coursera
## Creates function best(), which takes a state and an outcome and returns 
## the hospital in the state with the best performance on that outcome. 

data(state)

## Reading, cleaning the 'outcome of care measures' csv
outcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
outcomes[,11] <- as.numeric(outcomes[,11])
outcomes[,17] <- as.numeric(outcomes[,17])
outcomes[,23] <- as.numeric(outcomes[,23])

## Defining some string variables to simplify the code for the "best" function
validoutcomes <- c("heart attack", "heart failure", "pneumonia")
columnStem <- "Hospital.30.Day.Death..Mortality..Rates.from."

## Creates a function to capitalize character input and separate it with a "."
simpleCap <- function(x){
	s <- strsplit(x, " ")[[1]]
	paste(toupper(substring(s,1,1)), substring(s,2), sep="", collapse=".")
}

## Takes a state (abbreviation) and outcome ("heart attack", "pneumonia", or "heart failure")
## and returns the hospital with the best performance (lowest 30-day mortality) for that outcome
## in the given state. Throws an error if the input is not well-formed as one of the state 
## abbreviations or one of the three defined outcomes

best <- function(state, outcome){
	if(!(state %in% c(state.abb, "GU", "VI", "PR", "DC"))) stop("invalid state")
	if(!(outcome %in% validoutcomes)) stop("invalid outcome")

	stateSubset <- outcomes[outcomes[,7]==state,]
	column <- paste(columnStem, simpleCap(outcome), sep="")
	stateSubset <- stateSubset[order(stateSubset[[column]]),]
	bestHospital <- stateSubset[1,2]
	bestHospital
}