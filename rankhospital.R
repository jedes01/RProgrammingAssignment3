## Part 3 for Programming Assignment 3 for
## Coursera R Programming
## Creates a function that takes a state abbreviation, outcome, and rank and returns the hospital
## with that rank for that outcome in that state.

data(state)

#Reading, cleaning the 'outcome of care measures' csv
outcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
outcomes[,11] <- as.numeric(outcomes[,11])
outcomes[,17] <- as.numeric(outcomes[,17])
outcomes[,23] <- as.numeric(outcomes[,23])

#Defining some string variables to simplify the code for the "best" function
validoutcomes <- c("heart attack", "heart failure", "pneumonia")
columnStem <- "Hospital.30.Day.Death..Mortality..Rates.from."

#Creates a function to capitalize character input and separate it with a "."
simpleCap <- function(x){
	s <- strsplit(x, " ")[[1]]
	paste(toupper(substring(s,1,1)), substring(s,2), sep="", collapse=".")
}

#Takes a state (abbreviation), outcome ("heart attack", "pneumonia", or "heart failure"), and
#rank returns the hospital in that state with that rank (lowest 30-day mortality) for that outcome.
#Throws an error if the input is not well-formed as one of the state abbreviations or one of
#the three defined outcomes
rankhospital <- function(state, outcome, rank){
	if(!(state %in% c(state.abb, "GU", "VI", "PR", "DC"))) stop("invalid state")
	if(!(outcome %in% validoutcomes)) stop("invalid outcome")


	stateSubset <- outcomes[outcomes[,7]==state,]
	column <- paste(columnStem, simpleCap(outcome), sep="")
	stateSubset <- stateSubset[order(stateSubset[[column]],stateSubset[["Hospital.Name"]]),]
	
	if(rank=="best") {rank <- 1}
	if(rank=="worst") {rank <- nrow(stateSubset)-sum(is.na(stateSubset[[column]]))}
	if(rank > nrow(stateSubset)) NA
	else stateSubset[rank,2]	
}