best <- function(state, outcome) {
        options(warn = -1) # disable warnings

        ## internal variables
        valid_outcome <- list(c("heart attack", "heart failure", "pneumonia"),c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"))
        
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## Check that state and outcome are valid
        if (nrow(data[data$State == state,]) == 0) {
                stop("invalid state")
        } else if (length(valid_outcome[[1]][valid_outcome[[1]] == outcome]) == 0) {
                stop("invalid outcome")
        } 
        
        ## rate
        if (outcome == "heart attack") {
                searchBest(11, state, data)
        } else if (outcome == "heart failure") {
                searchBest(17, state, data)
        } else if (outcome == "pneumonia") {
                searchBest(23, state, data)
        }
}

searchBest <- function(outcomeNumber, state, data) {
        data[, outcomeNumber] <- as.numeric(data[, outcomeNumber])
        data <- subset(data, data$State==state) 
        valMin <- min(data[[outcomeNumber]], na.rm = TRUE)
        data <- subset(data, data[[outcomeNumber]] == valMin)
        data <- data[order(data[["Hospital.Name"]]), ]
        return(data[1, "Hospital.Name"])
}        
