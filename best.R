best <- function(state, outcome) {
        ## internal variables
        valid_outcome <- c("heart attack", "heart failure", "pneumonia")
        names(valid_outcome) <- c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
        
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv")
        
        ## Check that state and outcome are valid
        if (nrow(data[data$State == state,]) == 0) {
                stop("invalid state")
        } else if (length(valid_outcome[valid_outcome == outcome]) == 0) {
                stop("invalid outcome")
        } else {
                data <- data[data$State == state,] # get data.frame from "state"
                order_data <- data[order(data[,"Hospital.Name"])] # sort data.frame by hospital name
        }
        
        ## rate
}