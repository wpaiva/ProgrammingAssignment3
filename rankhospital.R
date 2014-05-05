rankhospital <- function(state, outcome, num = "best") {
        options(warn = -1) # disable warnings
        
        # we will use this...
        source("best.R")
        
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
        
        ## Return hospital name in that state with the given rank 30-day death rate
        if (outcome == "heart attack") {
                if (num == "worst"){
                        return (searchWorst(11, state, data))
                }
                else {
                        data[, 11] <- as.numeric(data[, 11])
                        newData <- subset(data,data[["State"]] == state)
                        lengthNewData <- length(newData[["State"]])
                        
                        newData<-newData[order(newData[[11]],newData[["Hospital.Name"]]),]
                        newData<-subset(newData,subset=(!is.na(newData[[11]])))
                }
        }
        else if (outcome == "heart failure") {
                if(num == "worst"){
                        return(searcWorst(17, state, data))
                }
                else {
                        data[, 17] <- as.numeric(data[, 17])
                        newData <- subset(data, data[["State"]] == state)
                        lengthNewData <- length(newData[["State"]])
                        
                        newData <- newData[order(newData[[17]], newData[["Hospital.Name"]]), ]
                        newData <- subset(newData, subset = (!is.na(newData[[17]])))
                }
        }
        else if (outcome == "pneumonia") {
                if(num == "worst"){
                        return (searchWorst(23, state, data))
                }
                else {
                        data[, 23] <- as.numeric(data[, 23])
                        newData <- subset(data, data[["State"]] == state)
                        lengthNewData <- length(newData[["State"]])
                        
                        newData <- newData[order(newData[[23]], newData[["Hospital.Name"]]),]
                        newData <- subset(newData,subset = (!is.na(newData[[23]])))
                }
        }
        
        # if num is larger than the number of hospitals in state
        if (is.numeric(num) && (num > lengthNewData)) {
                return(NA)
        }
        
        # if num is "best" than return best
        if (!is.numeric(num) && (num == "best")) {
                return(best(state,outcome)) # best function defined at best.R
        }
        
        if (is.numeric(num)) {
                return(newData[num, "Hospital.Name"])
        }
}

# helper function that finds the worst hospiral for the given outcome number
searchWorst <- function(outcome, state, data) {
        data[, outcome] <- as.numeric(data[, outcome])
        data <- subset(data, data$State == state)
        valMax <- max(data[[outcome]], na.rm = TRUE)
        data <- subset(data,data[[outcome]] == valMax)
        data <- data[order(data[["Hospital.Name"]]), ]
        return(data[1, "Hospital.Name"])
}