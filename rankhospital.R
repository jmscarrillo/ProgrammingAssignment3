## Coursera - Data Science - Universidad Johns Hopkins
## R Programming - Week 4
## Programming Assignment 3: instructions-hospital-quality
## José Mª Sebastián Carrillo

rankhospital <- function(state, outcome, num = "best") {
    
    # Author <- "José Mª Sebastián Carrillo"
    
    # Validate if the outcomed passed is valid (before the read, better efficiency!).
    validOutcomes = c("heart attack", "heart failure", "pneumonia")
    if (!is.element(outcome, validOutcomes)) {
        stop("invalid outcome")
    }
    
    # Metadata of input file, for performance efficiency.
    notAvailableTexts <- c("Not Available",
                           "No data are available from the hospital for this measure",
                           "NA")
    
    # Read the external data.
    careMeasures <- read.csv("outcome-of-care-measures.csv",
                             na.strings=notAvailableTexts)
    
    # Check if the state is valid
    if (!is.element(state, careMeasures$State)) {
        stop("invalid state")
    }
    
    # Determine the value of the external data to be processed
    targetColumnNumber <- switch(outcome,
                                 "heart attack"  = 11,  # Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack
                                 "heart failure" = 17,  # Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure
                                 "pneumonia"     = 23)  # Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia
    
    # Construct the data frame for work...
    targetHospitalData  <- careMeasures[c(2,7,targetColumnNumber)]
    # ...And give it the correct names
    colnames(targetHospitalData) <- c("hospital name", "state", outcome)
    
    # Filter the data frame by...
    # ...the state to be processed and...
    targetHospitalData <- targetHospitalData[targetHospitalData$state == state,]
    # ...the complete cases.
    targetHospitalData <- targetHospitalData[complete.cases(targetHospitalData), ]
    
    # Order the results to obtain the first (alphabetically) hospital name.
    targetHospitalData <- targetHospitalData[order(targetHospitalData[ ,3],
                                                   targetHospitalData$"hospital name"), ]
    
    # Decode the "best/worst" to first/last row.
    if(num == "best") num <- 1
    else if (num == "worst") num <- nrow(targetHospitalData)
    
    # Finally return the name of the selected hospital.
    as.character(targetHospitalData[, "hospital name"][num])
}
