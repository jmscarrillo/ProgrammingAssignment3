## Coursera - Data Science - Universidad Johns Hopkins
## R Programming - Week 4
## Programming Assignment 3: instructions-hospital-quality
## José Mª Sebastián Carrillo

rankall <- function(outcome, num = "best") {
    
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
    
    # Determine the value of the external data to be processed
    targetColumnNumber <- switch(outcome,
                                 "heart attack"  = 11,  # Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack
                                 "heart failure" = 17,  # Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure
                                 "pneumonia"     = 23)  # Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia
    
    # Construct the data frame for work...
    targetHospitalData  <- careMeasures[c(2,7,targetColumnNumber)]
    # ...And give it the correct names
    colnames(targetHospitalData) <- c("hospital name", "state", outcome)
    
    # Filter the data frame by the complete cases.
    targetHospitalData <- targetHospitalData[complete.cases(targetHospitalData), ]
    
    # Order the results by state, outcome and hospital name.
    targetHospitalData <- targetHospitalData[order(targetHospitalData$state,
                                                   targetHospitalData[ ,3],
                                                   targetHospitalData$`hospital name`), ]
    
    # Decode the "best" to first row.
    if(num == "best") num <- 1
    
    # Aggregate by state, and return the N's value for every state
    # (for the "worst" hospital, we need to check the lenght of every aggregation group, inside the function)
    aggregateHospitalData <- aggregate(targetHospitalData,
                                       by=list(targetHospitalData$state),
                                       function(x) {
                                            if (num == "worst") {num <- length(x)}
                                            x[num]
                                        })
    
    # Constructs the final data frame...
    hospitalStatesData  <- aggregateHospitalData[c(2,1)]
    # ...and give it the correct names
    names(hospitalStatesData) <- c("hospital","state")
    
    # Finally return the processed data set.
    hospitalStatesData
}
