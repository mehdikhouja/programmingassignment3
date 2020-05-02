rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    df <-
        read.csv(
            "outcome-of-care-measures.csv",
            na.strings = "Not Available",
            stringsAsFactors = FALSE
        )
    ## Check that state and outcome are valid
    outcomes <-
        c(
            "heart attack" = 11,
            "heart failure" = 17,
            "pneumonia" = 23
        )
    if (!(state %in% df$State)) {
        stop("invalid state")
    }
    if (!(outcome %in% names(outcomes))) {
        stop("invalid outcome")
    }
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    my_data <- df[, c(2, 7, outcomes[outcome])]
    names(my_data) <- c("hospital", "state", "outcome")
    my_data2 <- na.omit(my_data)
    orderedData <-
        my_data2[order(my_data2$state, my_data2$outcome, my_data2$hospital), ]
    odata <- orderedData[which(orderedData$state == state), ]
    rk <- num
    if (num == "best") {
        rk <- 1
    } else
        if (num == "worst") {
            rk <- nrow(odata)
        }
    odata[rk, "hospital"]
}