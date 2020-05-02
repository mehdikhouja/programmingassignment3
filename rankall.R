rankall <- function(outcome, num = "best") {
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
    if (!(outcome %in% names(outcomes))) {
        stop("invalid outcome")
    }
    ## For each state, find the hospital of the given rank
    my_data <- df[, c(2, 7, outcomes[outcome])]
    names(my_data) <- c("hospital", "state", "outcome")
    my_data2 <- na.omit(my_data)
    orderedData <-
        my_data2[order(my_data2$state, my_data2$outcome, my_data2$hospital), ]

    splitted_data <- split(orderedData, orderedData$state)

    hospitalNamefunction <- function(data, num){
        rk <- num
        if (num == "best") {
            rk <- 1
        } else
            if (num == "worst") {
                rk <- nrow(data)
            }
        data[rk,"hospital"]
    }    
    results_of_sapply <-
         sapply(splitted_data, hospitalNamefunction ,num)
     list_names <- names(results_of_sapply)
    # unlisted_values <- unlist(results_of_lapply)
     data.frame(hospital = results_of_sapply,
                state = list_names,
                row.names = list_names)
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
}