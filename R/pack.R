cqv <- function(x, na.rm, digits) {  # coefficient of quartile variation
    na.rm = na.rm  # removes NAs if TRUE
    digits = digits  # digits required for rounding
    q3 <- unname(
        quantile(
            x, 
            probs = 0.75,  # third quartile  
            na.rm = na.rm
        )
    )
    q1 <- unname(
        quantile(
            x, 
            probs = 0.25,  # first quartile
            na.rm = na.rm
        )
    )
    if(q3 == 0) {  # to avoid NaNs
        q3 <- max(x)
    }
    cqv <- 100*(
        (q3 - q1)/(q3 + q1)
    )  # coefficient of quartile variation
    return(
        round(
            cqv,
            digits = digits
        )
    )
}