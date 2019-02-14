# this is for testing
x <- c(
    0.2, 0.5, 1.1, 1.4, 1.8, 2.3, 2.5, 2.7, 3.5, 4.4,
    4.6, 5.4, 5.4, 5.7, 5.8, 5.9, 6.0, 6.6, 7.1, 7.9
)
cqv <- function(x, na.rm = FALSE, digits = NULL, CI = NULL, ...) {  # coefficient of quartile variation
    if (!is.numeric(x)) {
        stop("argument is not numeric: returning NA")
        return(NA_real_)
    }
    if (!is.vector(x)) {
        stop("x is not a vector")
        return(NA_real_)
    }

    library(dplyr)
    library(SciViews)
    na.rm = na.rm  # removes NAs if TRUE
    if (is.null(digits)) {
        digits = 4
    }
    digits = digits  # digits required for rounding
    CI = CI  # returns 95% confidence interval if TRUE


    q3 <- unname(
        quantile(
            x,
            probs = 0.75,  # third quartile (0.75 percentile)
            na.rm = na.rm
        )
    )
    q1 <- unname(
        quantile(
            x,
            probs = 0.25,  # first quartile (0.75 percentile)
            na.rm = na.rm
        )
    )
    if (q3 == 0) {  # to avoid NaNs when q3 and q1 are zero
        q3 <- max(x)
    }
    a <- ceiling(
        (length(x)/4) - (1.96 * (((3 * length(x))/16)^(0.5)))
    )
    b <- round(
        (length(x)/4) + (1.96 * (((3 * length(x))/16)^(0.5))),
        digits = 0
    )

    c <- length(x) + 1 - b
    d <- length(x) + 1 - a
    Ya <- nth(x, a, order_by = x)
    Yb <- nth(x, b, order_by = x)
    Yc <- nth(x, c, order_by = x)
    Yd <- nth(x, d, order_by = x)
    star <- 0
    for (i in a:(b - 1)) {
        star[i] <- (
            (choose(length(x), i)) * (0.25^(i)) * (0.75^(length(x) - i))
            )
        alphastar <- 1 - sum(star[i])
    }
    zzz <- qnorm((1 - ((1 - alphastar)/2)))
    f1square <- (3 * (zzz)^2)/(4 * length(x) * ((Yb - Ya)^2))
    f3square <- (3 * (zzz)^2)/(4 * length(x) * ((Yd - Yc)^2))
    D <- q3 - q1
    S <- q3 + q1
    v <- (
        (1/(16 * length(x))) * (
        (((3/f1square) + (3/f3square) - (2/sqrt(f1square * f3square))) / D^2) +
        (((3/f1square) + (3/f3square) + (2/sqrt(f1square * f3square))) / S^2) -
        ((2 * ((3/f3square) - (3/f1square)))/(D*S))
       )
    )
    ccc <- length(x)/(length(x) - 1)
    upper.tile <- exp(((ln((D/S))*ccc)) + (zzz*(v^(0.5))))
    lower.tile <- exp(((ln((D/S))*ccc)) - (zzz*(v^(0.5))))
    cqv <- round(
        100*((q3 - q1)/(q3 + q1)), digits = digits
        )  # coefficient of quartile variation (CQV)
    CI95 <- paste0(
        round(lower.tile*100, digits = digits),
        "-",
        round(upper.tile*100, digits = digits)
        )
    if (CI == FALSE || is.null(CI)) {
        results = c(cqv)
        names = c("cqv")
        dim = c(1, 1)
    } else if (CI == TRUE) {
        results = c(cqv, CI95)
        names = c("cqv", "CI95%")
        dim = c(1, 2)
    }

    return(
        structure(
            .Data = results,
            "dim" = dim,
            "dimnames" = list(c(" "), names)
        )
    )
}
cqv(x = x, na.rm = TRUE, digits = 2, CI = FALSE)
cqv(x = x, na.rm = TRUE, digits = 2, CI = TRUE)
cqv(x = x, na.rm = TRUE, digits = 2)
cqv(x = x, na.rm = TRUE)
cqv(x = x)
