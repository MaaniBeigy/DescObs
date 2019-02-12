#' Coefficient of Quartile Variation (CQV)
#'
#' Generic function for the coefficient of quartile variation (cqv)
#'
#' @param x An {R} object. Currently there are methods for numeric vectors
#' @param na.rm a logical value indicating whether \code{NA} values should be stripped before the computation proceeds.
#' @param digits integer indicating the number of decimal places to be used.
#' @example ./examples/cqv.R
#' @references Bonett, DG., 2006, Confidence interval for a coefficient of quartile variation, Computational Statistics & Data Analysis, 50(11), 2953-7, DOI: \href{https://doi.org/10.1016/j.csda.2005.05.007}{https://doi.org/10.1016/j.csda.2005.05.007}
#' @export
cqv <- function(x, na.rm, digits) {
    if (!is.numeric(x)) {
        stop("argument is not numeric: returning NA")
        return(NA_real_)
    }
    if (!is.vector(x)) {
        stop("x is not a vector")
        return(NA_real_)
    }
    na.rm = na.rm  # removes NAs if TRUE
    digits = digits  # digits required for rounding
    a <- round(
        (length(x)/4) - (1.96 * (((3 * length(x))/16)^(0.5))),
        digits = 0
        )
    b <- round(
        (length(x)/4) + (1.96 * (((3 * length(x))/16)^(0.5))),
        digits = 0
        )
    c <- length(x) + 1 - b
    d <- length(x) + 1 - a

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
    cqv <- 100*(
        (q3 - q1)/(q3 + q1)
    )  # coefficient of quartile variation (CQV)
    return(
        round(  # round output
            cqv,
            digits = digits
        )
    )
}
