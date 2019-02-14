#' Coefficient of Quartile Variation (CQV)
#'
#' Generic function for the coefficient of quartile variation (cqv)
#'
#' @param x An \code{R} object. Currently there are methods for numeric vectors
#' @param na.rm a logical value indicating whether \code{NA} values should be stripped before the computation proceeds.
#' @param digits integer indicating the number of decimal places to be used.
#' @example ./examples/cqv.R
#' @references Bonett, DG., 2006, Confidence interval for a coefficient of quartile variation, Computational Statistics & Data Analysis, 50(11), 2953-7, DOI: \href{https://doi.org/10.1016/j.csda.2005.05.007}{https://doi.org/10.1016/j.csda.2005.05.007}
#' @export
cqv <- function(x, na.rm = FALSE, digits = NULL, CI = NULL, ...) {
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
    library(boot)
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
    upper.tile <- exp(((ln((D/S)) * ccc)) + (zzz * (v^(0.5))))
    lower.tile <- exp(((ln((D/S)) * ccc)) - (zzz * (v^(0.5))))
    boot.cqv <- boot(
        x,
        function(x, i) {
            round(((
                unname(quantile(x[i], probs = 0.75, na.rm = na.rm)) -
                    unname(quantile(x[i], probs = 0.25, na.rm = na.rm))
            ) / (
                unname(quantile(x[i], probs = 0.75, na.rm = na.rm)) +
                    unname(quantile(x[i], probs = 0.25, na.rm = na.rm))
            )) * 100, digits = digits)
        },
        R = 1000
    )
    boot.cqv.ci <- boot.ci(boot.cqv, conf = 0.95, type = "bca")
    if (is.null(CI)) {
        cqv <- round(
            100 * ((q3 - q1)/(q3 + q1)), digits = digits
        )
    } else if (CI == "Bonett") {
        cqv <- round(
            100 * ((q3 - q1)/(q3 + q1)), digits = digits
        )
    } else if (CI == "bca") {
        cqv <- (
            boot.cqv.ci$bca[2]
        )
    } else {
        cqv <- round(
            100 * ((q3 - q1)/(q3 + q1)), digits = digits
        )
    }
    if (is.null(CI)) {
        CI95 <- NA
    } else if (CI == "Bonett") {
        CI95 <- paste0(
            round(lower.tile * 100, digits = digits),
            "-",
            round(upper.tile * 100, digits = digits)
        )
    } else if (CI == "bca") {
        CI95 <- paste0(
            round(boot.cqv.ci$bca[4], digits = digits),
            "-",
            round(boot.cqv.ci$bca[5], digits = digits)
        )
    } else {
        CI95 <- NA
    }
    if (is.null(CI)) {
        results <- c(cqv)
        names <- c("cqv")
        dim <- c(1, 1)
    } else if (CI == "Bonett") {
        results <- c(cqv, CI95)
        names <- c("cqv", "Bonett's CI95%")
        dim <- c(1, 2)
    } else if (CI == "bca") {
        results <- c(cqv, CI95)
        names <- c("cqv", "adjusted bootstrap percentile (BCa)")
        dim <- c(1, 2)
    } else {
        stop("method for confidence interval is not available")
        return(NA_real_)
    }

    return(
        structure(
            .Data = results,
            "dim" = dim,
            "dimnames" = list(c(" "), names)
        )
    )
}
