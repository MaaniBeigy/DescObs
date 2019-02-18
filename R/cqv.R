#' Coefficient of Quartile Variation (CQV)
#'
#' Generic function for the coefficient of quartile variation (cqv)
#'
#' @param x An \code{R} object. Currently there are methods for numeric vectors
#' @param na.rm a logical value indicating whether \code{NA} values should be stripped before the computation proceeds.
#' @param digits integer indicating the number of decimal places to be used.
#' @param CI a scalar representing the type of confidence intervals required. The value should be any of the values "Bonett", "norm","basic", "perc", "bca" or "all".
#' @param R integer indicating the number of bootstrap replicates.
#' @details \code{\deqn{ cqv = (q3-q1)/(q3 + q1) , } } where \eqn{q3} and \eqn{q1} are third quartile (\emph{i.e.,} 75th percentile) and first quartile (\emph{i.e.,} 25th percentile), respectively.
#' @example ./examples/cqv.R
#' @references Bonett, DG., 2006, Confidence interval for a coefficient of quartile variation, Computational Statistics & Data Analysis, 50(11), 2953-7, DOI: \href{https://doi.org/10.1016/j.csda.2005.05.007}{https://doi.org/10.1016/j.csda.2005.05.007}
#' @references Altunkaynak, B., Gamgam, H., 2018, Bootstrap confidence intervals for the coefficient of quartile variation, Simulation and Computation, 1-9, DOI: \href{https://doi.org/10.1080/03610918.2018.1435800}{https://doi.org/10.1080/03610918.2018.1435800}
#' @export
cqv <- function(x, na.rm = FALSE, digits = NULL, CI = NULL, R = NULL, ...) {
    if (!is.numeric(x)) {
        stop("argument is not numeric: returning NA")
        return(NA_real_)
    }
    if (!is.vector(x)) {
        stop("x is not a vector")
        return(NA_real_)
    }
    # library(dplyr)
    # library(SciViews)
    # library(boot)
    na.rm = na.rm  # removes NAs if TRUE
    if (is.null(digits)) {
        digits = 4
    }
    digits = digits  # digits required for rounding
    CI = CI  # returns 95% confidence interval if TRUE
    if (is.null(R)) {
        R = 1000
    }
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
            probs = 0.25,  # first quartile (0.25 percentile)
            na.rm = na.rm
        )
    )
    if (q3 == 0) {  # to avoid NaNs when q3 and q1 are zero
        q3 <- max(x, na.rm = na.rm)
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
        alphastar <- 1 - sum(star[i], na.rm = na.rm)
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
    if (
        unname(quantile(x, probs = 0.75, na.rm = na.rm)) != 0
    ) {
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
            R = R
        )
    } else if (
        unname(quantile(x, probs = 0.75, na.rm = na.rm)) == 0
    ) {
        boot.cqv <- boot(
            x,
            function(x, i) {
                round(((
                    max(x[i], na.rm = na.rm) -
                        unname(quantile(x[i], probs = 0.25, na.rm = na.rm))
                ) / (
                    max(x[i], na.rm = na.rm) +
                        unname(quantile(x[i], probs = 0.25, na.rm = na.rm))
                )) * 100, digits = digits)
            },
            R = R
        )
    }

    if (is.null(CI)) {
        boot.cqv.ci <- NA
    } else if (CI == "Bonett") {
        boot.cqv.ci <- NA
    } else if (CI == "norm") {
        boot.norm.ci <- boot.ci(boot.cqv, conf = 0.95, type = "norm")
    } else if (CI == "basic") {
        boot.basic.ci <- boot.ci(boot.cqv, conf = 0.95, type = "basic")
    } else if (CI == "perc") {
        boot.perc.ci <- boot.ci(boot.cqv, conf = 0.95, type = "perc")
    } else if (CI == "bca") {
        boot.bca.ci <- boot.ci(boot.cqv, conf = 0.95, type = "bca")
    } else if (CI == "all") {
        boot.norm.ci <- boot.ci(boot.cqv, conf = 0.95, type = "norm")
        boot.basic.ci <- boot.ci(boot.cqv, conf = 0.95, type = "basic")
        boot.perc.ci <- boot.ci(boot.cqv, conf = 0.95, type = "perc")
        boot.bca.ci <- boot.ci(boot.cqv, conf = 0.95, type = "bca")
    }

    if (is.null(CI)) {
        cqv <- round(
            100 * ((q3 - q1)/(q3 + q1)), digits = digits
        )
    } else if (CI == "Bonett") {
        cqv <- round(
            100 * ((q3 - q1)/(q3 + q1)), digits = digits
        )
    } else if (CI == "norm") {
        cqv <- round(
            100 * ((q3 - q1)/(q3 + q1)), digits = digits
        )
    } else if (CI == "basic") {
        cqv <- round(
            100 * ((q3 - q1)/(q3 + q1)), digits = digits
        )
    } else if (CI == "perc") {
        cqv <- round(
            100 * ((q3 - q1)/(q3 + q1)), digits = digits
        )
    } else if (CI == "bca") {
        cqv <- round(
            100 * ((q3 - q1)/(q3 + q1)), digits = digits
        )
    } else if (CI == "all") {
        cqv <- round(
            100 * ((q3 - q1)/(q3 + q1)), digits = digits
        )
    }

    if (is.null(CI)) {
        lower <- NA
        upper <- NA
    } else if (CI == "Bonett") {
        lower <- round(lower.tile * 100, digits = digits)
        upper <- round(upper.tile * 100, digits = digits)
    } else if (CI == "norm") {
        lower <- round(boot.norm.ci$normal[2], digits = digits)
        upper <- round(boot.norm.ci$normal[3], digits = digits)
    } else if (CI == "basic") {
        lower <- round(boot.basic.ci$basic[4], digits = digits)
        upper <- round(boot.basic.ci$basic[5], digits = digits)
    } else if (CI == "perc") {
        lower <- round(boot.perc.ci$percent[4], digits = digits)
        upper <- round(boot.perc.ci$percent[5], digits = digits)
    } else if (CI == "bca") {
        lower <- round(boot.bca.ci$bca[4], digits = digits)
        upper <- round(boot.bca.ci$bca[5], digits = digits)
    }

    if (is.null(CI)) {
        return(
            list(
                method = "cqv = (q3-q1)/(q3+q1)",
                statistics = data.frame(
                    cqv = cqv
                )
            )
        )
    } else if (CI == "Bonett" && cqv != 100) {
        return(
            list(
                method = "cqv with Bonett's 95% CI",
                statistics = data.frame(
                    cqv = cqv,
                    lower = lower,
                    upper = upper
                    )
                )
            )
    } else if (CI == "norm" && cqv != 100) {
        return(
            list(
                method = "cqv with normal approximation 95% CI",
                statistics = data.frame(
                    cqv = cqv,
                    lower = lower,
                    upper = upper
                    )
                )
            )
    } else if (CI == "basic" && cqv != 100) {
        return(
            list(
                method = "cqv with basic bootstrap 95% CI",
                statistics = data.frame(
                    cqv = cqv,
                    lower = lower,
                    upper = upper
                    )
                )
            )
    } else if (CI == "perc" && cqv != 100) {
        return(
            list(
                method = "cqv with bootstrap percentile 95% CI",
                statistics = data.frame(
                    cqv = cqv,
                    lower = lower,
                    upper = upper
                    )
                )
            )
    } else if (CI == "bca" && cqv != 100) {
        return(
            list(
                method = "cqv with adjusted bootstrap percentile (BCa) 95% CI",
                statistics = data.frame(
                    cqv = cqv,
                    lower = lower,
                    upper = upper
                    )
                )
            )
    } else if (
        (
    CI == "norm" | CI == "Bonett" | CI == "basic" | CI == "perc" |
    CI == "bca" | CI == "all"
    ) && cqv == 100
        ) {
        warning("All values of t are equal to  100; Cannot calculate confidence intervals")
        return(
            list(
                method = "cqv with Bonett's 95% CI",
                statistics = data.frame(
                    cqv = cqv,
                    lower = round(lower.tile * 100, digits = digits),
                    upper = round(upper.tile * 100, digits = digits)
                )
            )
        )
    } else if (CI == "all" && cqv != 100) {
        return(
            list(
                method = "All Bootstrap methods",
                statistics = data.frame(
                    row.names = c(
                        "cqv with Bonett's 95% CI",
                        "cqv with normal approximation 95% CI",
                        "cqv with basic bootstrap 95% CI",
                        "cqv with bootstrap percentile 95% CI",
                        "cqv with adjusted bootstrap percentile (BCa) 95% CI"
                    ),
                    cqv = c(cqv, cqv, cqv, cqv, cqv),
                    lower = c(
                        round(lower.tile * 100, digits = digits),
                        round(boot.norm.ci$normal[2], digits = digits),
                        round(boot.basic.ci$basic[4], digits = digits),
                        round(boot.perc.ci$percent[4], digits = digits),
                        round(boot.bca.ci$bca[4], digits = digits)
                    ),
                    upper = c(
                        round(upper.tile * 100, digits = digits),
                        round(boot.norm.ci$normal[3], digits = digits),
                        round(boot.basic.ci$basic[5], digits = digits),
                        round(boot.perc.ci$percent[5], digits = digits),
                        round(boot.bca.ci$bca[5], digits = digits)
                        )
                    )
                )
            )
    } else {
        stop("method for confidence interval is not available")
        return(NA_real_)
    }
}
