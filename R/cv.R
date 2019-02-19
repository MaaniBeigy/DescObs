#' @title Coefficient of Variation (cv)
#' @name cv
#' @description Generic function for the coefficient of variation (cv)
cv <- function(
    x,
    na.rm = FALSE,
    digits = NULL,
    method = NULL,
    unbiased = TRUE,
    alpha = 0.05,
    R = NULL,
    ...
) {
    # library(MBESS)
    # require(dplyr)
    # require(SciViews)
    # require(boot)
    if (!is.numeric(x)) {
        stop("argument is not numeric: returning NA")
        return(NA_real_)
    }
    if (!is.vector(x)) {
        stop("x is not a vector")
        return(NA_real_)
    }
    na.rm = na.rm  # removes NAs if TRUE
    if (is.null(digits)) {
        digits = 4
    }
    digits = digits  # digits required for rounding
    method = method  # returns 95% confidence interval
    cv <- (
        sd(x, na.rm = na.rm)/mean(x, na.rm = na.rm)
    )
    if (method == "Kelley" && unbiased == FALSE) {
        res <- cv * (
            (1 - (1/(4 * (length(x) - 1))) +
                 (1/length(x)) * cv^2) +
                (1/(2 * (length(x) - 1)^2))
        )
        ci <- MBESS::conf.limits.nct(
            ncp = sqrt(length(x))/res,
            df = length(x) - 1,
            conf.level = (1 - alpha)
        )
        est <- cv
        lower.tile <- unname(sqrt(length(x))/ci$Upper.Limit)
        upper.tile <- unname(sqrt(length(x))/ci$Lower.Limit)
    } else if (method == "Kelley" && unbiased == TRUE) {
        res <- cv * (
            (1 - (1/(4 * (length(x) - 1))) +
                 (1/length(x)) * cv^2) +
                (1/(2 * (length(x) - 1)^2))
        )
        ci <- MBESS::conf.limits.nct(
            ncp = sqrt(length(x))/res,
            df = length(x) - 1,
            conf.level = (1 - alpha)
        )
        est <- res
        lower.tile <- unname(sqrt(length(x))/ci$Upper.Limit)
        upper.tile <- unname(sqrt(length(x))/ci$Lower.Limit)
    }
    if (method == "Kelley" && unbiased == FALSE) {
        return(
            list(
                method = "cv with Kelley's 95% CI",
                statistics = data.frame(
                    est = round(est * 100, digits = digits),
                    lower = round(lower.tile * 100, digits = digits),
                    upper = round(upper.tile * 100, digits = digits)
                )
            )
        )
    } else if (method == "Kelley" && unbiased == TRUE) {
        return(
            list(
                method = "unbiased cv with Kelley's 95% CI",
                statistics = data.frame(
                    est = round(est * 100, digits = digits),
                    lower = round(lower.tile * 100, digits = digits),
                    upper = round(upper.tile * 100, digits = digits)
                )
            )
        )
    }



}
