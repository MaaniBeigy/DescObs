#' @title Coefficient of Variation (cv)
#' @name cv
#' @description Generic function for the coefficient of variation (cv)
cv <- function(
    x,
    na.rm = FALSE,
    digits = NULL,
    method = NULL,
    correction = TRUE,
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
    na.rm <- na.rm  # removes NAs if TRUE
    if (is.null(digits)) {
        digits = 4
    }
    digits <- digits  # digits required for rounding
    method <- tolower(method)
    method <- match.arg(
        arg = method,
        choices = c(
            "kelley", "mckay", "miller", "vangel", " mahmoudvand_hassani",
            "equal_tailed", "shortest_length", "normal_approximation",
            "norm","basic", "perc", "bca", "all"
        ),
        several.ok = TRUE
    )
    if ("kelley" %in% method) {
        if (!require(MBESS)) {
            warning(
        "package 'MBESS' required to calculate Kelley's confidence interval"
            )
        }
    }
    if (is.na(method)) {
        stop("invalid confidence interval method")
    }
    cv <- (
        sd(x, na.rm = na.rm)/mean(x, na.rm = na.rm)
    )
    if ("kelley" %in% method && correction == FALSE) {
        cv_corr <- cv * (
            (1 - (1/(4 * (length(x) - 1))) +
                 (1/length(x)) * cv^2) +
                (1/(2 * (length(x) - 1)^2))
        )
        ci <- MBESS::conf.limits.nct(
            ncp = sqrt(length(x))/cv_corr,
            df = length(x) - 1,
            conf.level = (1 - alpha)
        )
        est <- cv
        lower.tile <- unname(sqrt(length(x))/ci$Upper.Limit)
        upper.tile <- unname(sqrt(length(x))/ci$Lower.Limit)
    } else if (method == "kelley" && correction == TRUE) {
        cv_corr <- cv * (
            (1 - (1/(4 * (length(x) - 1))) +
                 (1/length(x)) * cv^2) +
                (1/(2 * (length(x) - 1)^2))
        )
        ci <- MBESS::conf.limits.nct(
            ncp = sqrt(length(x))/cv_corr,
            df = length(x) - 1,
            conf.level = (1 - alpha)
        )
        est <- cv_corr
        lower.tile <- unname(sqrt(length(x))/ci$Upper.Limit)
        upper.tile <- unname(sqrt(length(x))/ci$Lower.Limit)
    } else if (method == "mckay" && correction == FALSE) {
        cv
    }



    if (method == "kelley" && correction == FALSE) {
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
    } else if (method == "kelley" && correction == TRUE) {
        return(
            list(
                method = "Corrected cv with Kelley's 95% CI",
                statistics = data.frame(
                    est = round(est * 100, digits = digits),
                    lower = round(lower.tile * 100, digits = digits),
                    upper = round(upper.tile * 100, digits = digits)
                )
            )
        )
    }



}
