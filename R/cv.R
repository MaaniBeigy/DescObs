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
            "kelley", "mckay", "miller", "vangel", "mahmoudvand_hassani",
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
    cv_corr <- cv * (
        (1 - (1/(4 * (length(x) - 1))) +
             (1/length(x)) * cv^2) +
            (1/(2 * (length(x) - 1)^2))
    )
    if ("kelley" %in% method && correction == FALSE) {
        ci <- MBESS::conf.limits.nct(
            ncp = sqrt(length(x))/cv_corr,
            df = length(x) - 1,
            conf.level = (1 - alpha)
        )
        est <- cv
        lower.tile <- unname(sqrt(length(x))/ci$Upper.Limit)
        upper.tile <- unname(sqrt(length(x))/ci$Lower.Limit)
    } else if (method == "kelley" && correction == TRUE) {
        ci <- MBESS::conf.limits.nct(
            ncp = sqrt(length(x))/cv_corr,
            df = length(x) - 1,
            conf.level = (1 - alpha)
        )
        est <- cv_corr
        lower.tile <- unname(sqrt(length(x))/ci$Upper.Limit)
        upper.tile <- unname(sqrt(length(x))/ci$Lower.Limit)
    } else if (method == "mckay" && correction == FALSE) {
        if (cv > 0.33) {
            warning("Confidence interval may be very approximate")
        }
        v <- length(x) - 1
        t1 <- qchisq(1 - alpha/2,v)/v
        t2 <- qchisq(alpha/2,v)/v
        u1 <- v*t1
        u2 <- v*t2
        est <- cv
        lower.tile <- cv/sqrt((u1/(v + 1) - 1 )*(cv^2) + u1/v)
        upper.tile <- cv/sqrt((u2/(v + 1) - 1)*(cv^2) + u2/v)
    } else if (method == "mckay" && correction == TRUE) {
        if (cv_corr > 0.33) {
            warning("Confidence interval may be very approximate")
        }
        v <- length(x) - 1
        t1 <- qchisq(1 - alpha/2,v)/v
        t2 <- qchisq(alpha/2,v)/v
        u1 <- v*t1
        u2 <- v*t2
        est <- cv_corr
        lower.tile <- cv_corr/sqrt((u1/(v + 1) - 1 )*(cv_corr^2) + u1/v)
        upper.tile <- cv_corr/sqrt((u2/(v + 1) - 1)*(cv_corr^2) + u2/v)
    } else if (method == "miller" && correction == FALSE) {
        v <- length(x) - 1
        z_alpha_over2 <- qnorm(1 - (alpha/2))
        u <- sqrt(
            (cv^2/v) * (0.5 + cv^2)
        )
        zu <- z_alpha_over2 * u
        est <- cv
        lower.tile <- cv - zu
        upper.tile <- cv + zu
    } else if (method == "miller" && correction == TRUE) {
        v <- length(x) - 1
        z_alpha_over2 <- qnorm(1 - (alpha/2))
        u <- sqrt(
            (cv_corr^2/v) * (0.5 + cv_corr^2)
        )
        zu <- z_alpha_over2 * u
        est <- cv_corr
        lower.tile <- cv_corr - zu
        upper.tile <- cv_corr + zu
    }  else if (method == "vangel" && correction == FALSE) {
        if (cv > 0.33) {
            warning("Confidence interval may be very approximate")
        }
        v <- length(x) - 1
        t1 <- qchisq(1 - alpha/2,v)/v
        t2 <- qchisq(alpha/2,v)/v
        u1 <- v*t1
        u2 <- v*t2
        est <- cv
        lower.tile <- cv/sqrt(((u1 + 1)/(v + 1) - 1 )*(cv^2) + u1/v)
        upper.tile <- cv/sqrt(((u2 + 1)/(v + 1) - 1)*(cv^2) + u2/v)
    } else if (method == "vangel" && correction == TRUE) {
        if (cv_corr > 0.33) {
            warning("Confidence interval may be very approximate")
        }
        v <- length(x) - 1
        t1 <- qchisq(1 - alpha/2,v)/v
        t2 <- qchisq(alpha/2,v)/v
        u1 <- v*t1
        u2 <- v*t2
        est <- cv_corr
        lower.tile <- cv_corr/sqrt(((u1 + 1)/(v + 1) - 1 )*(cv_corr^2) + u1/v)
        upper.tile <- cv_corr/sqrt(((u2 + 1)/(v + 1) - 1)*(cv_corr^2) + u2/v)
    } else if (method == "mahmoudvand_hassani" && correction == FALSE) {
        if (length(x) <= 340) {
            cn <- sqrt(2/(length(x) - 1)) * (
                (gamma(length(x)/2))/(gamma((length(x) - 1)/2))
                )
        } else {
            cn <- sqrt(2/(length(x) - 1)) * (
                (lgamma(length(x)/2))/(lgamma((length(x) - 1)/2))
            )
        }
        ul <- 2 - (cn + (qnorm((alpha/2)) * sqrt(1 - cn^2)))
        uu <- 2 - (cn - (qnorm((alpha/2)) * sqrt(1 - cn^2)))
        est <- cv
        lower.tile <- cv/ul
        upper.tile <- cv/uu
    } else if (method == "mahmoudvand_hassani" && correction == TRUE) {
        if (length(x) <= 340) {
            cn <- sqrt(2/(length(x) - 1)) * (
                (gamma(length(x)/2))/(gamma((length(x) - 1)/2))
            )
        } else {
            cn <- sqrt(2/(length(x) - 1)) * (
                (lgamma(length(x)/2))/(lgamma((length(x) - 1)/2))
            )
        }
        ul <- 2 - (cn + (qnorm((alpha/2)) * sqrt(1 - cn^2)))
        uu <- 2 - (cn - (qnorm((alpha/2)) * sqrt(1 - cn^2)))
        est <- cv_corr
        lower.tile <- cv_corr/ul
        upper.tile <- cv_corr/uu
    } else if (method == "normal_approximation" && correction == FALSE) {
        cn <- sqrt(1 - (1/(2 * length(x))))
        ul <- cn + (qnorm(1 - (alpha/2)) * sqrt(1 - cn^2))
        uu <- cn - (qnorm(1 - (alpha/2)) * sqrt(1 - cn^2))
        est <- cv
        lower.tile <- cv/ul
        upper.tile <- cv/uu
    } else if (method == "normal_approximation" && correction == TRUE) {
        cn <- sqrt(1 - (1/(2 * length(x))))
        ul <- cn + (qnorm(1 - (alpha/2)) * sqrt(1 - cn^2))
        uu <- cn - (qnorm(1 - (alpha/2)) * sqrt(1 - cn^2))
        est <- cv_corr
        lower.tile <- cv_corr/ul
        upper.tile <- cv_corr/uu
    }

    if (method == "kelley" && correction == FALSE) {
        return(
            list(
                method = "cv with Kelley 95% CI",
                statistics = data.frame(
                    est = round(est * 100, digits = digits),
                    lower = round(lower.tile * 100, digits = digits),
                    upper = round(upper.tile * 100, digits = digits),
                    row.names = c(" ")
                )
            )
        )
    } else if (method == "kelley" && correction == TRUE) {
        return(
            list(
                method = "Corrected cv with Kelley 95% CI",
                statistics = data.frame(
                    est = round(est * 100, digits = digits),
                    lower = round(lower.tile * 100, digits = digits),
                    upper = round(upper.tile * 100, digits = digits),
                    row.names = c(" ")
                )
            )
        )
    } else if (method == "mckay" && correction == FALSE) {
        return(
            list(
                method = "cv with McKay 95% CI",
                statistics = data.frame(
                    est = round(est * 100, digits = digits),
                    lower = round(lower.tile * 100, digits = digits),
                    upper = round(upper.tile * 100, digits = digits),
                    row.names = c(" ")
                )
            )
        )
    } else if (method == "mckay" && correction == TRUE) {
        return(
            list(
                method = "Corrected cv with McKay 95% CI",
                statistics = data.frame(
                    est = round(est * 100, digits = digits),
                    lower = round(lower.tile * 100, digits = digits),
                    upper = round(upper.tile * 100, digits = digits),
                    row.names = c(" ")
                )
            )
        )
    } else if (method == "miller" && correction == FALSE) {
        return(
            list(
                method = "cv with Miller 95% CI",
                statistics = data.frame(
                    est = round(est * 100, digits = digits),
                    lower = round(lower.tile * 100, digits = digits),
                    upper = round(upper.tile * 100, digits = digits),
                    row.names = c(" ")
                )
            )
        )
    } else if (method == "miller" && correction == TRUE) {
        return(
            list(
                method = "Corrected cv with Miller 95% CI",
                statistics = data.frame(
                    est = round(est * 100, digits = digits),
                    lower = round(lower.tile * 100, digits = digits),
                    upper = round(upper.tile * 100, digits = digits),
                    row.names = c(" ")
                )
            )
        )
    } else if (method == "vangel" && correction == FALSE) {
        return(
            list(
                method = "cv with Vangel 95% CI",
                statistics = data.frame(
                    est = round(est * 100, digits = digits),
                    lower = round(lower.tile * 100, digits = digits),
                    upper = round(upper.tile * 100, digits = digits),
                    row.names = c(" ")
                )
            )
        )
    } else if (method == "vangel" && correction == TRUE) {
        return(
            list(
                method = "Corrected cv with Vangel 95% CI",
                statistics = data.frame(
                    est = round(est * 100, digits = digits),
                    lower = round(lower.tile * 100, digits = digits),
                    upper = round(upper.tile * 100, digits = digits),
                    row.names = c(" ")
                )
            )
        )
    } else if (method == "mahmoudvand_hassani" && correction == FALSE) {
        return(
            list(
                method = "cv with Mahmoudvand-Hassani 95% CI",
                statistics = data.frame(
                    est = round(est * 100, digits = digits),
                    lower = round(lower.tile * 100, digits = digits),
                    upper = round(upper.tile * 100, digits = digits),
                    row.names = c(" ")
                )
            )
        )
    } else if (method == "mahmoudvand_hassani" && correction == TRUE) {
        return(
            list(
                method = "Corrected cv with Mahmoudvand-Hassani 95% CI",
                statistics = data.frame(
                    est = round(est * 100, digits = digits),
                    lower = round(lower.tile * 100, digits = digits),
                    upper = round(upper.tile * 100, digits = digits),
                    row.names = c(" ")
                )
            )
        )
    } else if (method == "normal_approximation" && correction == FALSE) {
        return(
            list(
                method = "cv with Normal Approximation 95% CI",
                statistics = data.frame(
                    est = round(est * 100, digits = digits),
                    lower = round(lower.tile * 100, digits = digits),
                    upper = round(upper.tile * 100, digits = digits),
                    row.names = c(" ")
                )
            )
        )
    } else if (method == "normal_approximation" && correction == TRUE) {
        return(
            list(
                method = "Corrected cv with Normal Approximation 95% CI",
                statistics = data.frame(
                    est = round(est * 100, digits = digits),
                    lower = round(lower.tile * 100, digits = digits),
                    upper = round(upper.tile * 100, digits = digits),
                    row.names = c(" ")
                )
            )
        )
    }

}
