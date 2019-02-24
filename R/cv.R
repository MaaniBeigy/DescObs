#' @title Coefficient of Variation (cv)
#' @name cv
#' @description Generic function for the coefficient of variation (cv)
#' @param x An \code{R} object. Currently there are methods for numeric vectors
#' @param na.rm a logical value indicating whether \code{NA} values should be
#'              stripped before the computation proceeds.
#' @param digits integer indicating the number of decimal places to be used.
#' @param method a scalar representing the type of confidence intervals
#'               required. The value should be any of the values "kelley",
#'               "mckay", "miller", "vangel", "mahmoudvand_hassani",
#'               "equal_tailed", "shortest_length", "normal_approximation",
#'               "norm","basic", "perc", "bca", or "all".
#' @param correction returns the unbiased estimate of the coefficient of
#'                   variation
#' @param alpha The allowed type I error probability
#' @param R integer indicating the number of bootstrap replicates.
cv <- function(
    x,  # Currently there are methods for numeric vectors
    na.rm = FALSE,  # indicating whether NA values should be stripped
    digits = NULL,  # digits of output after rounding. default is 4
    method = NULL,  # method for the computation of confidence interval (CI)
    correction = TRUE,  # indicating whether to compute the unbiased statistics
    alpha = 0.05,  # The allowed type I error probability
    R = NULL,  # integer indicating the number of bootstrap replicates
    ...
) {
    # library(MBESS)
    # require(dplyr)
    # require(SciViews)
    # require(boot)
    if (!is.numeric(x)) {  # checkpoint 1 typeof x
        stop("argument is not numeric: returning NA")
        return(NA_real_)
    }
    if (!is.vector(x)) {  # checkpoint 2 typeof x
        stop("x is not a vector")
        return(NA_real_)
    }
    na.rm <- na.rm  # removes NAs if TRUE
    if (is.null(digits)) {  # checkpoint 3 determining digits
        digits = 4
    }
    digits <- digits  # digits required for rounding
    method <- tolower(method)  # convert user's input to lower-case
    method <- match.arg(  # match the user's input with available methods
        arg = method,
        choices = c(
            "kelley", "mckay", "miller", "vangel", "mahmoudvand_hassani",
            "equal_tailed", "shortest_length", "normal_approximation",
            "norm","basic", "perc", "bca", "all"
        ),
        several.ok = TRUE
    )
    shortest_length <- data.frame(  # "a" and "b" values for shortest-length CI
        v = c(  # degrees of freedom
            2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
            21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 40, 50, 60, 70, 80, 90,
            100, 150, 200, 250, 300, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13,
            14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29,
            30, 40, 50, 60, 70, 80, 90, 100, 150, 200, 250, 300, 2, 3, 4,
            5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21,
            22, 23, 24, 25, 26, 27, 28, 29, 30, 40, 50, 60, 70, 80, 90,
            100, 150, 200, 250, 300
        ),
        al = c(  # al is the allowed type I error probability
            0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1,
            0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1,
            0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1,
            0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1,
            0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05,
            0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05,
            0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05,
            0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05,
            0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05,
            0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01,
            0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01,
            0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01,
            0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01,
            0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01
        ),
        a = c(  # a is an attribute for the length of CI
            0.2065, 0.5654, 1.02, 1.5352, 2.093, 2.6828, 3.2981, 3.9343,
            4.5883, 5.2573, 5.9397, 6.6337, 7.3382, 8.0521, 8.7745,
            9.5047, 10.2421, 10.9861, 11.7362, 12.4919, 13.253,
            14.0191, 14.7899, 15.565, 16.3443, 17.1275, 17.9144,
            18.7049, 19.4987, 27.5919, 35.9012, 44.3661, 52.9501,
            61.629, 70.386, 79.2086, 124.0372, 169.6646, 215.8057,
            262.3132, 0.1015, 0.3449, 0.6918, 1.1092, 1.5776,
            2.0851, 2.6235, 3.1874, 3.7729, 4.3768, 4.9967,
            5.6308, 6.2776, 6.9357, 7.6042, 8.282, 8.9685,
            9.6629, 10.3647, 11.0733, 11.7882, 12.5092,
            13.2357, 13.9675, 14.7043, 15.4458, 16.1917,
            16.9419, 17.6961, 25.4233, 33.4085, 41.5794, 49.8923,
            58.3183, 66.8374, 75.4347, 119.2737, 164.0642,
            209.4667, 255.3057, 0.02, 0.114, 0.2937, 0.5461,
            0.8567, 1.2143, 1.6107, 2.0394, 2.4958, 2.976, 3.4771,
            3.9968, 4.5329, 5.084, 5.6487, 6.2256, 6.8139, 7.4126,
            8.0209, 8.6383, 9.264, 9.8976, 10.5385, 11.1864, 11.8408,
            12.5014, 13.1678, 13.8397, 14.517, 21.5331, 28.8879,
            36.4863, 44.2711, 52.2044, 60.2597, 68.4177, 110.3262,
            153.4834, 197.444, 241.9776
        ),
        b = c(  # b is an attribute for the length of CI
            12.5208, 13.1532, 14.18, 15.3498, 16.5807, 17.8391,
            19.1099, 20.3848, 21.6598, 22.9325, 24.2016,
            25.4666, 26.7269, 27.9825, 29.2334, 30.4796,
            31.7212, 32.9585, 34.1915, 35.4205, 36.6455,
            37.8668, 39.0844, 40.2986, 41.5095, 42.7171,
            43.9217, 45.1234, 46.3222, 58.1755, 69.8342,
            81.3479, 92.7487, 104.0584, 115.2925, 126.4628,
            181.6128, 235.9748, 289.8273, 343.3155, 15.1194,
            15.5897, 16.5735, 17.7432, 18.9954, 20.2863,
            21.5953, 22.9118, 24.2303, 25.5476, 26.8618,
            28.1717, 29.4769, 30.777, 32.072, 33.3619,
            34.6467, 35.9266, 37.2016, 38.472, 39.7379,
            40.9995, 42.257, 43.5105, 44.7601, 46.006,
            47.2483, 48.4872, 49.7229, 61.9217, 73.892,
            85.6914, 97.3573, 108.9153, 120.3839, 131.7767,
            187.9079, 243.1025, 297.691, 351.8461, 20.8264,
            20.9856, 21.8371, 22.9867, 24.2618, 25.6017,
            26.9749, 28.3643, 29.7602, 31.158, 32.5543,
            33.9474, 35.3358, 36.7192, 38.0968, 39.4688,
            40.8347, 42.1952, 43.5498, 44.8989, 46.2426,
            47.581, 48.9144, 50.2428, 51.5665, 52.8856,
            54.2002, 55.5107, 56.8169, 69.6808, 82.2534,
            94.6063, 106.7867, 118.8272, 130.7514, 142.5771,
            200.6194, 257.4375, 313.462, 368.9185
        )
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
        sd(x, na.rm = na.rm)/mean(x, na.rm = na.rm)  # coefficient of variation
    )
    cv_corr <- cv * (
        (1 - (1/(4 * (length(x) - 1))) +  # corrected coefficient of variation
             (1/length(x)) * cv^2) +
            (1/(2 * (length(x) - 1)^2))
    )
# calculating cv and its CI attributes based on selected methods
    if ("kelley" %in% method && correction == FALSE) {
        ci <- MBESS::conf.limits.nct(
            ncp = sqrt(length(x))/cv_corr,
            df = length(x) - 1,
            conf.level = (1 - alpha)
        )
        est <- cv  # cv is an estimate of CV
        lower.tile <- unname(sqrt(length(x))/ci$Upper.Limit)
        upper.tile <- unname(sqrt(length(x))/ci$Lower.Limit)
    } else if (method == "kelley" && correction == TRUE) {
        ci <- MBESS::conf.limits.nct(
            ncp = sqrt(length(x))/cv_corr,
            df = length(x) - 1,
            conf.level = (1 - alpha)
        )
        est <- cv_corr  # corrected cv is an estimate of CV
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
        est <- cv  # cv is an estimate of CV
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
        est <- cv_corr  # corrected cv is an estimate of CV
        lower.tile <- cv_corr/sqrt((u1/(v + 1) - 1 )*(cv_corr^2) + u1/v)
        upper.tile <- cv_corr/sqrt((u2/(v + 1) - 1)*(cv_corr^2) + u2/v)
    } else if (method == "miller" && correction == FALSE) {
        v <- length(x) - 1
        z_alpha_over2 <- qnorm(1 - (alpha/2))
        u <- sqrt(
            (cv^2/v) * (0.5 + cv^2)
        )
        zu <- z_alpha_over2 * u
        est <- cv  # cv is an estimate of CV
        lower.tile <- cv - zu
        upper.tile <- cv + zu
    } else if (method == "miller" && correction == TRUE) {
        v <- length(x) - 1
        z_alpha_over2 <- qnorm(1 - (alpha/2))
        u <- sqrt(
            (cv_corr^2/v) * (0.5 + cv_corr^2)
        )
        zu <- z_alpha_over2 * u
        est <- cv_corr  # corrected cv is an estimate of CV
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
        est <- cv  # cv is an estimate of CV
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
        est <- cv_corr  # corrected cv is an estimate of CV
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
        est <- cv  # cv is an estimate of CV
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
        est <- cv_corr  # corrected cv is an estimate of CV
        lower.tile <- cv_corr/ul
        upper.tile <- cv_corr/uu
    } else if (method == "normal_approximation" && correction == FALSE) {
        cn <- sqrt(1 - (1/(2 * length(x))))
        ul <- cn + (qnorm(1 - (alpha/2)) * sqrt(1 - cn^2))
        uu <- cn - (qnorm(1 - (alpha/2)) * sqrt(1 - cn^2))
        est <- cv  # cv is an estimate of CV
        lower.tile <- cv/ul
        upper.tile <- cv/uu
    } else if (method == "normal_approximation" && correction == TRUE) {
        cn <- sqrt(1 - (1/(2 * length(x))))
        ul <- cn + (qnorm(1 - (alpha/2)) * sqrt(1 - cn^2))
        uu <- cn - (qnorm(1 - (alpha/2)) * sqrt(1 - cn^2))
        est <- cv_corr  # corrected cv is an estimate of CV
        lower.tile <- cv_corr/ul
        upper.tile <- cv_corr/uu
    } else if (method == "shortest_length" && correction == FALSE) {
        if (length(x) <= 300) {
            a_value <- shortest_length %>%
                subset(al == alpha & v == length(x) - 1) %>%
                dplyr::select(a)
            b_value <- shortest_length %>%
                subset(al == alpha & v == length(x) - 1) %>%
                dplyr::select(b)
        } else if (length(x) > 300) {
            a_value <- shortest_length %>%
                subset(al == alpha & v == 300) %>%
                dplyr::select(a)
            b_value <- shortest_length %>%
                subset(al == alpha & v == 300) %>%
                dplyr::select(b)
        }
        est <- cv  # cv is an estimate of CV
        lower.tile <- (cv*sqrt(length(x)))/sqrt(b_value$b)
        upper.tile <- (cv*sqrt(length(x)))/sqrt(a_value$a)
    } else if (method == "shortest_length" && correction == TRUE) {
        if (length(x) <= 300) {
            a_value <- shortest_length %>%
                subset(al == alpha & v == length(x) - 1) %>%
                dplyr::select(a)
            b_value <- shortest_length %>%
                subset(al == alpha & v == length(x) - 1) %>%
                dplyr::select(b)
        } else if (length(x) > 300) {
            a_value <- shortest_length %>%
                subset(al == alpha & v == 300) %>%
                dplyr::select(a)
            b_value <- shortest_length %>%
                subset(al == alpha & v == 300) %>%
                dplyr::select(b)
        }
        est <- cv_corr  # corrected cv is an estimate of CV
        lower.tile <- (cv_corr*sqrt(length(x)))/sqrt(b_value$b)
        upper.tile <- (cv_corr*sqrt(length(x)))/sqrt(a_value$a)
    }
# preparing the output based on the selected methods
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
    } else if (method == "shortest_length" && correction == FALSE) {
        return(
            list(
                method = "cv with Shortest-Length 95% CI",
                statistics = data.frame(
                    est = round(est * 100, digits = digits),
                    lower = round(lower.tile * 100, digits = digits),
                    upper = round(upper.tile * 100, digits = digits),
                    row.names = c(" ")
                )
            )
        )
    } else if (method == "shortest_length" && correction == TRUE) {
        return(
            list(
                method = "Corrected cv with Shortest-Length 95% CI",
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
