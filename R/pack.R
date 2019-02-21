# # this is for testing
# #
# # x <- c(
# #
# #     0.2, 0.5, 1.1, 1.4, 1.8, 2.3, 2.5, 2.7, 3.5, 4.4,
# #
# #     4.6, 5.4, 5.4, 5.7, 5.8, 5.9, 6.0, 6.6, 7.1, 7.9
# #
# # )
#
# cqv <- function(x, na.rm = FALSE, digits = NULL, CI = NULL, ...) {
#
#     if (!is.numeric(x)) {
#
#         stop("argument is not numeric: returning NA")
#
#         return(NA_real_)
#
#     }
#
#     if (!is.vector(x)) {
#
#         stop("x is not a vector")
#
#         return(NA_real_)
#
#     }
#
#     library(dplyr)
#
#     library(SciViews)
#
#     library(boot)
#
#     na.rm = na.rm  # removes NAs if TRUE
#
#     if (is.null(digits)) {
#
#         digits = 4
#
#     }
#
#     digits = digits  # digits required for rounding
#
#     CI = CI  # returns 95% confidence interval if TRUE
#
#     q3 <- unname(
#
#         quantile(
#
#             x,
#
#             probs = 0.75,  # third quartile (0.75 percentile)
#
#             na.rm = na.rm
#
#         )
#
#     )
#
#     q1 <- unname(
#
#         quantile(
#
#             x,
#
#             probs = 0.25,  # first quartile (0.75 percentile)
#
#             na.rm = na.rm
#
#         )
#
#     )
#
#     if (q3 == 0) {  # to avoid NaNs when q3 and q1 are zero
#
#         q3 <- max(x)
#
#     }
#
#     a <- ceiling(
#
#         (length(x)/4) - (1.96 * (((3 * length(x))/16)^(0.5)))
#
#     )
#
#     b <- round(
#
#         (length(x)/4) + (1.96 * (((3 * length(x))/16)^(0.5))),
#
#         digits = 0
#
#     )
#
#
#
#     c <- length(x) + 1 - b
#
#     d <- length(x) + 1 - a
#
#     Ya <- nth(x, a, order_by = x)
#
#     Yb <- nth(x, b, order_by = x)
#
#     Yc <- nth(x, c, order_by = x)
#
#     Yd <- nth(x, d, order_by = x)
#
#     star <- 0
#
#     for (i in a:(b - 1)) {
#
#         star[i] <- (
#
#             (choose(length(x), i)) * (0.25^(i)) * (0.75^(length(x) - i))
#
#         )
#
#         alphastar <- 1 - sum(star[i])
#
#     }
#
#     zzz <- qnorm((1 - ((1 - alphastar)/2)))
#
#     f1square <- (3 * (zzz)^2)/(4 * length(x) * ((Yb - Ya)^2))
#
#     f3square <- (3 * (zzz)^2)/(4 * length(x) * ((Yd - Yc)^2))
#
#     D <- q3 - q1
#
#     S <- q3 + q1
#
#     v <- (
#
#         (1/(16 * length(x))) * (
#
#             (((3/f1square) + (3/f3square) - (2/sqrt(f1square * f3square))) / D^2) +
#
#                 (((3/f1square) + (3/f3square) + (2/sqrt(f1square * f3square))) / S^2) -
#
#                 ((2 * ((3/f3square) - (3/f1square)))/(D*S))
#
#         )
#
#     )
#
#     ccc <- length(x)/(length(x) - 1)
#
#     upper.tile <- exp(((ln((D/S)) * ccc)) + (zzz * (v^(0.5))))
#
#     lower.tile <- exp(((ln((D/S)) * ccc)) - (zzz * (v^(0.5))))
#
#     boot.cqv <- boot(
#
#         x,
#
#         function(x, i) {
#
#             round(((
#
#                 unname(quantile(x[i], probs = 0.75, na.rm = na.rm)) -
#
#                     unname(quantile(x[i], probs = 0.25, na.rm = na.rm))
#
#             ) / (
#
#                 unname(quantile(x[i], probs = 0.75, na.rm = na.rm)) +
#
#                     unname(quantile(x[i], probs = 0.25, na.rm = na.rm))
#
#             )) * 100, digits = digits)
#
#         },
#
#         R = 1000
#
#     )
#
#     if (is.null(CI)) {
#
#         boot.cqv.ci <- NA
#
#     } else if (CI == "Bonett") {
#
#         boot.cqv.ci <- NA
#
#     } else if (CI == "norm") {
#
#         boot.cqv.ci <- boot.ci(boot.cqv, conf = 0.95, type = "norm")
#
#     } else if (CI == "basic") {
#
#         boot.cqv.ci <- boot.ci(boot.cqv, conf = 0.95, type = "basic")
#
#     } else if (CI == "stud") {
#
#         boot.cqv.ci <- boot.ci(boot.cqv, conf = 0.95, type = "stud")
#
#     } else if (CI == "perc") {
#
#         boot.cqv.ci <- boot.ci(boot.cqv, conf = 0.95, type = "perc")
#
#     } else if (CI == "bca") {
#
#         boot.cqv.ci <- boot.ci(boot.cqv, conf = 0.95, type = "bca")
#
#     }
#
#
#
#     if (is.null(CI)) {
#
#         cqv <- round(
#
#             100 * ((q3 - q1)/(q3 + q1)), digits = digits
#
#         )
#
#     } else if (CI == "Bonett") {
#
#         cqv <- round(
#
#             100 * ((q3 - q1)/(q3 + q1)), digits = digits
#
#         )
#
#     } else if (CI == "norm") {
#
#         cqv <- round(
#
#             100 * ((q3 - q1)/(q3 + q1)), digits = digits
#
#         )
#
#     } else if (CI == "basic") {
#
#         cqv <- round(
#
#             100 * ((q3 - q1)/(q3 + q1)), digits = digits
#
#         )
#
#     } else if (CI == "perc") {
#
#         cqv <- round(
#
#             100 * ((q3 - q1)/(q3 + q1)), digits = digits
#
#         )
#
#     } else if (CI == "bca") {
#
#         cqv <- (
#
#             boot.cqv.ci$bca[2]
#
#         )
#
#     }
#
#
#
#     if (is.null(CI)) {
#
#         CI95 <- NA
#
#     } else if (CI == "Bonett") {
#
#         CI95 <- paste0(
#
#             round(lower.tile * 100, digits = digits),
#
#             "-",
#
#             round(upper.tile * 100, digits = digits)
#
#         )
#
#     } else if (CI == "norm") {
#
#         CI95 <- paste0(
#
#             round(boot.cqv.ci$normal[2], digits = digits),
#
#             "-",
#
#             round(boot.cqv.ci$normal[3], digits = digits)
#
#         )
#
#     } else if (CI == "basic") {
#
#         CI95 <- paste0(
#
#             round(boot.cqv.ci$basic[4], digits = digits),
#
#             "-",
#
#             round(boot.cqv.ci$basic[5], digits = digits)
#
#         )
#
#     } else if (CI == "perc") {
#
#         CI95 <- paste0(
#
#             round(boot.cqv.ci$percent[4], digits = digits),
#
#             "-",
#
#             round(boot.cqv.ci$percent[5], digits = digits)
#
#         )
#
#     } else if (CI == "bca") {
#
#         CI95 <- paste0(
#
#             round(boot.cqv.ci$bca[4], digits = digits),
#
#             "-",
#
#             round(boot.cqv.ci$bca[5], digits = digits)
#
#         )
#
#     }
#
#
#
#     if (is.null(CI)) {
#
#         return(
#
#             structure(
#
#                 .Data = c(cqv),
#
#                 "dim" = c(1, 1),
#
#                 "dimnames" = list(c(" "), c("cqv"))
#
#             )
#
#         )
#
#     } else if (CI == "Bonett") {
#
#         return(
#
#             structure(
#
#                 .Data = c(cqv, CI95),
#
#                 "dim" = c(1, 2),
#
#                 "dimnames" = list(c(" "), c("cqv", "Bonett's 95% CI"))
#
#             )
#
#         )
#
#     } else if (CI == "norm") {
#
#         return(
#
#             structure(
#
#                 .Data = c(cqv, CI95),
#
#                 "dim" = c(1, 2),
#
#                 "dimnames" = list(c(" "),
#
#                                   c("cqv", "normal approximation 95% CI"))
#
#             )
#
#         )
#
#     } else if (CI == "basic") {
#
#         return(
#
#             structure(
#
#                 .Data = c(cqv, CI95),
#
#                 "dim" = c(1, 2),
#
#                 "dimnames" = list(c(" "),
#
#                                   c("cqv", "basic bootstrap 95% CI"))
#
#             )
#
#         )
#
#     } else if (CI == "perc") {
#
#         return(
#
#             structure(
#
#                 .Data = c(cqv, CI95),
#
#                 "dim" = c(1, 2),
#
#                 "dimnames" = list(c(" "),
#
#                                   c("cqv", "bootstrap percentile 95% CI"))
#
#             )
#
#         )
#
#     } else if (CI == "bca") {
#
#         return(
#
#             structure(
#
#                 .Data = c(cqv, CI95),
#
#                 "dim" = c(1, 2),
#
#                 "dimnames" = list(
#
#                     c(" "),
#
#                     c("cqv", "adjusted bootstrap percentile (BCa) 95% CI"))
#
#             )
#
#         )
#
#     } else {
#
#         stop("method for confidence interval is not available")
#
#         return(NA_real_)
#
#     }
#
#
#
#
#
# }
#
#
#
# cqv(x = x, na.rm = TRUE, digits = 2)
#
# cqv(x = x, na.rm = TRUE)
#
# cqv(x = x, na.rm = TRUE, digits = 2, CI = "Bonett")
#
# # cqv(x = x, na.rm = TRUE, digits = 2, CI = "stud")
#
# cqv(x = x, na.rm = TRUE, digits = 2, CI = "norm")
#
# cqv(x = x, na.rm = TRUE, digits = 2, CI = "perc")
#
# cqv(x = x, na.rm = TRUE, digits = 2, CI = "basic")
#
# cqv(x = x, na.rm = TRUE, digits = 2, CI = "bca")
#
# cqv(x = x)
#
# # cqv(x = x, na.rm = TRUE, digits = 2, CI = "khs")
#
# #
#
# # boot.cqv <- boot(
#
# #     x,
#
# #     function(x, i) {
#
# #         cqv(x[i])
#
# #     },
#
# #     R = 100
#
# # )
#
# boot.cqv.ci <- boot.ci(boot.cqv, conf = 0.95, type = "basic")
# df1 <- read.csv(
#     file = paste0(dir, "/data/checkmerged2.csv"),
#     encoding = "UTF-8",
#     sep = "|",
#     header = TRUE
# )
# cqv(x = df1$cardioadm, na.rm = TRUE, digits = 2, method = "norm", R = 1000)
#
