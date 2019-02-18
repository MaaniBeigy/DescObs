x <- c(
    0.2, 0.5, 1.1, 1.4, 1.8, 2.3, 2.5, 2.7, 3.5, 4.4,
    4.6, 5.4, 5.4, 5.7, 5.8, 5.9, 6.0, 6.6, 7.1, 7.9
)
cqv(x)
cqv(x, na.rm = TRUE, digits = 2)
cqv(x, na.rm = TRUE, digits = 2, CI = "Bonett")
cqv(x, na.rm = TRUE, digits = 2, CI = "norm")
cqv(x, na.rm = TRUE, digits = 2, CI = "basic")
cqv(x, na.rm = TRUE, digits = 2, CI = "perc")
cqv(x, na.rm = TRUE, digits = 2, CI = "bca")
cqv(x, na.rm = TRUE, digits = 2, CI = "all")

