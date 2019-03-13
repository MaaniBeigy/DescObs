y <- c(
    0.2, 0.5, 1.1, 1.4, 1.8, 2.3, 2.5, 2.7, 3.5, 4.4,
    4.6, 5.4, 5.4, 5.7, 5.8, 5.9, 6.0, 6.6, 7.1, 7.9
)
CoefQuartVarCI$new(y, digits = 2, na.rm = TRUE)$bonett_ci()
cqv_y <- CoefQuartVarCI$new(
    y,
    alpha = 0.05,
    R = 1000,
    digits = 2
    )
cqv_y$bonett_ci()
cqv_y$norm_ci()
cqv_y$basic_ci()
cqv_y$perc_ci()
cqv_y$bca_ci()
cqv_y$all_ci()
R6::is.R6(cqv_y)
