x <- c(
    0.2, 0.5, 1.1, 1.4, 1.8, 2.3, 2.5, 2.7, 3.5, 4.4,
    4.6, 5.4, 5.4, 5.7, 5.8, 5.9, 6.0, 6.6, 7.1, 7.9
)
cqv_x <- BootCoefQuartVar$new(
    x = x,
    na.rm = TRUE,
    digits = 2,
    R = 1000,
    alpha = 0.05
)
cqv_x$boot_cqv()
cqv_x$boot_basic_ci()
cqv_x$boot_norm_ci()
cqv_x$boot_perc_ci()
cqv_x$boot_bca_ci()
R6::is.R6(cqv_x)

