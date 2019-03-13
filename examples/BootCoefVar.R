x <- c(
    0.2, 0.5, 1.1, 1.4, 1.8, 2.3, 2.5, 2.7, 3.5, 4.4,
    4.6, 5.4, 5.4, 5.7, 5.8, 5.9, 6.0, 6.6, 7.1, 7.9
)
cv_x <- BootCoefVar$new(x, na.rm = TRUE, digits = 2, R = 1000, alpha = 0.05)
cv_x$boot_cv()
cv_x$boot_basic_ci()
cv_x$boot_norm_ci()
cv_x$boot_perc_ci()
cv_x$boot_bca_ci()
R6::is.R6(cv_x)

