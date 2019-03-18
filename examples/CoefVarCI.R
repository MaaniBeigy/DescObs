y <- c(
    0.2, 0.5, 1.1, 1.4, 1.8, 2.3, 2.5, 2.7, 3.5, 4.4,
    4.6, 5.4, 5.4, 5.7, 5.8, 5.9, 6.0, 6.6, 7.1, 7.9
)
CoefVarCI$new(y)$kelley_ci()
cv_y <- CoefVarCI$new(
    y,
    alpha = 0.05,
    R = 1000,
    digits = 2,
    correction = TRUE
)
cv_y$kelley_ci()
cv_y$mckay_ci()
cv_y$miller_ci()
cv_y$vangel_ci()
cv_y$mh_ci()
cv_y$equal_ci()
cv_y$shortest_ci()
cv_y$normaapprox_ci()
cv_y$norm_ci()
cv_y$basic_ci()
cv_y$perc_ci()
cv_y$bca_ci()
cv_y$all_ci()
R6::is.R6(cv_y)
