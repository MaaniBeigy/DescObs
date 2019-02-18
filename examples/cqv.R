x <- c(
    0.2, 0.5, 1.1, 1.4, 1.8, 2.3, 2.5, 2.7, 3.5, 4.4,
    4.6, 5.4, 5.4, 5.7, 5.8, 5.9, 6.0, 6.6, 7.1, 7.9
)
cqv(x)
cqv(x, na.rm = TRUE)
# cqv
# 45.625
cqv(x, na.rm = TRUE, digits = 2)
# cqv
# 45.62
cqv(x, na.rm = TRUE, digits = 2, CI = "Bonett")
# cqv     Bonett's 95% CI
#   "45.62" "24.78-77.33"
cqv(x, na.rm = TRUE, digits = 2, CI = "norm")
# cqv     normal approximation 95% CI
# "45.62" "19.95-71.04"
cqv(x, na.rm = TRUE, digits = 2, CI = "basic")
# cqv     basic bootstrap 95% CI
# "45.62" "19.5-73.37"
cqv(x, na.rm = TRUE, digits = 2, CI = "perc")
# cqv     bootstrap percentile 95% CI
# "45.62" "16.22-68.68"
cqv(x, na.rm = TRUE, digits = 2, CI = "bca")
# cqv     adjusted bootstrap percentile (BCa) 95% CI
# "41.74" "23.69-80.39"


