x <- c(
    0.2, 0.5, 1.1, 1.4, 1.8, 2.3, 2.5, 2.7, 3.5, 4.4,
    4.6, 5.4, 5.4, 5.7, 5.8, 5.9, 6.0, 6.6, 7.1, 7.9
)
CoefQuartVar$new(x)$est()
cqv_x <- CoefQuartVar$new(x, digits = 2)
cqv_x$est()
R6::is.R6(cqv_x)

