#' @title R6 Coefficient of Variation (cv)
#' @name CoefVar
#' @description The R6 class \code{CoefVar} for the coefficient of
#'              variation (cv)
#' @usage \code{CoefVar$new(x, ...)}
#' @param x An \code{R} object. Currently there are methods for numeric vectors
#' @param na.rm a logical value indicating whether \code{NA} values should be
#'              stripped before the computation proceeds.
#' @param digits integer indicating the number of decimal places to be used.
#' @details \describe{
#'         \item{\strong{Coefficient of Variation}}{
#'         \code{\deqn{ CV = \sigma/\mu} } where \eqn{\sigma}
#'         and \eqn{\mu} are standard deviation and mean, respectively.
#'         The \emph{cv} is a measure of relative dispersion representing
#'         the degree of variability relative to the mean [1]. Since \eqn{cv} is
#'         unitless, it is useful for comparison of variables with different
#'         units. It is also a measure of homogeneity [1].
#'         }
#'         }
#' @example ./examples/CoefVar.R
#' @references [1] Albatineh, AN., Kibria, BM., Wilcox, ML., & Zogheib, B, 2014,
#'                 Confidence interval estimation for the population coefficient
#'                 of variation using ranked set sampling: A simulation study,
#'                 Journal of Applied Statistics, 41(4), 733–751, DOI:
#'                 \href{https://doi.org/10.1080/02664763.2013.847405}{
#'                 https://doi.org/10.1080/02664763.2013.847405}
#' @export
CoefVar <- R6::R6Class(
    classname = "CoefQuartVar",
    inherit = BootCoefVar,
    public = list(
        x = NA,
        na.rm = TRUE,
        digits = NULL,
        initialize = function(
            x,
            digits = NULL,
            na.rm,
            ...
        ) {
            # ---------------------- check NA or NAN -------------------------
            if (!missing(x)) {
                self$x <- x
            } else if (!missing(x)) {
                stop("no numeric vector is selected for input")
            }
            if (!missing(na.rm)) {
                self$na.rm <- na.rm
            }
            if (self$na.rm == TRUE) {
                self$x <- x[!is.na(x)]
            } else if (anyNA(x)) {
                stop(
                    "missing values and NaN's not allowed if 'na.rm' is FALSE"
                )
            }
            # ------------- stop if input x vector is not numeric -------------
            if (!is.numeric(x)) {
                stop("argument is not numeric: returning NA")
                return(NA_real_)
            }
            if (!is.vector(x)) {
                stop("x is not a vector")
                return(NA_real_)
            }
            # ------------------- set digits with default = 4 -----------------
            if (!missing(digits)) {
                self$digits <- digits
            } else {
                stop("please determine digits level")
            }
            # ---------------- initialize cv() i.e., cqv function ------------
            self$est()
        },
        # -------------- public function cv() i.e., cqv function -------------
        est = function(...) {
            return(
                round(
                    100 * (
                        (sd(self$x, na.rm = self$na.rm)) /
                            (mean(self$x, na.rm = self$na.rm))
                    ), digits = self$digits
                )
            )
        }
    ),
    active = list(
        super_ = function() super
    )
)
