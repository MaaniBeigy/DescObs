#' @title R6 Coefficient of Quartile Variation (cqv)
#' @name CoefQuartVar
#' @description The R6 class \code{CoefQuartVar} for the coefficient of
#'              quartile variation (cqv)
#' @usage \code{CoefQuartVar$new(x, ...)}
#'
#' ## Default R6 method:
#' \code{CoefQuartVar$new(x, na.rm = FALSE, digits = 1)$est()}
#' @param x An \code{R} object. Currently there are methods for numeric vectors
#' @param na.rm a logical value indicating whether \code{NA} values should be
#'              stripped before the computation proceeds.
#' @param digits integer indicating the number of decimal places to be used.
#' @details \describe{
#'         \item{\strong{Coefficient of Quartile Variation}}{
#'         \code{\deqn{ cqv = ((q3-q1)/(q3 + q1))*100 , } } where \eqn{q3}
#'         and \eqn{q1} are third quartile (\emph{i.e.,} 75th percentile) and
#'         first quartile (\emph{i.e.,} 25th percentile), respectively.
#'         The \emph{cqv} is a measure of relative dispersion that is based on
#'         interquartile range \emph{(iqr)}. Since \eqn{cqv} is unitless, it
#'         is useful for comparison of variables with different units. It is
#'         also a measure of homogeneity [1, 2].
#'         }
#'         }
#' @example ./examples/CoefQuartVar.R
#' @references [1] Bonett, DG., 2006, Confidence interval for a coefficient of
#'                 quartile variation, Computational Statistics & Data Analysis,
#'                 50(11), 2953-7, DOI: \href{
#'                 https://doi.org/10.1016/j.csda.2005.05.007}{
#'                 https://doi.org/10.1016/j.csda.2005.05.007}
#' @export
CoefQuartVar <- R6::R6Class(
    classname = "CoefQuartVar",
    inherit = BootCoefQuartVar,
    public = list(
        # ---------------- determining defaults for arguments -----------------
        x = NA,
        na.rm = FALSE,
        digits = 1,
        star = 0,
        # --------- determining constructor defaults for arguments ------------
        initialize = function(
            x = NA,
            na.rm = FALSE,
            digits = 1,
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
            # ------------------- set digits with user input ------------------
            if (!missing(digits)) {
                self$digits <- digits
            }
            # ---------- initialize cqv estimate i.e., est() function ---------
            self$est()
        },
        # --------- public method of cqv estimate i.e., est() function --------
        est = function(...) {
            if (  # check if 0.75 percentile is non-zero to avoid NANs
                super$super_$initialize(
                    x = self$x, na.rm = TRUE, probs = 0.75,
                    digits = self$digits
                ) != 0
            ) {
                return(
                    round(
                        (((super$super_$initialize(
                            x = self$x,
                            na.rm = TRUE,
                            probs = 0.75,
                            names = FALSE,
                            digits = self$digits
                        )) - (super$super_$initialize(
                            x = self$x,
                            na.rm = TRUE,
                            probs = 0.25,
                            names = FALSE,
                            digits = self$digits
                        ) )) / ((super$super_$initialize(
                            x = self$x,
                            na.rm = TRUE,
                            probs = 0.75,
                            names = FALSE,
                            digits = self$digits
                        )) + (super$super_$initialize(
                            x = self$x,
                            na.rm = TRUE,
                            probs = 0.25,
                            names = FALSE,
                            digits = self$digits
                        )))) * 100,
                        digits = self$digits
                    )
                )
            } else if (
                super$super_$initialize(
                    x = self$x, na.rm = TRUE, probs = 0.75,
                    digits = self$digits
                ) == 0
            ) {
                return(
                    round(
                        ((max(x = self$x) - (super$super_$initialize(
                            x = self$x,
                            na.rm = TRUE,
                            probs = 0.25,
                            names = FALSE
                        ) )) / (max(x = self$x) + (super$super_$initialize(
                            x = self$x,
                            na.rm = TRUE,
                            probs = 0.25,
                            names = FALSE
                        )))) * 100,
                        digits = self$digits
                    )
                )
            }
        },
        # ------------------- adding some internal methods --------------------
        # --------------------- adding some internal fields -------------------
        a =  function(...) {
            ceiling(
            (length(self$x)/4) - (1.96 * (((3 * length(self$x))/16)^(0.5)))
        )
            },
        b = function(...) {round(
            (length(self$x)/4) + (1.96 * (((3 * length(self$x))/16)^(0.5))),
            digits = 0
        )
            },
        c = function(...) {length(self$x) + 1 - self$b()},
        d = function(...) {length(self$x) + 1 - self$a()},
        Ya = function(...) {dplyr::nth(self$x, self$a(), order_by = self$x)},
        Yb = function(...) {dplyr::nth(self$x, self$b(), order_by = self$x)},
        Yc = function(...) {dplyr::nth(self$x, self$c(), order_by = self$x)},
        Yd = function(...) {dplyr::nth(self$x, self$d(), order_by = self$x)},
        alphastar = function(...) {
            for (i in self$a():(self$b() - 1)) {
                self$star[i] <- (
                    (choose(length(self$x), i)) *
                        (0.25^(i)) * (0.75^(length(self$x) - i))
                )
                return(alphastar = 1 - sum(self$star[i], na.rm = self$na.rm))
            }
        }
    ),
    # ---- define super_ function to enable multiple levels of inheritance ----
    active = list(
        super_ = function() super
    )
)



