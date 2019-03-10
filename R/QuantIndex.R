#' @title R6 Sample Quantiles
#' @name QuantIndex
#' @description The R6 class \code{QuantIndex} produces the sample quantiles
#'              corresponding to the given probabilities. It uses
#'              \link[stats]{quantile} from the package \pkg{stats}.
#' @usage \code{QuantIndex$new(x, ...)}
#'
#' ## Default R6 method:
#' \code{QuantIndex$new(x, na.rm = TRUE, digits = 4,
#'                probs = 0.5, ...)$qx()}
#' @param x An \code{R} object. Currently there are methods for numeric vectors
#' @param na.rm a logical value indicating whether \code{NA} values should be
#'              stripped before the computation proceeds.
#' @param digits integer indicating the number of decimal places to be used.
#' @param probs numeric vector of probabilities with values in \code{[0,1]}.
#' @example ./examples/QuantIndex.R
#' @export
QuantIndex <- R6::R6Class(
    classname = "QuantIndex",
    public = list(
        x = NA,
        na.rm = TRUE,
        digits = NA,
        probs = NA,
        initialize = function(
            x,
            na.rm ,
            digits,
            probs,
            ...
        ) {
            if (!missing(x)) {
                self$x <- x
            }
            if (missing(na.rm)) {
                self$na.rm <- TRUE
            } else if (!missing(na.rm)) {
                self$na.rm <- na.rm
            }
            if (missing(digits)) {
                self$digits <- 4
            } else if (is.null(digits)) {
                self$digits <- 4
            } else if (!missing(digits)) {
                self$digits <- digits
            }
            if (missing(probs)) {
                self$probs <- 0.5
            } else if (!missing(probs)) {
                self$probs <- probs
            }
            if (!is.numeric(x)) {
                stop("argument is not numeric: returning NA")
                return(NA_real_)
            }
            if (!is.vector(x)) {
                stop("x is not a vector")
                return(NA_real_)
            }
            self$qx()
        },
        qx = function(
            x,
            na.rm ,
            digits,
            probs,
            ...
            ) {
            return(
                unname(
                    quantile(
                        self$x,
                        probs = self$probs,
                        na.rm = self$na.rm
                        )
                    )
                )
        }
        )
)
