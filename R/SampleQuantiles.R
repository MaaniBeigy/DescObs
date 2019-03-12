#' @title R6 Sample Quantiles
#' @name SampleQuantiles
#' @description The R6 class \code{QuantIndex} produces the sample quantiles
#'              corresponding to the given probabilities. It uses
#'              \link[stats]{quantile} from the package \pkg{stats}.
#' @usage \code{SampleQuantiles$new(x, ...)}
#'
#' ## Default R6 method:
#' \code{SampleQuantiles$new(x, na.rm = FALSE, digits = 4,
#'                probs = 0.5, ...)$qx()}
#' @param x An \code{R} object. Currently there are methods for numeric vectors
#' @param na.rm a logical value indicating whether \code{NA} values should be
#'              stripped before the computation proceeds.
#' @param digits integer indicating the number of decimal places to be used.
#' @param probs numeric vector of probabilities with values in \code{[0,1]}.
#' @param type an integer between 1 and 9 selecting one of the nine quantile
#'             algorithms explained in \link[stats]{quantile} to be used.
#' @example ./examples/SampleQuantiles.R
#' @export
SampleQuantiles <- R6::R6Class(
    classname = "SampleQuantiles",
    public = list(
        x = NA,
        na.rm = FALSE,
        digits = NA,
        probs = NA,
        names = TRUE,
        type = 7,
        initialize = function(
            x,
            na.rm ,
            digits,
            probs,
            names,
            type,
            ...) {
            # ---------------------- check NA or NAN -------------------------
            if (!missing(x)) {
                self$x <- x
            } else if (!missing(x)) {
                stop("no numeric vector is selected for input")
            }
            if (missing(na.rm)) {
                self$na.rm <- FALSE
            } else if (!missing(na.rm)) {
                self$na.rm <- na.rm
            }
            if (na.rm == TRUE) {
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
            # -------------- check for probs being in range [0,1] -------------
            if (missing(probs)) {
                self$probs <- 0.5
            } else if (!missing(probs)) {
                self$probs <- probs
            }
            eps <- 100*.Machine$double.eps
            if (any((!is.na(self$probs)) &
                    (self$probs < -eps | self$probs > 1 + eps)))
                stop("'probs' outside [0,1]")
            # ------------------- set digits with default = 4 -----------------
            if (missing(digits)) {
                self$digits <- 4
            } else if (is.null(digits)) {
                self$digits <- 4
            } else if (!missing(digits)) {
                self$digits <- digits
            }
            # ------------------ set type with default = 7 --------------------
            if (missing(type)) {
                self$type <- 7
            } else if (is.null(digits)) {
                self$type <- 7
            } else if (!missing(digits)) {
                self$type <- type
            }
            # ---------------- set names with default = TRUE ------------------
            if (missing(names)) {
                self$names <- TRUE
            } else if (!missing(names)) {
                self$names <- names
            }
            self$qx()  # initialize qx() i.e., sample quantile function
        },
        # --------- public function  qx() i.e., sample quantile function ------
        qx = function(...) {
            return(
                    quantile(
                        self$x,
                        probs = self$probs,
                        na.rm = self$na.rm,
                        type = self$type,
                        names = self$names
                    )
            )
        }
    )
)
