# QuantIndex
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
                        probs = self$probs,  # third quartile (0.75 percentile)
                        na.rm = self$na.rm
                        )
                    )
                )
        }
        )
)
