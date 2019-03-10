# BootCoefQuartVar
BootCoefQuartVar <- R6::R6Class(
    classname = "BootCoefQuartVar",
    inherit = QuantIndex,
    public = list(
        x = NA,
        na.rm = TRUE,
        digits = NA,
        R = NA,
        initialize = function(
            x,
            na.rm,
            digits,
            R,
            ...
        ) {
            if (!missing(x)) {
                self$x <- x
            }
            if (!is.numeric(x)) {
                stop("argument is not numeric: returning NA")
                return(NA_real_)
            }
            if (!is.vector(x)) {
                stop("x is not a vector")
                return(NA_real_)
            }
            if (missing(na.rm)) {
                self$na.rm <- TRUE
            } else if (!missing(na.rm)) {
                self$na.rm <- na.rm
            }
            if (!missing(digits)) {
                self$digits <- digits
            } else if (is.null(digits)) {
                self$digits <- 4
            }
            if (!missing(R)) {
                self$R <- R
            } else if (is.null(R)) {
                self$R <- 1000
            }
        },
        boot_cqv = function(
            x,
            na.rm,
            digits,
            R
        ) {
            if (
                super$initialize(x = self$x, na.rm = TRUE, probs = 0.75) != 0
                ) {
                return(
                        boot::boot(self$x, function(x, i) {
                            round(((
                                unname(
                                    quantile(
                                        self$x[i],
                                        probs = 0.75,
                                        na.rm = self$na.rm
                                    )
                                ) - unname(
                                    quantile(
                                        self$x[i],
                                        probs = 0.25,
                                        na.rm = self$na.rm)
                                )
                            ) / (unname(
                                quantile(
                                    self$x[i],
                                    probs = 0.75,
                                    na.rm = self$na.rm)
                            ) + unname(
                                quantile(
                                    self$x[i],
                                    probs = 0.25,
                                    na.rm = self$na.rm
                                )
                            )
                            )) * 100, digits = self$digits)
                        },
                        R = self$R
                        )
                )
            } else if (
                super$initialize(x = self$x, na.rm = TRUE, probs = 0.75) == 0
                ) {
                warning(
        "cqv is NaN because both q3 and q1 are 0, max was used instead of q3"
                    )
                return(
                    boot::boot(self$x, function(x, i) {
                        round(((
                            unname(
                                max(
                                    self$x[i],
                                    na.rm = self$na.rm
                                )
                            ) - unname(
                                quantile(
                                    self$x[i],
                                    probs = 0.25,
                                    na.rm = self$na.rm)
                            )
                        ) / (unname(
                            max(
                                self$x[i],
                                na.rm = self$na.rm)
                        ) + unname(
                            quantile(
                                self$x[i],
                                probs = 0.25,
                                na.rm = self$na.rm
                            )
                        )
                        )) * 100, digits = self$digits)
                    },
                    R = self$R
                    )
                )
            }
        }
        )
)

