#' @title R6 Bootstrap Resampling for Coefficient of Quartile Variation
#' @name BootCoefQuartVar
#' @description The R6 class \code{BootCoefQuartVar} produces the bootstrap
#'              resamplimg for the coeficient of quartile variation (cqv) of the
#'              given numeric vectors. It uses \link[boot]{boot} from the
#'              package \pkg{boot}.
#' @usage \code{BootCoefQuartVar$new(x, ...)}
#'
#' ## Default R6 method:
#' \code{BootCoefQuartVar$new(x, na.rm = FALSE, digits = 4,
#'                R = 1000, ...)$boot_cqv()}
#' @param x An \code{R} object. Currently there are methods for numeric vectors
#' @param na.rm a logical value indicating whether \code{NA} values should be
#'              stripped before the computation proceeds.
#' @param digits integer indicating the number of decimal places to be used.
#' @param R integer indicating the number of bootstrap replicates.
#' @example ./examples/BootCoefQuartVar.R
#' @export
BootCoefQuartVar <- R6::R6Class(
    classname = "BootCoefQuartVar",
    inherit = SampleQuantiles,
    public = list(
        x = NA,
        na.rm = TRUE,
        digits = NULL,
        R = NA,
        initialize = function(
            x,
            na.rm,
            digits = NULL,
            R,
            ...
        ) {
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
            # ------------------- set digits with default = 4 -----------------
            if (!missing(digits)) {
                self$digits <- digits
            } else if (is.null(digits)) {
                self$digits <- 4
            } else if (missing(digits)) {
                self$digits <- 4
            } else {
                self$digits <- 4
            }
            # -- set the number of bootstrap replicates with default = 1000 ---
            if (missing(R)) {
                self$R <- 1000
            } else if (!missing(R)) {
                self$R <- R
            }
            # ----- initialize boot_cqv() i.e., bootstrap of cqv function -----
            self$boot_cqv()
        },
        # ---- public function boot_cqv() i.e., bootstrap of cqv function -----
        boot_cqv = function(
            ...
        ) {
            if (  # check if 0.75 percentile is non-zero to avoid NANs
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
            } else if (  # check if 0.75 percentile is zero to avoid NANs
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
        ),
    active = list(
        super_ = function() {super}
    )
)

