#' @title R6 Bootstrap Resampling for Coefficient of Quartile Variation
#' @name BootCoefQuartVar
#' @description The R6 class \code{BootCoefQuartVar} produces the bootstrap
#'              resamplimg for the coeficient of quartile variation (cqv) of the
#'              given numeric vectors. It uses \link[boot]{boot} from the
#'              package \pkg{boot}. Also, it produces the bootstrap confidence
#'              intervals for the cqv based on the \link[boot]{boot.ci} from the
#'              package \pkg{boot}.
#' @usage \code{BootCoefQuartVar$new(x, ...)}
#'
#' ## Default R6 method:
#' \code{BootCoefQuartVar$new(x, na.rm = FALSE, digits = 1,
#'                R = 1000, alpha = 0.05, ...)$boot_cqv()}
#' @param x An \code{R} object. Currently there are methods for numeric vectors
#' @param na.rm a logical value indicating whether \code{NA} values should be
#'              stripped before the computation proceeds.
#' @param digits integer indicating the number of decimal places to be used.
#' @param alpha The allowed type I error probability
#' @param R integer indicating the number of bootstrap replicates.
#' @example ./examples/BootCoefQuartVar.R
#' @export
#' @references Canty, A., & Ripley, B, 2017, boot: Bootstrap R (S-Plus)
#'             Functions. R package version 1.3-20.
#' @references Davison, AC., & Hinkley, DV., 1997, Bootstrap Methods and
#'             Their Applications. Cambridge University Press, Cambridge.
#'             ISBN 0-521-57391-2
#' @references Altunkaynak, B., Gamgam, H., 2018, Bootstrap confidence
#'             intervals for the coefficient of quartile variation,
#'             Simulation and Computation, 1-9, DOI: \href{
#'             https://doi.org/10.1080/03610918.2018.1435800}{
#'             https://doi.org/10.1080/03610918.2018.1435800}
BootCoefQuartVar <- R6::R6Class(
    classname = "BootCoefQuartVar",
    inherit = SampleQuantiles,
    public = list(
        # ---------------- determining defaults for arguments -----------------
        x = NA,
        na.rm = FALSE,
        digits = 1,
        R = 1000,
        alpha = 0.05,
        boot_cqv = NA,
        # --------- determining constructor defaults for arguments ------------
        initialize = function(
            x = NA,
            na.rm = FALSE,
            digits = 1,
            R = 1000,
            alpha = 0.05,
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
            # ------------------- set digits with user input ------------------
            if (!missing(digits)) {
                self$digits <- digits
            }
            # ---- set the number of bootstrap replicates with user input -----
            if (!missing(R)) {
                self$R <- R
            }
            # ------ set the probability of type I error with user input ------
            if (!missing(alpha)) {
                self$alpha <- alpha
            }
            # ----- initialize boot_cqv() i.e., bootstrap of cqv function -----
            self$boot_cqv = function(...) {
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
        },
        # -------- public methods of bootstrap confidence intervals ---------
        boot_norm_ci = function(...) {
            return(
                boot::boot.ci(
                    self$boot_cqv(),
                    conf = (1 - self$alpha),
                    type = "norm"
                )
            )
        },
        boot_basic_ci = function(...) {
            return(
                boot::boot.ci(
                    self$boot_cqv(),
                    conf = (1 - self$alpha),
                    type = "basic"
                )
            )
        },
        boot_perc_ci = function(...) {
            return(
                boot::boot.ci(
                    self$boot_cqv(),
                    conf = (1 - self$alpha),
                    type = "perc"
                )
            )
        },
        boot_bca_ci = function(...) {
            return(
                boot::boot.ci(
                    self$boot_cqv(),
                    conf = (1 - self$alpha),
                    type = "bca"
                )
            )
        }
        ),
    # ---- define super_ function to enable multiple levels of inheritance ----
    active = list(
        super_ = function() {super}
    )
)

