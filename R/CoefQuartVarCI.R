#' @title R6 Confidence Intervals for the Coefficient of Quartile Variation
#'        (cqv)
#' @name CoefQuartVarCI
#' @description The R6 class \code{CoefQuartVarCI} for the confidence intervals of
#'              coefficient of quartile variation (cqv)
#' @usage \code{CoefQuartVarCI$new(x, ...)}
#'
#' ## Default R6 method:
#' \code{CoefQuartVarCI$new(x, na.rm = FALSE, digits = 4)$bonett_ci()}
#' @param x An \code{R} object. Currently there are methods for numeric vectors
#' @param na.rm a logical value indicating whether \code{NA} values should be
#'              stripped before the computation proceeds.
#' @param digits integer indicating the number of decimal places to be used.
#' @param methods the available computation methods of confidence intervals are:
#'                "bonett_ci", "norm_ci", "basic_ci", "perc_ci", "bca_ci" or
#'                "all_ci".
#' @param R integer indicating the number of bootstrap replicates.
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
#' @return An object of type "list" which contains the estimate, the
#'         intervals, and the computation method. It has two components:
#' @return \describe{
#'        \item{$method}{
#'        A description of statistical method used for the computations.
#'        }
#'        \item{$statistics}{
#'        A data frame representing three vectors: est, lower and upper limits
#'        of 95\% confidence interval \code{(CI)}:
#'        \cr \cr
#'        \strong{est:}{
#'        \code{\deqn{((q3-q1)/(q3 + q1))*100}}
#'        }
#'        \strong{Bonett 95\% CI:}{
#'        \code{\deqn{  exp{ln(D/S)C +/- (z(1 - alpha/2) * sqrt(v))}, }}
#'        where \eqn{C = n/(n - 1)} is a centering adjustment which helps to
#'        equalize the tail error probabilities. For this confidence interval,
#'        \eqn{D = q3 - q1} and \eqn{S = q3 + q1}; \eqn{z(1 - alpha/2)} is the
#'        \eqn{1 - alpha/2} quantile of the standard normal distribution [1, 2].
#'        }
#'        \cr \cr
#'        \strong{Normal approximation 95\% CI:}{
#'        The intervals calculated by the normal approximation [3, 4],
#'        using \link[boot]{boot.ci}.
#'        }
#'        \cr \cr
#'        \strong{Basic bootstrap 95\% CI:}{
#'        The intervals calculated by the basic bootstrap method [3, 4],
#'        using \link[boot]{boot.ci}.
#'        }
#'        \cr \cr
#'        \strong{Bootstrap percentile 95\% CI:}{
#'        The intervals calculated by the bootstrap percentile method [3, 4],
#'        using \link[boot]{boot.ci}.
#'        }
#'        \cr \cr
#'        \strong{Adjusted bootstrap percentile (BCa) 95\% CI:}{
#'        The intervals calculated by the adjusted bootstrap percentile
#'        (BCa) method [3, 4], using \link[boot]{boot.ci}.
#'        }
#'        }
#'        }
#' @example ./examples/CoefQuartVarCI.R
#' @references [1] Bonett, DG., 2006, Confidence interval for a coefficient of
#'                 quartile variation, Computational Statistics & Data Analysis,
#'                 50(11), 2953-7, DOI: \href{
#'                 https://doi.org/10.1016/j.csda.2005.05.007}{
#'                 https://doi.org/10.1016/j.csda.2005.05.007}
#' @references [2] Altunkaynak, B., Gamgam, H., 2018, Bootstrap confidence
#'                 intervals for the coefficient of quartile variation,
#'                 Simulation and Computation, 1-9, DOI: \href{
#'                 https://doi.org/10.1080/03610918.2018.1435800}{
#'                 https://doi.org/10.1080/03610918.2018.1435800}
#' @references [3] Canty, A., & Ripley, B, 2017, boot: Bootstrap R (S-Plus)
#'                 Functions. R package version 1.3-20.
#' @references [4] Davison, AC., & Hinkley, DV., 1997, Bootstrap Methods and
#'                 Their Applications. Cambridge University Press, Cambridge.
#'                 ISBN 0-521-57391-2
#' @export
CoefQuartVarCI <- R6::R6Class(
    classname = "CoefQuartVarCI",
    inherit = CoefQuartVar,
    public = list(
        x = NA,
        na.rm = TRUE,
        digits = NULL,
        method = NA,
        R = 1000,
        alpha = 0.05,
        zzz = NA,
        f1square = NA,
        f3square = NA,
        D = NA,
        S = NA,
        v = NA,
        ccc = NA,
        bootcqv = NA,
        boot_norm_ci = NA,
        boot_basic_ci = NA,
        boot_perc_ci = NA,
        boot_bca_ci = NA,
        initialize = function(
            x,
            na.rm,
            digits = NULL,
            method,
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
            if (missing(na.rm)) {
                self$na.rm <- FALSE
            } else if (!missing(na.rm)) {
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

            if (missing(alpha)) {
                self$alpha <- 0.05
            } else if (!missing(alpha)) {
                self$alpha <- alpha
            }
            # ------------- initialize zzz() i.e., z(1 - alpha/2) -------------
            self$zzz = function(...) {
                qnorm((1 - ((1 - super$alphastar())/2)))
            }
            # --------------- initialize internal function f1^2 ---------------
            self$f1square = function(...) {
                (3 * (self$zzz())^2)/(
                    4 * length(self$x) * ((super$Yb() - super$Ya())^2))
            }
            # --------------- initialize internal function f3^2 ---------------
            self$f3square =  function(...) {
                (3 * (self$zzz())^2)/(
                    4 * length(self$x) * ((super$Yd() - super$Yc())^2))
            }
            # ----------- initialize internal function D = q3 - q1 ------------
            self$D = function(...) {
                super$super_$super_$initialize(
                    x = self$x,
                    na.rm = TRUE,
                    probs = 0.75,
                    names = FALSE,
                    digits = self$digits
                ) - super$super_$super_$initialize(
                    x = self$x,
                    na.rm = TRUE,
                    probs = 0.25,
                    names = FALSE,
                    digits = self$digits
                )
            }
            # ----------- initialize internal function S = q3 + q1 ------------
            self$S = function(...) {
                super$super_$super_$initialize(
                    x = self$x,
                    na.rm = TRUE,
                    probs = 0.75,
                    names = FALSE,
                    digits = self$digits
                ) + super$super_$super_$initialize(
                    x = self$x,
                    na.rm = TRUE,
                    probs = 0.25,
                    names = FALSE,
                    digits = self$digits
                )
            }
            # -------- initialize internal function v = var{ln(D/S)} ----------
            self$v = function(...) {
                (
                    (1/(16 * length(self$x))) * (
                        (((3/self$f1square()) + (3/self$f3square()) - (
                            2/sqrt(self$f1square() * self$f3square()))) /
                             self$D()^2) +
                            (((3/self$f1square()) + (3/self$f3square()) +
                                  (2/sqrt(self$f1square() * self$f3square()))) /
                                 self$S()^2) -
                            ((2 * ((3/self$f3square()) - (3/self$f1square()))) /
                                 (self$D()*self$S()))
                    )
                )
            }
            # ----------- initialize internal function c = n/(n-1) ------------
            # ---------------- which is a centering adjustment ----------------
            self$ccc = function(...) {length(self$x)/(length(self$x) - 1)}
            # ---------- initialize normal approximation bootstrap ------------
            self$bootcqv = function(...) {
                return(super$super_$super_$initialize(
                    x = self$x,
                    na.rm = self$na.rm,
                    digits = self$digits,
                    R = 1000
                ))
            }
            self$boot_norm_ci = function(...) {
                boot::boot.ci(
                    self$boot_cqv(),
                    conf = (1 - self$alpha),
                    type = "norm"
                    )
            }
            self$boot_basic_ci = function(...) {
                boot::boot.ci(
                    self$boot_cqv(),
                    conf = (1 - self$alpha),
                    type = "basic"
                )
            }
            self$boot_perc_ci = function(...) {
                boot::boot.ci(
                    self$boot_cqv(),
                    conf = (1 - self$alpha),
                    type = "perc"
                )
            }
            self$boot_bca_ci = function(...) {
                boot::boot.ci(
                    self$boot_cqv(),
                    conf = (1 - self$alpha),
                    type = "bca"
                )
            }
        }
        ,
        # ------------ public function bonett_ci() i.e., Bonett Ci ------------
        bonett_ci = function(x, digits) {
            return(
                list(
                    method = "cqv with Bonett 95% CI",
                    statistics = data.frame(
                        est = super$est(),
                        lower = (
                            round(
                                (100 * exp(
                                    ((SciViews::ln((self$D()/self$S())) *
                                          self$ccc())) -
                                        (self$zzz() * (self$v()^(0.5)))
                                )), digits = self$digits
                            )
                        ),
                        upper = (
                            round(
                                (100 * exp(
                                    ((SciViews::ln((self$D()/self$S())) *
                                          self$ccc())) +
                                        (self$zzz() * (self$v()^(0.5)))
                                )), digits = self$digits
                            )
                        ),
                        row.names = c(" ")
                    )
                )
            )
        },
        norm_ci = function(...) {
            return(
                list(
                    method = "cqv with normal approximation bootstrap 95% CI",
                    statistics = data.frame(
                        est = super$est(),
                        lower = round(
                            self$boot_norm_ci()$normal[2],
                            digits = self$digits),
                        upper = round(
                            self$boot_norm_ci()$normal[3],
                            digits = self$digits),
                        row.names = c(" ")
                    )
                )
            )
        },
        basic_ci = function(...) {
            return(
                list(
                    method = "cqv with basic bootstrap 95% CI",
                    statistics = data.frame(
                        est = super$est(),
                        lower = round(
                            self$boot_basic_ci()$basic[4],
                            digits = self$digits),
                        upper = round(
                            self$boot_basic_ci()$basic[5],
                            digits = self$digits),
                        row.names = c(" ")
                    )
                )
            )
        },
        perc_ci = function(...) {
            return(
                list(
                    method = "cqv with bootstrap percentile 95% CI",
                    statistics = data.frame(
                        est = super$est(),
                        lower = round(
                            self$boot_perc_ci()$percent[4],
                            digits = self$digits),
                        upper = round(
                            self$boot_perc_ci()$percent[5],
                            digits = self$digits),
                        row.names = c(" ")
                    )
                )
            )
        },
        bca_ci = function(...) {
            return(
                list(
                method = "cqv with adjusted bootstrap percentile (BCa) 95% CI",
                    statistics = data.frame(
                        est = super$est(),
                        lower = round(
                            self$boot_bca_ci()$bca[4],
                            digits = self$digits),
                        upper = round(
                            self$boot_bca_ci()$bca[5],
                            digits = self$digits),
                        row.names = c(" ")
                    )
                )
            )
        },
        all_ci = function(...) {
            return(
                list(
                    method = "All methods",
                    statistics = data.frame(
                        row.names = c(
                            "bonett",
                            "norm",
                            "basic",
                            "percent",
                            "bca"
                        ),
                        est = c(
                            super$est(),
                            super$est(),
                            super$est(),
                            super$est(),
                            super$est()
                            ),
                        lower = c(
                            round(
                                (100 * exp(
                                    ((SciViews::ln((self$D()/self$S())) *
                                          self$ccc())) -
                                        (self$zzz() * (self$v()^(0.5)))
                                )), digits = self$digits
                            ),
                            round(
                                self$boot_norm_ci()$normal[2],
                                digits = self$digits),
                            round(
                                self$boot_basic_ci()$basic[4],
                                digits = self$digits),
                            round(
                                self$boot_perc_ci()$percent[4],
                                digits = self$digits),
                            round(
                                self$boot_bca_ci()$bca[4],
                                digits = self$digits)
                        ),
                        upper = c(
                            round(
                                (100 * exp(
                                    ((SciViews::ln((self$D()/self$S())) *
                                          self$ccc())) +
                                        (self$zzz() * (self$v()^(0.5)))
                                )), digits = self$digits
                            ),
                            round(
                                self$boot_norm_ci()$normal[3],
                                digits = self$digits),
                            round(
                                self$boot_basic_ci()$basic[5],
                                digits = self$digits),
                            round(
                                self$boot_perc_ci()$percent[5],
                                digits = self$digits),
                            round(
                                self$boot_bca_ci()$bca[5],
                                digits = self$digits)
                        ),
                        description = c(
                            "cqv with Bonett 95% CI",
                            "cqv with normal approximation 95% CI",
                            "cqv with basic bootstrap 95% CI",
                            "cqv with bootstrap percentile 95% CI",
                            "cqv with adjusted bootstrap percentile (BCa) 95% CI"
                        )
                    )
                )
            )
        }
        )
    )


