confint.cv <- function(x,alpha=.05, method="modmckay"){
    # Calculate the confidence interval of the cv of the vector x
    # Author: Kevin Wright
    # See: Vangel, Mark.  Confidence Intervals for a Normal Coefficient
    # of Variation. American Statistician, Vol 15, No1, p. 21--26.
    # x <- c(326,302,307,299,329)
    # confint.cv(x,.05,"modmckay")

    x <- na.omit(x)
    v <- length(x) - 1
    mu <- mean(x)
    sigma <- sqrt(var(x))
    k <- sigma/mu
    # CV > .33 may give poor results, so warn the user
    if ( k > 0.33) warning("Confidence interval may be very approximate.")

    method <- casefold(method) # In case we see "McKay"

    if (method == "mckay") {
        # McKay method.  See equation 15.
        t1 <- qchisq(1 - alpha/2,v)/v
        t2 <- qchisq(alpha/2,v)/v
        u1 <- v*t1
        u2 <- v*t2
        lower <- k/sqrt((u1/(v + 1) - 1 )*k*k + u1/v)
        upper <- k/sqrt((u2/(v + 1) - 1)*k*k + u2/v)
    } else if (method == "naive") {
        # Naive method.  See equation 17.
        t1 <- qchisq(1 - alpha/2,v)/v
        t2 <- qchisq(alpha/2,v)/v
        lower <- k/sqrt(t1)
        upper <- k/sqrt(t2)
    } else {
        # Modified McKay method. See equation 16.
        u1 <- qchisq(1 - alpha/2,v)
        u2 <- qchisq(alpha/2,v)
        lower <- k/sqrt(((u1 + 2)/(v + 1) - 1)*k*k + u1/v)
        upper <- k/sqrt(((u2 + 2)/(v + 1) - 1)*k*k + u2/v)
    }
    ci <- c(lower,upper)
    attr(ci,"CV") <- k
    attr(ci,"alpha") <- alpha
    return(ci)
}

library(DescTools)
CoefVar.default <- function(x, unbiased = FALSE, conf.level = NA, na.rm = FALSE, ...) {

    if (na.rm) {
        x <- na.omit(x)
        }

    res <- sd(x) / mean(x)
    n <- length(x)
    if (unbiased) {
        res <- res * ((1 - (1/(4*(n - 1))) + (1/n) * res^2) + (1/(2*(n - 1)^2)))
    }


    if (!is.na(conf.level)) {
        ci <- .nctCI(sqrt(n)/res, df = n - 1, conf = conf.level)
        res <- c(est = res, low.ci = unname(sqrt(n)/ci["upr.ci"]), upr.ci = unname(sqrt(n)/ci["lwr.ci"]))
    }
    return(res)
}
