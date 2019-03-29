#' @title Versatile Function for Removing Objects
#' @name rm_versatile
#' @aliases rm_versatile rm_pattern rm_except
#' @description Versatile function to remove objects.
#' @param save.objects an optional list naming objects to be saved (\emph{i.e.,}
#'                     not to be removed).
#' @param save.patterns an optional list of string patterns or
#'                      \link[base:regex]{regular expressions} which specifies
#'                      the objects to be saved (\emph{i.e.,} not to be removed)
#'                      .
#' @param rm.objects a list naming objects to be removed.
#' @param rm.patterns an optional list of string patterns or
#'                      \link[base:regex]{regular expressions}
#'                      which specifies the objects to be removed.
#' @param modes a list to set the type or storage mode of the save.patterns
#'              and/or rm.patterns. For example "logical", "integer", "double",
#'              "complex", "raw", "character", "list", "expression", "name",
#'              "symbol" and "function".
#' @param envir the \link[base]{environment} to use. Default is set to
#'              .GlobalEnv
#' @param inherits should the enclosing frames of the environment be inspected?
#' @details The traditional \link[base]{rm} function from \pkg{base} is not a
#'          versatile function because it cannot handle exceptional objects or
#'          string patterns to skip or remove. Moreover, \emph{rm} gives no
#'          warning before removing the objects and is not interactive. Also,
#'          the \emph{pattern} argument of \link[base]{ls} function cannot
#'          handle multiple patterns or multiple
#'          \link[base:regex]{regular expressions}. This function has been
#'          developed for sake of versatility, where you can determine
#'          exceptional objects and patterns for saving and/or removing objects.
#'          Also, you receive proper message regarding the final approval of
#'          removing objects. The search strategy of \code{rm_versatile} is
#'          based upon \link[utils]{apropos} function, which
#'          enables you to determine the type or storage mode of the objects
#'          (\emph{e.g.,} double, integer, character, \emph{etc.}).
#' @examples
#' # First let us making some objects
#' a <- 1
#' b <- "female"
#' c <- FALSE
#' d <- 4:10
#' e <- 5
#' f <- 6
#' g <- 7
#' df1 <- 8
#' df2 <- 9
#' dfcol <- 10
#' dff1 <- 11
#' dfff3 <- 12
#' mydata1 <- 13
#' mymydata <- 14
#' dataframe1 <- data.frame(
#'     gender = c("F", "M", "M", "F"),
#'     score = c(10, 12, 14, 18)
#' )
#' matrix.a <- matrix(1:10, nrow = 2, ncol = 5)
#' matrix2 <- matrix(1:100, ncol = 2)
#' x <- c("a", "b", "c", "d")
#' y <- c("d", "e", "f")
#' # Remove objects by rm.versatile
#' rm_versatile(
#'     save.objects = list("a", "b", "c"),
#'     save.patterns = list("df", "data"),
#'     rm.objects = list("x", "y"),
#'     rm.patterns = list("matrix")
#' )
#' # List objects
#' ls()
#' @export
#' @import dplyr SciViews boot R6 utils
NULL
#' @importFrom stats quantile sd qchisq qnorm
#' @importFrom MBESS conf.limits.nct
NULL
rm_versatile <- function(
    save.objects = NULL,
    save.patterns = NULL,
    rm.objects = NULL,
    rm.patterns = NULL,
    modes = list("integer", "double", "character", "list"),
    envir = .GlobalEnv,
    inherits = FALSE
) {
    if (is.null(save.objects)) {
        save.objects = NULL
    } else {
        save.objects = unlist(save.objects)
    }
    if (is.null(save.patterns)) {
        save.patterns = NULL
    } else {
        save.patterns = unlist(save.patterns)
    }
    if (is.null(rm.objects)) {
        rm.objects = NULL
    } else {
        rm.objects = unlist(rm.objects)
    }
    if (is.null(rm.patterns)) {
        rm.patterns = NULL
    } else {
        rm.patterns = unlist(rm.patterns)
    }

    mode <- match.arg(  # match the user's input with available methods
        arg = unlist(modes),
        choices = c(
            "logical", "integer", "double", "complex", "raw", "character",
            "list", "expression", "name", "symbol", "function"
        ),
        several.ok = TRUE
    )
    envir = envir
    inherits = inherits
    if (
        (is.null(save.patterns)) &
        (is.null(save.objects)) &
        (is.null(rm.objects)) &
        (is.null(rm.patterns))
    ) {
        stop("You have selected neither save nor remove objects/patterns!")
    } else if ((!is.null(save.patterns)) |
               (!is.null(save.objects))
    ) {
        if ("integer" %in% mode) {
            save.mode.integer <- unlist(lapply(
                save.patterns, apropos, mode = "integer")
            )
        } else if (!("integer" %in% mode)) {
            save.mode.integer <- NULL
        }
        if ("double" %in% mode) {
            save.mode.double <- unlist(lapply(
                save.patterns, apropos, mode = "double")
            )
        } else if (!("double" %in% mode)) {
            save.mode.double <- NULL
        }
        if ("character" %in% mode) {
            save.mode.character <- unlist(
                lapply(save.patterns, apropos, mode = "character")
            )
        } else if (!("character" %in% mode)) {
            save.mode.character <- NULL
        }
        if ("list" %in% mode) {
            save.mode.list <- unlist(
                lapply(save.patterns, apropos, mode = "list")
            )
        } else if (!("list" %in% mode)) {
            save.mode.list <- NULL
        }
        if ("logical" %in% mode) {
            save.mode.logical <- unlist(lapply(
                save.patterns, apropos, mode = "logical")
            )
        } else if (!("logical" %in% mode)) {
            save.mode.logical <- NULL
        }
        if ("complex" %in% mode) {
            save.mode.complex <- unlist(lapply(
                save.patterns, apropos, mode = "complex")
            )
        } else if (!("complex" %in% mode)) {
            save.mode.complex <- NULL
        }
        if ("raw" %in% mode) {
            save.mode.raw <- unlist(
                lapply(save.patterns, apropos, mode = "raw")
            )
        } else if (!("raw" %in% mode)) {
            save.mode.raw <- NULL
        }
        if ("expression" %in% mode) {
            save.mode.expression <- unlist(
                lapply(save.patterns, apropos, mode = "expression")
            )
        } else if (!("expression" %in% mode)) {
            save.mode.expression <- NULL
        }
        if ("name" %in% mode) {
            save.mode.name <- unlist(
                lapply(save.patterns, apropos, mode = "name")
            )
        } else if (!("name" %in% mode)) {
            save.mode.name <- NULL
        }
        if ("symbol" %in% mode) {
            save.mode.symbol <- unlist(
                lapply(save.patterns, apropos, mode = "symbol")
            )
        } else if (!("symbol" %in% mode)) {
            save.mode.symbol <- NULL
        }
        if ("function" %in% mode) {
            save.mode.function <- unlist(lapply(
                save.patterns, apropos, mode = "function")
            )
        } else if (!("function" %in% mode)) {
            save.mode.function <- NULL
        }
        save.formula = union(
            c(save.objects),
            c(save.mode.logical, save.mode.integer, save.mode.double,
              save.mode.complex, save.mode.raw, save.mode.character,
              save.mode.list, save.mode.expression, save.mode.name,
              save.mode.symbol, save.mode.function)
        )
        print(setdiff(ls(envir = envir), save.formula))
        ANSWER <- readline(
            "Are you a sure you want to remove these objects? [yes/no]"
        )
        if (substr(ANSWER, 1, 1) == "n") {
            message("OK, change your patterns and exceptional objects")
        } else {
            rm(
                list = setdiff(ls(envir = envir), save.formula),
                envir = envir,
                inherits = inherits
            )
            message("Done!")
        }
    } else if (
        ((!is.null(rm.objects)) |
        (!is.null(rm.patterns)))
    ) {
        if ("integer" %in% mode) {
            rm.mode.integer <- unlist(lapply(
                rm.patterns, apropos, mode = "integer")
            )
        } else if (!("integer" %in% mode)) {
            rm.mode.integer <- NULL
        }
        if ("double" %in% mode) {
            rm.mode.double <- unlist(lapply(
                rm.patterns, apropos, mode = "double")
            )
        } else if (!("double" %in% mode)) {
            rm.mode.double <- NULL
        }
        if ("character" %in% mode) {
            rm.mode.character <- unlist(
                lapply(rm.patterns, apropos, mode = "character")
            )
        } else if (!("character" %in% mode)) {
            rm.mode.character <- NULL
        }
        if ("list" %in% mode) {
            rm.mode.list <- unlist(
                lapply(rm.patterns, apropos, mode = "list")
            )
        } else if (!("list" %in% mode)) {
            rm.mode.list <- NULL
        }
        if ("logical" %in% mode) {
            rm.mode.logical <- unlist(lapply(
                rm.patterns, apropos, mode = "logical")
            )
        } else if (!("logical" %in% mode)) {
            rm.mode.logical <- NULL
        }
        if ("complex" %in% mode) {
            rm.mode.complex <- unlist(lapply(
                rm.patterns, apropos, mode = "complex")
            )
        } else if (!("complex" %in% mode)) {
            rm.mode.complex <- NULL
        }
        if ("raw" %in% mode) {
            rm.mode.raw <- unlist(
                lapply(rm.patterns, apropos, mode = "raw")
            )
        } else if (!("raw" %in% mode)) {
            rm.mode.raw <- NULL
        }
        if ("expression" %in% mode) {
            rm.mode.expression <- unlist(
                lapply(rm.patterns, apropos, mode = "expression")
            )
        } else if (!("expression" %in% mode)) {
            rm.mode.expression <- NULL
        }
        if ("name" %in% mode) {
            rm.mode.name <- unlist(
                lapply(rm.patterns, apropos, mode = "name")
            )
        } else if (!("name" %in% mode)) {
            rm.mode.name <- NULL
        }
        if ("symbol" %in% mode) {
            rm.mode.symbol <- unlist(
                lapply(rm.patterns, apropos, mode = "symbol")
            )
        } else if (!("symbol" %in% mode)) {
            rm.mode.symbol <- NULL
        }
        if ("function" %in% mode) {
            rm.mode.function <- unlist(lapply(
                rm.patterns, apropos, mode = "function")
            )
        } else if (!("function" %in% mode)) {
            rm.mode.function <- NULL
        }
        rm.formula = union(
            c(rm.objects),
            c(rm.mode.logical, rm.mode.integer, rm.mode.double,
              rm.mode.complex, rm.mode.raw, rm.mode.character,
              rm.mode.list, rm.mode.expression, rm.mode.name,
              rm.mode.symbol, rm.mode.function)
        )
        print(rm.formula)
        ANSWER <- readline(
            "Are you a sure you want to remove these objects? [yes/no]"
        )
        if (substr(ANSWER, 1, 1) == "n") {
            message("OK, change your patterns and remove objects")
        } else {
            rm(
                list = rm.formula,
                envir = envir,
                inherits = inherits
            )
            message("Done!")
        }
    } else if (
        ((!is.null(save.patterns)) |
         (!is.null(save.objects))) &
        ((!is.null(rm.objects)) |
         (!is.null(rm.patterns)))
    ) {
        if ("integer" %in% mode) {
            save.mode.integer <- unlist(lapply(
                save.patterns, apropos, mode = "integer")
            )
        } else if (!("integer" %in% mode)) {
            save.mode.integer <- NULL
        }
        if ("double" %in% mode) {
            save.mode.double <- unlist(lapply(
                save.patterns, apropos, mode = "double")
            )
        } else if (!("double" %in% mode)) {
            save.mode.double <- NULL
        }
        if ("character" %in% mode) {
            save.mode.character <- unlist(
                lapply(save.patterns, apropos, mode = "character")
            )
        } else if (!("character" %in% mode)) {
            save.mode.character <- NULL
        }
        if ("list" %in% mode) {
            save.mode.list <- unlist(
                lapply(save.patterns, apropos, mode = "list")
            )
        } else if (!("list" %in% mode)) {
            save.mode.list <- NULL
        }
        if ("logical" %in% mode) {
            save.mode.logical <- unlist(lapply(
                save.patterns, apropos, mode = "logical")
            )
        } else if (!("logical" %in% mode)) {
            save.mode.logical <- NULL
        }
        if ("complex" %in% mode) {
            save.mode.complex <- unlist(lapply(
                save.patterns, apropos, mode = "complex")
            )
        } else if (!("complex" %in% mode)) {
            save.mode.complex <- NULL
        }
        if ("raw" %in% mode) {
            save.mode.raw <- unlist(
                lapply(save.patterns, apropos, mode = "raw")
            )
        } else if (!("raw" %in% mode)) {
            save.mode.raw <- NULL
        }
        if ("expression" %in% mode) {
            save.mode.expression <- unlist(
                lapply(save.patterns, apropos, mode = "expression")
            )
        } else if (!("expression" %in% mode)) {
            save.mode.expression <- NULL
        }
        if ("name" %in% mode) {
            save.mode.name <- unlist(
                lapply(save.patterns, apropos, mode = "name")
            )
        } else if (!("name" %in% mode)) {
            save.mode.name <- NULL
        }
        if ("symbol" %in% mode) {
            save.mode.symbol <- unlist(
                lapply(save.patterns, apropos, mode = "symbol")
            )
        } else if (!("symbol" %in% mode)) {
            save.mode.symbol <- NULL
        }
        if ("function" %in% mode) {
            save.mode.function <- unlist(lapply(
                save.patterns, apropos, mode = "function")
            )
        } else if (!("function" %in% mode)) {
            save.mode.function <- NULL
        }
        save.formula = union(
            c(save.objects),
            c(save.mode.logical, save.mode.integer, save.mode.double,
              save.mode.complex, save.mode.raw, save.mode.character,
              save.mode.list, save.mode.expression, save.mode.name,
              save.mode.symbol, save.mode.function)
        )
        if ("integer" %in% mode) {
            rm.mode.integer <- unlist(lapply(
                rm.patterns, apropos, mode = "integer")
            )
        } else if (!("integer" %in% mode)) {
            rm.mode.integer <- NULL
        }
        if ("double" %in% mode) {
            rm.mode.double <- unlist(lapply(
                rm.patterns, apropos, mode = "double")
            )
        } else if (!("double" %in% mode)) {
            rm.mode.double <- NULL
        }
        if ("character" %in% mode) {
            rm.mode.character <- unlist(
                lapply(rm.patterns, apropos, mode = "character")
            )
        } else if (!("character" %in% mode)) {
            rm.mode.character <- NULL
        }
        if ("list" %in% mode) {
            rm.mode.list <- unlist(
                lapply(rm.patterns, apropos, mode = "list")
            )
        } else if (!("list" %in% mode)) {
            rm.mode.list <- NULL
        }
        if ("logical" %in% mode) {
            rm.mode.logical <- unlist(lapply(
                rm.patterns, apropos, mode = "logical")
            )
        } else if (!("logical" %in% mode)) {
            rm.mode.logical <- NULL
        }
        if ("complex" %in% mode) {
            rm.mode.complex <- unlist(lapply(
                rm.patterns, apropos, mode = "complex")
            )
        } else if (!("complex" %in% mode)) {
            rm.mode.complex <- NULL
        }
        if ("raw" %in% mode) {
            rm.mode.raw <- unlist(
                lapply(rm.patterns, apropos, mode = "raw")
            )
        } else if (!("raw" %in% mode)) {
            rm.mode.raw <- NULL
        }
        if ("expression" %in% mode) {
            rm.mode.expression <- unlist(
                lapply(rm.patterns, apropos, mode = "expression")
            )
        } else if (!("expression" %in% mode)) {
            rm.mode.expression <- NULL
        }
        if ("name" %in% mode) {
            rm.mode.name <- unlist(
                lapply(rm.patterns, apropos, mode = "name")
            )
        } else if (!("name" %in% mode)) {
            rm.mode.name <- NULL
        }
        if ("symbol" %in% mode) {
            rm.mode.symbol <- unlist(
                lapply(rm.patterns, apropos, mode = "symbol")
            )
        } else if (!("symbol" %in% mode)) {
            rm.mode.symbol <- NULL
        }
        if ("function" %in% mode) {
            rm.mode.function <- unlist(lapply(
                rm.patterns, apropos, mode = "function")
            )
        } else if (!("function" %in% mode)) {
            rm.mode.function <- NULL
        }
        rm.formula = union(
            c(rm.objects),
            c(rm.mode.logical, rm.mode.integer, rm.mode.double,
              rm.mode.complex, rm.mode.raw, rm.mode.character,
              rm.mode.list, rm.mode.expression, rm.mode.name,
              rm.mode.symbol, rm.mode.function)
        )
        print(union(setdiff(ls(envir = envir), save.formula), rm.formula))
        ANSWER <- readline(
            "Are you a sure you want to remove these objects? [yes/no]"
        )
        if (substr(ANSWER, 1, 1) == "n") {
            message("OK, change your patterns and objects")
        } else {
            rm(
                list = union(
                    setdiff(ls(envir = envir), save.formula),
                    rm.formula
                ),
                envir = envir,
                inherits = inherits
            )
            message("Done!")
        }
    }
}
