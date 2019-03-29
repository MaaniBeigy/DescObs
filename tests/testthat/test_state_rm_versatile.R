context("state_rm_versatile")
test_that(
    desc = "understands the default values", {
        expect_null(
            formals(rm_versatile)$save.objects
        )
        expect_null(
            formals(rm_versatile)$save.patterns
        )
        expect_null(
            formals(rm_versatile)$rm.objects
        )
        expect_null(
            formals(rm_versatile)$rm.patterns
        )
        expect_false(
            formals(rm_versatile)$inherits
        )
    }
)
test_that(
    desc = "warn if empty", {
        expect_error(
            rm_versatile(),
            "You have selected neither save nor remove objects/patterns!"
        )
    }
)
test_that(
    desc = "correct remove by both remove and save", {
        a <- 1
        b <- "female"
        c <- FALSE
        d <- 4:10
        e <- 5
        f <- 6
        g <- 7
        df1 <- 8
        df2 <- 9
        dfcol <- 10
        dff1 <- 11
        dfff3 <- 12
        mydata1 <- 13
        mymydata <- 14
        dataframe1 <- data.frame(
            gender = c("F", "M", "M", "F"),
            score = c(10, 12, 14, 18)
        )
        matrix.a <- matrix(1:10, nrow = 2, ncol = 5)
        matrix2 <- matrix(1:100, ncol = 2)
        x <- c("a", "b", "c", "d")
        y <- c("d", "e", "f")
        expect_message(
            rm_versatile(
                save.objects = list("a", "b", "c"),
                save.patterns = list("df", "data"),
                rm.objects = list("x", "y"),
                rm.patterns = list("matrix")
                # ,
                # modes = list(
                #     "logical", "integer", "double", "complex", "raw",
                #     "character", "list", "expression", "name",
                #     "symbol", "function"
                # )
            ),
            "Done!"
        )
    }
)
test_that(
    desc = "correct remove by remove", {
        a <- 1
        b <- "female"
        c <- FALSE
        d <- 4:10
        e <- 5
        f <- 6
        g <- 7
        df1 <- 8
        df2 <- 9
        dfcol <- 10
        dff1 <- 11
        dfff3 <- 12
        mydata1 <- 13
        mymydata <- 14
        dataframe1 <- data.frame(
            gender = c("F", "M", "M", "F"),
            score = c(10, 12, 14, 18)
        )
        matrix.a <- matrix(1:10, nrow = 2, ncol = 5)
        matrix2 <- matrix(1:100, ncol = 2)
        x <- c("a", "b", "c", "d")
        y <- c("d", "e", "f")
        expect_message(
            rm_versatile(
                rm.objects = list("x", "y"),
                rm.patterns = list("matrix")
            ),
            "Done!"
        )
    }
)
test_that(
    desc = "correct remove by save", {
        a <- 1
        b <- "female"
        c <- FALSE
        d <- 4:10
        e <- 5
        f <- 6
        g <- 7
        df1 <- 8
        df2 <- 9
        dfcol <- 10
        dff1 <- 11
        dfff3 <- 12
        mydata1 <- 13
        mymydata <- 14
        dataframe1 <- data.frame(
            gender = c("F", "M", "M", "F"),
            score = c(10, 12, 14, 18)
        )
        matrix.a <- matrix(1:10, nrow = 2, ncol = 5)
        matrix2 <- matrix(1:100, ncol = 2)
        x <- c("a", "b", "c", "d")
        y <- c("d", "e", "f")
        expect_message(
            rm_versatile(
                save.objects = list("a", "b", "c"),
                save.patterns = list("df", "data")
            ),
            "Done!"
        )
    }
)
test_that(
    desc = "correct remove multiple df with modes", {
        id <- c(1,2,3,4)
        gender <- c("male","female","female","male")
        time <- c(2018,2019,2018,2017)
        df <- data.frame(id, gender, time)
        myvalues <- c(2017,2018,2019)
        for (i in 1:100) {
            assign(paste0("dfy",i), df[df$time == myvalues[i],])
        }
        a <- 1
        b <- "female"
        c <- FALSE
        d <- 4:10
        e <- 5
        f <- 6
        g <- 7
        df1 <- 8
        df2 <- 9
        dfcol <- 10
        dff1 <- 11
        dfff3 <- 12
        mydata1 <- 13
        mymydata <- 14
        dataframe1 <- data.frame(
            gender = c("F", "M", "M", "F"),
            score = c(10, 12, 14, 18)
        )
        matrix.a <- matrix(1:10, nrow = 2, ncol = 5)
        matrix2 <- matrix(1:100, ncol = 2)
        x <- c("a", "b", "c", "d")
        y <- c("d", "e", "f")
        expect_message(
            rm_versatile(
                rm.patterns = list("df", "matrix", "dataframe"),
                modes = list("list")
            ),
            "Done!"
        )
    }
)
test_that(
    desc = "correct remove by save 2", {
        b <- "female"
        # c <- FALSE
        x <- c("a", "b", "c", "d")
        y <- c("d", "e", "f")
        expect_message(
            rm_versatile(
                save.objects = list("b"),
                save.patterns = list("x"),
                modes = list("character")
            ),
            "Done!"
        )
    }
)
test_that(
    desc = "correct remove by save 3", {
        b <- 1
        c <- 2
        x <- c(10, 12.5, 6.1, 8.4)
        y <- c(3.4, 7.8, 2.2)
        expect_message(
            rm_versatile(
                save.objects = list("b"),
                save.patterns = list("x"),
                modes = list("double")
            ),
            "Done!"
        )
    }
)
test_that(
    desc = "correct remove by save 4", {
        b <- 1L
        c <- 2L
        x <- c(10L, 12L, 6L, 8L)
        y <- c(3L, 7L, 2L)
        expect_message(
            rm_versatile(
                save.objects = list("b"),
                save.patterns = list("x"),
                modes = list("integer")
            ),
            "Done!"
        )
    }
)
test_that(
    desc = "correct remove by save 5", {
        b <- raw(2)
        c <- raw(3)
        x <- raw(4)
        y <- raw(5)
        expect_message(
            rm_versatile(
                save.objects = list("b"),
                save.patterns = list("x"),
                modes = list("raw")
            ),
            "Done!"
        )
    }
)
test_that(
    desc = "correct remove by save 6", {
        b <- 0i
        c <- 1i
        x <- 2i
        y <- 3i
        expect_message(
            rm_versatile(
                save.objects = list("b"),
                save.patterns = list("x"),
                modes = list("complex")
            ),
            "Done!"
        )
    }
)
test_that(
    desc = "correct remove by save 7", {
        b <- list(1, 2)
        c <- list(3)
        x <- list(4)
        y <- list(5)
        expect_message(
            rm_versatile(
                save.objects = list("b"),
                save.patterns = list("x"),
                modes = list("list")
            ),
            "Done!"
        )
    }
)
test_that(
    desc = "correct remove by save 8", {
        b <- as.name("arrg")
        c <- as.name("arrrg")
        x <- as.name("arrrrg")
        y <- as.name("arrrrrg")
        expect_message(
            rm_versatile(
                save.objects = list("b"),
                save.patterns = list("x"),
                modes = list("name")
            ),
            "Done!"
        )
    }
)
test_that(
    desc = "correct remove by save 9", {
        b <- c(TRUE)
        c <- c(FALSE)
        x <- c(TRUE, FALSE)
        y <- c(TRUE, FALSE, TRUE, FALSE)
        expect_message(
            rm_versatile(
                save.objects = list("b"),
                save.patterns = list("x"),
                modes = list("logical")
            ),
            "Done!"
        )
    }
)
test_that(
    desc = "correct remove by save 10", {
        b <- expression(1 + 0:9)
        c <- expression(2 + 0:9)
        x <- expression(3 + 0:9)
        y <- expression(4 + 0:9)
        expect_message(
            rm_versatile(
                save.objects = list("b"),
                save.patterns = list("x"),
                modes = list("expression")
            ),
            "Done!"
        )
    }
)
test_that(
    desc = "correct remove by save 11", {
        b <- function(x) {print(x)}
        cc <- function(x) {print(x^2)}
        x <- function(x) {print(x*2)}
        yyyyyyyyyyyy <- function(x) {print(x/2)}
        expect_message(
            rm_versatile(
                save.objects = list("b"),
                save.patterns = list("yyyyyyyyyyyy"),
                modes = list("function")
            ),
            "Done!"
        )
    }
)
test_that(
    desc = "correct remove by save 12", {
        b <- as.symbol("arrgr")
        cc <- as.symbol("arrrgr")
        x <- as.symbol("arrrrgr")
        yyyyyyyyyyyy <- as.symbol("arrrrrgr")
        expect_message(
            rm_versatile(
                save.objects = list("b", "rm_versatile"),
                save.patterns = list("yyyyyyyyyyyy"),
                modes = list("symbol")
            ),
            "Done!"
        )
    }
)
test_that(
    desc = "correct remove by rm 2", {
        b <- "female"
        # c <- FALSE
        x <- c("a", "b", "c", "d")
        y <- c("d", "e", "f")
        expect_message(
            rm_versatile(
                rm.objects = list("b"),
                rm.patterns = list("x"),
                modes = list("character")
            ),
            "Done!"
        )
    }
)
test_that(
    desc = "correct remove by rm 3", {
        b <- 1
        c <- 2
        x <- c(10, 12.5, 6.1, 8.4)
        y <- c(3.4, 7.8, 2.2)
        expect_message(
            rm_versatile(
                rm.objects = list("b"),
                rm.patterns = list("x"),
                modes = list("double")
            ),
            "Done!"
        )
    }
)
test_that(
    desc = "correct remove by rm 4", {
        b <- 1L
        c <- 2L
        x <- c(10L, 12L, 6L, 8L)
        y <- c(3L, 7L, 2L)
        expect_message(
            rm_versatile(
                rm.objects = list("b"),
                rm.patterns = list("x"),
                modes = list("integer")
            ),
            "Done!"
        )
    }
)
test_that(
    desc = "correct remove by rm 5", {
        b <- raw(2)
        c <- raw(3)
        x <- raw(4)
        y <- raw(5)
        expect_message(
            rm_versatile(
                rm.objects = list("b"),
                rm.patterns = list("x"),
                modes = list("raw")
            ),
            "Done!"
        )
    }
)
test_that(
    desc = "correct remove by rm 6", {
        b <- 0i
        c <- 1i
        x <- 2i
        y <- 3i
        expect_message(
            rm_versatile(
                rm.objects = list("b"),
                rm.patterns = list("x"),
                modes = list("complex")
            ),
            "Done!"
        )
    }
)
test_that(
    desc = "correct remove by rm 7", {
        b <- list(1, 2)
        c <- list(3)
        x <- list(4)
        y <- list(5)
        expect_message(
            rm_versatile(
                rm.objects = list("b"),
                rm.patterns = list("x"),
                modes = list("list")
            ),
            "Done!"
        )
    }
)
test_that(
    desc = "correct remove by rm 8", {
        b <- as.name("arrg")
        c <- as.name("arrrg")
        x <- as.name("arrrrg")
        y <- as.name("arrrrrg")
        expect_message(
            rm_versatile(
                rm.objects = list("b"),
                rm.patterns = list("x"),
                modes = list("name")
            ),
            "Done!"
        )
    }
)
test_that(
    desc = "correct remove by rm 9", {
        b <- c(TRUE)
        c <- c(FALSE)
        x <- c(TRUE, FALSE)
        y <- c(TRUE, FALSE, TRUE, FALSE)
        expect_message(
            rm_versatile(
                rm.objects = list("b"),
                rm.patterns = list("x"),
                modes = list("logical")
            ),
            "Done!"
        )
    }
)
test_that(
    desc = "correct remove by rm 10", {
        b <- expression(1 + 0:9)
        c <- expression(2 + 0:9)
        x <- expression(3 + 0:9)
        y <- expression(4 + 0:9)
        expect_message(
            rm_versatile(
                rm.objects = list("b"),
                rm.patterns = list("x"),
                modes = list("expression")
            ),
            "Done!"
        )
    }
)
test_that(
    desc = "correct remove by rm 11", {
        b <- function(x) {print(x)}
        c <- function(x) {print(x^2)}
        x <- function(x) {print(x*2)}
        yyyyyyyyyyyy <- function(x) {print(x/2)}
        expect_message(
            rm_versatile(
                rm.objects = list("b"),
                rm.patterns = list("yyyyyyyyyyyy"),
                modes = list("function")
            ),
            "Done!"
        )
    }
)
test_that(
    desc = "correct remove by rm 12", {
        b <- as.symbol("arrgr")
        cc <- as.symbol("arrrgr")
        x <- as.symbol("arrrrgr")
        yyyyyyyyyyyy <- as.symbol("arrrrrgr")
        expect_message(
            rm_versatile(
                rm.objects = list("b"),
                rm.patterns = list("yyyyyyyyyyyy"),
                modes = list("symbol")
            ),
            "Done!"
        )
    }
)
test_that(
    desc = "correct remove by both remove and save 2", {
        a <- 1
        b <- function(x) {print(x)}
        cx <- as.symbol("arrrgr")
        d <- expression(1 + 0:9)
        e <- c(TRUE)
        f <- as.name("arrrrg")
        g <- list(1, 2)
        x <- 1i
        y <- raw(3)
        dfcol <- 10L
        dff1 <- "female"
        data1 <- matrix(1:10, nrow = 2, ncol = 5)
        expect_message(
            rm_versatile(
                save.objects = list("a", "b", "d"),
                save.patterns = list("df"),
                rm.objects = list("x", "y"),
                rm.patterns = list("data"),
                modes = list(
                    "logical", "integer", "double", "complex", "raw",
                    "character", "list", "expression", "name",
                    "symbol", "function"
                )
            ),
            "Done!"
        )
    }
)

