context("state_digits")
test_that(
    desc = "understands the default value of digits = 1", {
        x = rnorm(118, mean = 64, sd = 14.9)
        expect_equal(
            nchar(
                sub('.*\\.', '', CoefVar$new(x)$est())
            ),
            1
        )
        expect_equal(
            nchar(
                sub('.*\\.', '', CoefQuartVar$new(x)$est())
            ),
            1
        )
        expect_equal(
            nchar(
                sub(
                    '.*\\.', '',
                    CoefVarCI$new(x)$kelley_ci()$statistics$est
                )
            ),
            1
        )
        expect_equal(
            nchar(
                sub(
                    '.*\\.', '',
                    CoefQuartVarCI$new(x)$norm_ci()$statistics$est
                )
            ),
            1
        )
        expect_equal(
            nchar(
                sub(
                    '.*\\.', '',
                    SampleQuantiles$new(x, probs = 0.91)$qx()
                )
            ),
            1
        )
        expect_equal(
            nchar(
                sub(
                    '.*\\.', '',
                    cv(x, method = "kelley")$statistics$est
                )
            ),
            1
        )
        expect_equal(
            nchar(
                sub(
                    '.*\\.', '',
                    cqv(x, method = "bonett")$statistics$est
                )
            ),
            1
        )
    }
)
test_that(
    desc = "understands the user input of digits", {
        x = rnorm(118, mean = 64, sd = 14.9)
        expect_equal(
            nchar(
                sub('.*\\.', '', CoefVar$new(x, digits = 3)$est())
            ),
            3
        )
        expect_equal(
            nchar(
                sub('.*\\.', '', CoefQuartVar$new(x, digits = 3)$est())
            ),
            3
        )
        expect_equal(
            nchar(
                sub(
                    '.*\\.', '',
                    CoefVarCI$new(x, digits = 3)$kelley_ci()$statistics$est
                    )
            ),
            3
        )
        expect_equal(
            nchar(
                sub(
                    '.*\\.', '',
                    CoefQuartVarCI$new(x, digits = 3)$norm_ci()$statistics$est
                )
            ),
            3
        )
        expect_equal(
            nchar(
                sub(
                    '.*\\.', '',
                    SampleQuantiles$new(x, digits = 2, probs = 0.91)$qx()
                )
            ),
            2
        )
        expect_equal(
            nchar(
                sub(
                    '.*\\.', '',
                    cv(x, digits = 3, method = "kelley")$statistics$est
                )
            ),
            3
        )
        expect_equal(
            nchar(
                sub(
                    '.*\\.', '',
                    cqv(x, digits = 3, method = "bonett")$statistics$est
                )
            ),
            3
        )
    }
)
