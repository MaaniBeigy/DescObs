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

