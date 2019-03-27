context("state_cv_versatile")
test_that(
    desc = "finds the correct findings", {
        x <- c(
            0.2, 0.5, 1.1, 1.4, 1.8, 2.3, 2.5, 2.7, 3.5, 4.4,
            4.6, 5.4, 5.4, 5.7, 5.8, 5.9, 6.0, 6.6, 7.1, 7.9
        )
        expect_equal(
            cv_versatile(
                x,
                method = "kelley",
                correction = FALSE
                )$method,
            "cv with Kelley 95% CI"
        )
        expect_equal(
            cv_versatile(
                x,
                method = "kelley",
                correction = FALSE
            )$statistics$est,
            57.8, tolerance = 0.0001
        )
        expect_equal(
            cv_versatile(
                x,
                method = "kelley",
                correction = FALSE
            )$statistics$lower,
            41.5, tolerance = 0.0001
        )
        expect_equal(
            cv_versatile(
                x,
                method = "kelley",
                correction = FALSE
            )$statistics$upper,
            98.5, tolerance = 0.0001
        )
        expect_equal(
            cv_versatile(
                x,
                method = "kelley",
                correction = TRUE
            )$method,
            "Corrected cv with Kelley 95% CI"
        )
        expect_equal(
            cv_versatile(
                x,
                method = "kelley",
                correction = TRUE
            )$statistics$est,
            58.1, tolerance = 0.0001
        )
        expect_equal(
            cv_versatile(
                x,
                method = "kelley",
                correction = TRUE
            )$statistics$lower,
            41.5, tolerance = 0.0001
        )
        expect_equal(
            cv_versatile(
                x,
                method = "kelley",
                correction = TRUE
            )$statistics$upper,
            98.5, tolerance = 0.0001
        )
        expect_equal(
            cv_versatile(
                x,
                method = "mckay",
                correction = FALSE
            )$method,
            "cv with McKay 95% CI"
        )
        expect_equal(
            cv_versatile(
                x,
                method = "mckay",
                correction = FALSE
            )$statistics$est,
            57.8, tolerance = 0.0001
        )
        expect_equal(
            cv_versatile(
                x,
                method = "mckay",
                correction = FALSE
            )$statistics$lower,
            41.4, tolerance = 0.0001
        )
        expect_equal(
            cv_versatile(
                x,
                method = "mckay",
                correction = FALSE
            )$statistics$upper,
            108.5, tolerance = 0.0001
        )
        expect_equal(
            cv_versatile(
                x,
                method = "mckay",
                correction = TRUE
            )$method,
            "Corrected cv with McKay 95% CI"
        )
        expect_equal(
            cv_versatile(
                x,
                method = "mckay",
                correction = TRUE
            )$statistics$est,
            58.1, tolerance = 0.0001
        )
        expect_equal(
            cv_versatile(
                x,
                method = "mckay",
                correction = TRUE
            )$statistics$lower,
            41.6, tolerance = 0.0001
        )
        expect_equal(
            cv_versatile(
                x,
                method = "mckay",
                correction = TRUE
            )$statistics$upper,
            109.4, tolerance = 0.0001
        )
        expect_equal(
            cv_versatile(
                x,
                method = "miller",
                correction = FALSE
            )$method,
            "cv with Miller 95% CI"
        )
        expect_equal(
            cv_versatile(
                x,
                method = "miller",
                correction = FALSE
            )$statistics$est,
            57.8, tolerance = 0.0001
        )
        expect_equal(
            cv_versatile(
                x,
                method = "miller",
                correction = FALSE
            )$statistics$lower,
            34.1, tolerance = 0.0001
        )
        expect_equal(
            cv_versatile(
                x,
                method = "miller",
                correction = FALSE
            )$statistics$upper,
            81.5, tolerance = 0.0001
        )
        expect_equal(
            cv_versatile(
                x,
                method = "miller",
                correction = TRUE
            )$method,
            "Corrected cv with Miller 95% CI"
        )
        expect_equal(
            cv_versatile(
                x,
                method = "miller",
                correction = TRUE
            )$statistics$est,
            58.1, tolerance = 0.0001
        )
        expect_equal(
            cv_versatile(
                x,
                method = "miller",
                correction = TRUE
            )$statistics$lower,
            34.2, tolerance = 0.0001
        )
        expect_equal(
            cv_versatile(
                x,
                method = "miller",
                correction = TRUE
            )$statistics$upper,
            81.9, tolerance = 0.0001
        )
        expect_equal(
            cv_versatile(
                x,
                method = "vangel",
                correction = FALSE
            )$method,
            "cv with Vangel 95% CI"
        )
        expect_equal(
            cv_versatile(
                x,
                method = "vangel",
                correction = FALSE
            )$statistics$est,
            57.8, tolerance = 0.0001
        )
        expect_equal(
            cv_versatile(
                x,
                method = "vangel",
                correction = FALSE
            )$statistics$lower,
            41.3, tolerance = 0.0001
        )
        expect_equal(
            cv_versatile(
                x,
                method = "vangel",
                correction = FALSE
            )$statistics$upper,
            105.4, tolerance = 0.0001
        )
        expect_equal(
            cv_versatile(
                x,
                method = "vangel",
                correction = TRUE
            )$method,
            "Corrected cv with Vangel 95% CI"
        )
        expect_equal(
            cv_versatile(
                x,
                method = "vangel",
                correction = TRUE
            )$statistics$est,
            58.1, tolerance = 0.0001
        )
        expect_equal(
            cv_versatile(
                x,
                method = "vangel",
                correction = TRUE
            )$statistics$lower,
            41.4, tolerance = 0.0001
        )
        expect_equal(
            cv_versatile(
                x,
                method = "vangel",
                correction = TRUE
            )$statistics$upper,
            106.2, tolerance = 0.0001
        )
        expect_equal(
            cv_versatile(
                x,
                method = "mahmoudvand_hassani",
                correction = FALSE
            )$method,
            "cv with Mahmoudvand-Hassani 95% CI"
        )
        expect_equal(
            cv_versatile(
                x,
                method = "mahmoudvand_hassani",
                correction = FALSE
            )$statistics$est,
            57.8, tolerance = 0.0001
        )
        expect_equal(
            cv_versatile(
                x,
                method = "mahmoudvand_hassani",
                correction = FALSE
            )$statistics$lower,
            43.5, tolerance = 0.0001
        )
        expect_equal(
            cv_versatile(
                x,
                method = "mahmoudvand_hassani",
                correction = FALSE
            )$statistics$upper,
            82.9, tolerance = 0.0001
        )
        expect_equal(
            cv_versatile(
                x,
                method = "mahmoudvand_hassani",
                correction = TRUE
            )$method,
            "Corrected cv with Mahmoudvand-Hassani 95% CI"
        )
        expect_equal(
            cv_versatile(
                x,
                method = "mahmoudvand_hassani",
                correction = TRUE
            )$statistics$est,
            58.1, tolerance = 0.0001
        )
        expect_equal(
            cv_versatile(
                x,
                method = "mahmoudvand_hassani",
                correction = TRUE
            )$statistics$lower,
            43.7, tolerance = 0.0001
        )
        expect_equal(
            cv_versatile(
                x,
                method = "mahmoudvand_hassani",
                correction = TRUE
            )$statistics$upper,
            83.3, tolerance = 0.0001
        )
        expect_equal(
            cv_versatile(
                x,
                method = "normal_approximation",
                correction = FALSE
            )$method,
            "cv with Normal Approximation 95% CI"
        )
        expect_equal(
            cv_versatile(
                x,
                method = "normal_approximation",
                correction = FALSE
            )$statistics$est,
            57.8, tolerance = 0.0001
        )
        expect_equal(
            cv_versatile(
                x,
                method = "normal_approximation",
                correction = FALSE
            )$statistics$lower,
            44.5, tolerance = 0.0001
        )
        expect_equal(
            cv_versatile(
                x,
                method = "normal_approximation",
                correction = FALSE
            )$statistics$upper,
            85.3, tolerance = 0.0001
        )
        expect_equal(
            cv_versatile(
                x,
                method = "normal_approximation",
                correction = TRUE
            )$method,
            "Corrected cv with Normal Approximation 95% CI"
        )
        expect_equal(
            cv_versatile(
                x,
                method = "normal_approximation",
                correction = TRUE
            )$statistics$est,
            58.1, tolerance = 0.0001
        )
        expect_equal(
            cv_versatile(
                x,
                method = "normal_approximation",
                correction = TRUE
            )$statistics$lower,
            44.8, tolerance = 0.0001
        )
        expect_equal(
            cv_versatile(
                x,
                method = "normal_approximation",
                correction = TRUE
            )$statistics$upper,
            85.7, tolerance = 0.0001
        )
        expect_equal(
            cv_versatile(
                x,
                method = "shortest_length",
                correction = FALSE
            )$method,
            "cv with Shortest-Length 95% CI"
        )
        expect_equal(
            cv_versatile(
                x,
                method = "shortest_length",
                correction = FALSE
            )$statistics$est,
            57.8, tolerance = 0.0001
        )
        expect_equal(
            cv_versatile(
                x,
                method = "shortest_length",
                correction = FALSE
            )$statistics$lower,
            42, tolerance = 0.0001
        )
        expect_equal(
            cv_versatile(
                x,
                method = "shortest_length",
                correction = FALSE
            )$statistics$upper,
            81, tolerance = 0.0001
        )
        expect_equal(
            cv_versatile(
                x,
                method = "shortest_length",
                correction = TRUE
            )$method,
            "Corrected cv with Shortest-Length 95% CI"
        )
        expect_equal(
            cv_versatile(
                x,
                method = "shortest_length",
                correction = TRUE
            )$statistics$est,
            58.1, tolerance = 0.0001
        )
        expect_equal(
            cv_versatile(
                x,
                method = "shortest_length",
                correction = TRUE
            )$statistics$lower,
            42.2, tolerance = 0.0001
        )
        expect_equal(
            cv_versatile(
                x,
                method = "shortest_length",
                correction = TRUE
            )$statistics$upper,
            81.4, tolerance = 0.0001
        )
        expect_equal(
            cv_versatile(
                x,
                method = "equal_tailed",
                correction = FALSE
            )$method,
            "cv with Equal-Tailed 95% CI"
        )
        expect_equal(
            cv_versatile(
                x,
                method = "equal_tailed",
                correction = FALSE
            )$statistics$est,
            57.8, tolerance = 0.0001
        )
        expect_equal(
            cv_versatile(
                x,
                method = "equal_tailed",
                correction = FALSE
            )$statistics$lower,
            43.9, tolerance = 0.0001
        )
        expect_equal(
            cv_versatile(
                x,
                method = "equal_tailed",
                correction = FALSE
            )$statistics$upper,
            84.4, tolerance = 0.0001
        )
        expect_equal(
            cv_versatile(
                x,
                method = "equal_tailed",
                correction = TRUE
            )$method,
            "Corrected cv with Equal-Tailed 95% CI"
        )
        expect_equal(
            cv_versatile(
                x,
                method = "equal_tailed",
                correction = TRUE
            )$statistics$est,
            58.1, tolerance = 0.0001
        )
        expect_equal(
            cv_versatile(
                x,
                method = "equal_tailed",
                correction = TRUE
            )$statistics$lower,
            44.2, tolerance = 0.0001
        )
        expect_equal(
            cv_versatile(
                x,
                method = "equal_tailed",
                correction = TRUE
            )$statistics$upper,
            84.8, tolerance = 0.0001
        )
        expect_equal(
            cv_versatile(
                x,
                method = "norm",
                correction = FALSE
            )$method,
            "cv with Normal Approximation Bootstrap 95% CI"
        )
        expect_equal(
            cv_versatile(
                x,
                method = "norm",
                correction = FALSE
            )$statistics$est,
            57.8, tolerance = 0.0001
        )
        expect_true(
            abs(
                cv_versatile(
                x,
                method = "norm",
                correction = FALSE
            )$statistics$lower -
            38.6
            )/100 < 0.05
        )
        expect_true(
            abs(
                cv_versatile(
                    x,
                    method = "norm",
                    correction = FALSE
                )$statistics$upper -
                    77.6
            )/100 < 0.05
        )
        expect_equal(
            cv_versatile(
                x,
                method = "norm",
                correction = TRUE
            )$method,
            "Corrected cv with Normal Approximation Bootstrap 95% CI"
        )
        expect_equal(
            cv_versatile(
                x,
                method = "norm",
                correction = TRUE
            )$statistics$est,
            58.1, tolerance = 0.0001
        )
        expect_true(
            abs(
                cv_versatile(
                    x,
                    method = "norm",
                    correction = TRUE
                )$statistics$lower -
                    38.6
            )/100 < 0.05
        )
        expect_true(
            abs(
                cv_versatile(
                    x,
                    method = "norm",
                    correction = TRUE
                )$statistics$upper -
                    77.6
            )/100 < 0.05
        )
        expect_equal(
            cv_versatile(
                x,
                method = "basic",
                correction = FALSE
            )$method,
            "cv with Basic Bootstrap 95% CI"
        )
        expect_equal(
            cv_versatile(
                x,
                method = "basic",
                correction = FALSE
            )$statistics$est,
            57.8, tolerance = 0.0001
        )
        expect_true(
            abs(
                cv_versatile(
                    x,
                    method = "basic",
                    correction = FALSE
                )$statistics$lower -
                    37.1
            )/100 < 0.05
        )
        expect_true(
            abs(
                cv_versatile(
                    x,
                    method = "basic",
                    correction = FALSE
                )$statistics$upper -
                    77
            )/100 < 0.05
        )
        expect_equal(
            cv_versatile(
                x,
                method = "basic",
                correction = TRUE
            )$method,
            "Corrected cv with Basic Bootstrap 95% CI"
        )
        expect_equal(
            cv_versatile(
                x,
                method = "basic",
                correction = TRUE
            )$statistics$est,
            58.1, tolerance = 0.0001
        )
        expect_true(
            abs(
                cv_versatile(
                    x,
                    method = "basic",
                    correction = TRUE
                )$statistics$lower -
                    38
            )/100 < 0.05
        )
        expect_true(
            abs(
                cv_versatile(
                    x,
                    method = "basic",
                    correction = TRUE
                )$statistics$upper -
                    77
            )/100 < 0.05
        )
        expect_error(
            cv_versatile(
                x,
                method = "perc",
                correction = FALSE
            ),
            "percent method is not developed yet"
        )

        expect_error(
            cv_versatile(
                x,
                method = "perc",
                correction = TRUE
            ),
            "percent method is not developed yet"
        )
        expect_error(
            cv_versatile(
                x,
                method = "bca",
                correction = FALSE
            ),
            "BCa method is not developed yet"
        )

        expect_error(
            cv_versatile(
                x,
                method = "bca",
                correction = TRUE
            ),
            "BCa method is not developed yet"
        )
        expect_equal(
            cv_versatile(
                x,
                method = "all",
                correction = TRUE
            )$method,
            "All methods"
        )
        expect_equal(
            nrow(cv_versatile(
                x,
                method = "all",
                correction = TRUE
            )$statistics),
            10
        )
        expect_equal(
            ncol(cv_versatile(
                x,
                method = "all",
                correction = TRUE
            )$statistics),
            4
        )
        expect_equal(
            cv_versatile(
                x,
                correction = FALSE
            )$method,
            "cv = sd/mean (may be biased)"
        )
        expect_equal(
            cv_versatile(
                x,
                correction = TRUE
            )$method,
            "Corrected (i.e., unbiased) cv"
        )
        expect_equal(
            cv_versatile(
                x,
                correction = FALSE
            )$statistics$est,
            57.8
        )
        expect_equal(
            cv_versatile(
                x,
                correction = TRUE
            )$statistics$est,
            58.1
        )
    }
)
