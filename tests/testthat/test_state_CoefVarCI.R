context("state_CoefVarCI")
test_that(
    desc = "understands the default value of correction = FALSE", {
        x <- c(
            0.2, 0.5, 1.1, 1.4, 1.8, 2.3, 2.5, 2.7, 3.5, 4.4,
            4.6, 5.4, 5.4, 5.7, 5.8, 5.9, 6.0, 6.6, 7.1, 7.9
        )
        expect_false(
            CoefVarCI$new(x)$correction
        )
    }
)
test_that(
    desc = "understands the user input value of correction = TRUE", {
        x <- c(
            0.2, 0.5, 1.1, 1.4, 1.8, 2.3, 2.5, 2.7, 3.5, 4.4,
            4.6, 5.4, 5.4, 5.7, 5.8, 5.9, 6.0, 6.6, 7.1, 7.9
        )
        expect_true(
            CoefVarCI$new(x, correction = TRUE)$correction
        )
    }
)
test_that(
    desc = "understands the user input value of correction = FALSE", {
        x <- c(
            0.2, 0.5, 1.1, 1.4, 1.8, 2.3, 2.5, 2.7, 3.5, 4.4,
            4.6, 5.4, 5.4, 5.7, 5.8, 5.9, 6.0, 6.6, 7.1, 7.9
        )
        expect_false(
            CoefVarCI$new(x, correction = FALSE)$correction
        )
    }
)
test_that(
    desc = "finds the correct findings", {
        x <- c(
            0.2, 0.5, 1.1, 1.4, 1.8, 2.3, 2.5, 2.7, 3.5, 4.4,
            4.6, 5.4, 5.4, 5.7, 5.8, 5.9, 6.0, 6.6, 7.1, 7.9
        )
        expect_equal(
            CoefVarCI$new(x)$ncp_cv(),
            7.737259,
            tolerance = 0.000001
        )
        expect_equal(
            CoefVarCI$new(x)$ncp_cv_corr(),
            7.697308,
            tolerance = 0.000001
        )
        expect_equal(
            CoefVarCI$new(x)$v(),
            19
        )
        expect_equal(
            CoefVarCI$new(x)$kelley_nctci_cv()$Lower.Limit,
            4.565712,
            tolerance = 0.000001
        )
        expect_equal(
            CoefVarCI$new(x)$kelley_nctci_cv()$Upper.Limit,
            10.82754,
            tolerance = 0.00001
        )
        expect_equal(
            CoefVarCI$new(x)$kelley_nctci_cv_corr()$Lower.Limit,
            4.535534,
            tolerance = 0.000001
        )
        expect_equal(
            CoefVarCI$new(x)$kelley_nctci_cv_corr()$Upper.Limit,
            10.77774,
            tolerance = 0.00001
        )
        expect_equal(
            CoefVarCI$new(x)$lower_tile_kelley_cv(),
            0.4130336,
            tolerance = 0.0000001
        )
        expect_equal(
            CoefVarCI$new(x)$upper_tile_kelley_cv(),
            0.9795047,
            tolerance = 0.0000001
        )
        expect_equal(
            CoefVarCI$new(x)$lower_tile_kelley_cv_corr(),
            0.4149419,
            tolerance = 0.0000001
        )
        expect_equal(
            CoefVarCI$new(x)$upper_tile_kelley_cv_corr(),
            0.9860219,
            tolerance = 0.0000001
        )
        expect_equal(
            CoefVarCI$new(x)$t1_mckay(),
            1.72907,
            tolerance = 0.00001
        )
        expect_equal(
            CoefVarCI$new(x)$t2_mckay(),
            0.468764,
            tolerance = 0.000001
        )
        expect_equal(
            CoefVarCI$new(x)$u1_mckay(),
            32.85233,
            tolerance = 0.00001
        )
        expect_equal(
            CoefVarCI$new(x)$u2_mckay(),
            8.906516,
            tolerance = 0.000001
        )
        expect_equal(
            CoefVarCI$new(x)$lower_tile_mckay_cv(),
            0.4145785,
            tolerance = 0.0000001
        )
        expect_equal(
            CoefVarCI$new(x)$upper_tile_mckay_cv(),
            1.085637,
            tolerance = 0.000001
        )
        expect_equal(
            CoefVarCI$new(x)$lower_tile_mckay_cv_corr(),
            0.416491,
            tolerance = 0.000001
        )
        expect_equal(
            CoefVarCI$new(x)$upper_tile_mckay_cv_corr(),
            1.095004,
            tolerance = 0.000001
        )
        expect_equal(
            CoefVarCI$new(x)$z_miller(),
            1.959964,
            tolerance = 0.000001
        )
        expect_equal(
            CoefVarCI$new(x)$u_miller_cv(),
            0.1211033,
            tolerance = 0.0000001
        )
        expect_equal(
            CoefVarCI$new(x)$u_miller_cv_corr(),
            0.1219853,
            tolerance = 0.0000001
        )
        expect_equal(
            CoefVarCI$new(x)$zu_miller_cv(),
            0.2373581,
            tolerance = 0.0000001
        )
        expect_equal(
            CoefVarCI$new(x)$zu_miller_cv_corr(),
            0.2390868,
            tolerance = 0.0000001
        )
        expect_equal(
            CoefVarCI$new(x)$lower_tile_miller_cv(),
            0.3406419,
            tolerance = 0.0000001
        )
        expect_equal(
            CoefVarCI$new(x)$upper_tile_miller_cv(),
            0.8153581,
            tolerance = 0.0000001
        )
        expect_equal(
            CoefVarCI$new(x)$lower_tile_miller_cv_corr(),
            0.3419132,
            tolerance = 0.0000001
        )
        expect_equal(
            CoefVarCI$new(x)$upper_tile_miller_cv_corr(),
            0.8200868,
            tolerance = 0.0000001
        )
        expect_equal(
            CoefVarCI$new(x)$t1_vangel(),
            1.72907,
            tolerance = 0.00001
        )
        expect_equal(
            CoefVarCI$new(x)$t2_vangel(),
            0.468764,
            tolerance = 0.000001
        )
        expect_equal(
            CoefVarCI$new(x)$u1_vangel(),
            32.85233,
            tolerance = 0.00001
        )
        expect_equal(
            CoefVarCI$new(x)$u2_vangel(),
            8.906516,
            tolerance = 0.000001
        )
        expect_equal(
            CoefVarCI$new(x)$lower_tile_vangel_cv(),
            0.4128085,
            tolerance = 0.0000001
        )
        expect_equal(
            CoefVarCI$new(x)$upper_tile_vangel_cv(),
            1.054997,
            tolerance = 0.000001
        )
        expect_equal(
            CoefVarCI$new(x)$lower_tile_vangel_cv_corr(),
            0.4146965,
            tolerance = 0.0000001
        )
        expect_equal(
            CoefVarCI$new(x)$upper_tile_vangel_cv_corr(),
            1.063586,
            tolerance = 0.000001
        )
        expect_equal(
            CoefVarCI$new(x)$cn_mh(),
            0.9869343,
            tolerance = 0.0000001
        )
        expect_equal(
            CoefVarCI$new(x)$ul_mh(),
            1.328862,
            tolerance = 0.000001
        )
        expect_equal(
            CoefVarCI$new(x)$uu_mh(),
            0.6972697,
            tolerance = 0.0000001
        )
        expect_equal(
            CoefVarCI$new(x)$lower_tile_mh_cv(),
            0.4349587,
            tolerance = 0.0000001
        )
        expect_equal(
            CoefVarCI$new(x)$upper_tile_mh_cv(),
            0.8289476,
            tolerance = 0.0000001
        )
        expect_equal(
            CoefVarCI$new(x)$lower_tile_mh_cv_corr(),
            0.4372163,
            tolerance = 0.0000001
        )
        expect_equal(
            CoefVarCI$new(x)$upper_tile_mh_cv_corr(),
            0.8332501,
            tolerance = 0.0000001
        )
        expect_equal(
            CoefVarCI$new(x)$cn_normaapprox(),
            0.9874209,
            tolerance = 0.0000001
        )
        expect_equal(
            CoefVarCI$new(x)$ul_normaapprox(),
            1.297318,
            tolerance = 0.000001
        )
        expect_equal(
            CoefVarCI$new(x)$uu_normaapprox(),
            0.6775234,
            tolerance = 0.0000001
        )
        expect_equal(
            CoefVarCI$new(x)$lower_tile_normaapprox_cv(),
            0.4455344,
            tolerance = 0.0000001
        )
        expect_equal(
            CoefVarCI$new(x)$upper_tile_normaapprox_cv(),
            0.8531071,
            tolerance = 0.0000001
        )
        expect_equal(
            CoefVarCI$new(x)$lower_tile_normaapprox_cv_corr(),
            0.4478469,
            tolerance = 0.0000001
        )
        expect_equal(
            CoefVarCI$new(x)$upper_tile_normaapprox_cv_corr(),
            0.857535,
            tolerance = 0.000001
        )
        expect_equal(
            unname(CoefVarCI$new(x)$a_value()$a),
            9.6629,
            tolerance = 0.0001
        )
        expect_equal(
            unname(CoefVarCI$new(x)$b_value()$b),
            35.9266,
            tolerance = 0.0001
        )
        expect_equal(
            CoefVarCI$new(x)$lower_tile_shortest_cv(),
            0.420336,
            tolerance = 0.000001
        )
        expect_equal(
            CoefVarCI$new(x)$upper_tile_shortest_cv(),
            0.810496,
            tolerance = 0.000001
        )
        expect_equal(
            CoefVarCI$new(x)$lower_tile_shortest_cv_corr(),
            0.4225177,
            tolerance = 0.0000001
        )
        expect_equal(
            CoefVarCI$new(x)$upper_tile_shortest_cv_corr(),
            0.8147028,
            tolerance = 0.0000001
        )
        expect_equal(
            CoefVarCI$new(x)$t1_equal(),
            32.85233,
            tolerance = 0.00001
        )
        expect_equal(
            CoefVarCI$new(x)$t2_equal(),
            8.906516,
            tolerance = 0.000001
        )
        expect_equal(
            CoefVarCI$new(x)$lower_tile_equal_cv(),
            0.4395634,
            tolerance = 0.0000001
        )
        expect_equal(
            CoefVarCI$new(x)$upper_tile_equal_cv(),
            0.8442104,
            tolerance = 0.0000001
        )
        expect_equal(
            CoefVarCI$new(x)$lower_tile_equal_cv_corr(),
            0.4418449,
            tolerance = 0.0000001
        )
        expect_equal(
            CoefVarCI$new(x)$upper_tile_equal_cv_corr(),
            0.8485921,
            tolerance = 0.0000001
        )
        expect_equal(
            CoefVarCI$new(x, correction = FALSE)$kelley_ci()$method,
            "cv with Kelley CI"
        )
        expect_equal(
            CoefVarCI$new(x, correction = FALSE)$kelley_ci()$statistics$est,
            57.8,
            tolerance = 0.0001
        )
        expect_equal(
            CoefVarCI$new(x, correction = FALSE)$kelley_ci()$statistics$lower,
            41.3,
            tolerance = 0.0001
        )
        expect_equal(
            CoefVarCI$new(x, correction = FALSE)$kelley_ci()$statistics$upper,
            98,
            tolerance = 0.0001
        )
        expect_equal(
            CoefVarCI$new(x, correction = TRUE)$kelley_ci()$method,
            "corrected cv with Kelley CI"
        )
        expect_equal(
            CoefVarCI$new(x, correction = TRUE)$kelley_ci()$statistics$est,
            58.1,
            tolerance = 0.0001
        )
        expect_equal(
            CoefVarCI$new(x, correction = TRUE)$kelley_ci()$statistics$lower,
            41.5,
            tolerance = 0.0001
        )
        expect_equal(
            CoefVarCI$new(x, correction = TRUE)$kelley_ci()$statistics$upper,
            98.6,
            tolerance = 0.0001
        )
        expect_equal(
            CoefVarCI$new(x, correction = FALSE)$mckay_ci()$method,
            "cv with McKay CI"
        )
        expect_equal(
            CoefVarCI$new(x, correction = FALSE)$mckay_ci()$statistics$est,
            57.8,
            tolerance = 0.0001
        )
        expect_equal(
            CoefVarCI$new(x, correction = FALSE)$mckay_ci()$statistics$lower,
            41.5,
            tolerance = 0.0001
        )
        expect_equal(
            CoefVarCI$new(x, correction = FALSE)$mckay_ci()$statistics$upper,
            108.6,
            tolerance = 0.0001
        )
        expect_equal(
            CoefVarCI$new(x, correction = TRUE)$mckay_ci()$method,
            "corrected cv with McKay CI"
        )
        expect_equal(
            CoefVarCI$new(x, correction = TRUE)$mckay_ci()$statistics$est,
            58.1,
            tolerance = 0.0001
        )
        expect_equal(
            CoefVarCI$new(x, correction = TRUE)$mckay_ci()$statistics$lower,
            41.6,
            tolerance = 0.0001
        )
        expect_equal(
            CoefVarCI$new(x, correction = TRUE)$mckay_ci()$statistics$upper,
            109.5,
            tolerance = 0.0001
        )
        expect_equal(
            CoefVarCI$new(x, correction = FALSE)$miller_ci()$method,
            "cv with Miller CI"
        )
        expect_equal(
            CoefVarCI$new(x, correction = FALSE)$miller_ci()$statistics$est,
            57.8,
            tolerance = 0.0001
        )
        expect_equal(
            CoefVarCI$new(x, correction = FALSE)$miller_ci()$statistics$lower,
            34.1,
            tolerance = 0.0001
        )
        expect_equal(
            CoefVarCI$new(x, correction = FALSE)$miller_ci()$statistics$upper,
            81.5,
            tolerance = 0.0001
        )
        expect_equal(
            CoefVarCI$new(x, correction = TRUE)$miller_ci()$method,
            "corrected cv with Miller CI"
        )
        expect_equal(
            CoefVarCI$new(x, correction = TRUE)$miller_ci()$statistics$est,
            58.1,
            tolerance = 0.0001
        )
        expect_equal(
            CoefVarCI$new(x, correction = TRUE)$miller_ci()$statistics$lower,
            34.2,
            tolerance = 0.0001
        )
        expect_equal(
            CoefVarCI$new(x, correction = TRUE)$miller_ci()$statistics$upper,
            82,
            tolerance = 0.0001
        )
        expect_equal(
            CoefVarCI$new(x, correction = FALSE)$vangel_ci()$method,
            "cv with Vangel CI"
        )
        expect_equal(
            CoefVarCI$new(x, correction = FALSE)$vangel_ci()$statistics$est,
            57.8,
            tolerance = 0.0001
        )
        expect_equal(
            CoefVarCI$new(x, correction = FALSE)$vangel_ci()$statistics$lower,
            41.3,
            tolerance = 0.0001
        )
        expect_equal(
            CoefVarCI$new(x, correction = FALSE)$vangel_ci()$statistics$upper,
            105.5,
            tolerance = 0.0001
        )
        expect_equal(
            CoefVarCI$new(x, correction = TRUE)$vangel_ci()$method,
            "corrected cv with Vangel CI"
        )
        expect_equal(
            CoefVarCI$new(x, correction = TRUE)$vangel_ci()$statistics$est,
            58.1,
            tolerance = 0.0001
        )
        expect_equal(
            CoefVarCI$new(x, correction = TRUE)$vangel_ci()$statistics$lower,
            41.5,
            tolerance = 0.0001
        )
        expect_equal(
            CoefVarCI$new(x, correction = TRUE)$vangel_ci()$statistics$upper,
            106.4,
            tolerance = 0.0001
        )
        expect_equal(
            CoefVarCI$new(x, correction = FALSE)$mh_ci()$method,
            "cv with Mahmoudvand-Hassani CI"
        )
        expect_equal(
            CoefVarCI$new(x, correction = FALSE)$mh_ci()$statistics$est,
            57.8,
            tolerance = 0.0001
        )
        expect_equal(
            CoefVarCI$new(x, correction = FALSE)$mh_ci()$statistics$lower,
            43.5,
            tolerance = 0.0001
        )
        expect_equal(
            CoefVarCI$new(x, correction = FALSE)$mh_ci()$statistics$upper,
            82.9,
            tolerance = 0.0001
        )
        expect_equal(
            CoefVarCI$new(x, correction = TRUE)$mh_ci()$method,
            "corrected cv with Mahmoudvand-Hassani CI"
        )
        expect_equal(
            CoefVarCI$new(x, correction = TRUE)$mh_ci()$statistics$est,
            58.1,
            tolerance = 0.0001
        )
        expect_equal(
            CoefVarCI$new(x, correction = TRUE)$mh_ci()$statistics$lower,
            43.7,
            tolerance = 0.0001
        )
        expect_equal(
            CoefVarCI$new(x, correction = TRUE)$mh_ci()$statistics$upper,
            83.3,
            tolerance = 0.0001
        )
        expect_equal(
            CoefVarCI$new(x, correction = FALSE)$normaapprox_ci()$method,
            "cv with Normal Approximation CI"
        )
        expect_equal(
            CoefVarCI$new(
                x, correction = FALSE
                )$normaapprox_ci()$statistics$est,
            57.8,
            tolerance = 0.0001
        )
        expect_equal(
            CoefVarCI$new(
                x, correction = FALSE
                )$normaapprox_ci()$statistics$lower,
            44.6,
            tolerance = 0.0001
        )
        expect_equal(
            CoefVarCI$new(
                x, correction = FALSE
                )$normaapprox_ci()$statistics$upper,
            85.3,
            tolerance = 0.0001
        )
        expect_equal(
            CoefVarCI$new(
                x, correction = TRUE
                )$normaapprox_ci()$method,
            "corrected cv with Normal Approximation CI"
        )
        expect_equal(
            CoefVarCI$new(
                x, correction = TRUE
                )$normaapprox_ci()$statistics$est,
            58.1,
            tolerance = 0.0001
        )
        expect_equal(
            CoefVarCI$new(
                x, correction = TRUE
                )$normaapprox_ci()$statistics$lower,
            44.8,
            tolerance = 0.0001
        )
        expect_equal(
            CoefVarCI$new(
                x, correction = TRUE
                )$normaapprox_ci()$statistics$upper,
            85.8,
            tolerance = 0.0001
        )
        expect_equal(
            CoefVarCI$new(x, correction = FALSE)$shortest_ci()$method,
            "cv with Shortest-Length CI"
        )
        expect_equal(
            CoefVarCI$new(x, correction = FALSE)$shortest_ci()$statistics$est,
            57.8,
            tolerance = 0.0001
        )
        expect_equal(
            CoefVarCI$new(x, correction = FALSE)$shortest_ci()$statistics$lower,
            42,
            tolerance = 0.0001
        )
        expect_equal(
            CoefVarCI$new(x, correction = FALSE)$shortest_ci()$statistics$upper,
            81,
            tolerance = 0.0001
        )
        expect_equal(
            CoefVarCI$new(x, correction = TRUE)$shortest_ci()$method,
            "corrected cv with Shortest-Length CI"
        )
        expect_equal(
            CoefVarCI$new(x, correction = TRUE)$shortest_ci()$statistics$est,
            58.1,
            tolerance = 0.0001
        )
        expect_equal(
            CoefVarCI$new(x, correction = TRUE)$shortest_ci()$statistics$lower,
            42.3,
            tolerance = 0.0001
        )
        expect_equal(
            CoefVarCI$new(x, correction = TRUE)$shortest_ci()$statistics$upper,
            81.5,
            tolerance = 0.0001
        )
        expect_equal(
            CoefVarCI$new(x, correction = FALSE)$equal_ci()$method,
            "cv with Equal-Tailed CI"
        )
        expect_equal(
            CoefVarCI$new(
                x, correction = FALSE
            )$equal_ci()$statistics$est,
            57.8,
            tolerance = 0.0001
        )
        expect_equal(
            CoefVarCI$new(
                x, correction = FALSE
            )$equal_ci()$statistics$lower,
            44,
            tolerance = 0.0001
        )
        expect_equal(
            CoefVarCI$new(
                x, correction = FALSE
            )$equal_ci()$statistics$upper,
            84.4,
            tolerance = 0.0001
        )
        expect_equal(
            CoefVarCI$new(
                x, correction = TRUE
            )$equal_ci()$method,
            "corrected cv with Equal-Tailed CI"
        )
        expect_equal(
            CoefVarCI$new(
                x, correction = TRUE
            )$equal_ci()$statistics$est,
            58.1,
            tolerance = 0.0001
        )
        expect_equal(
            CoefVarCI$new(
                x, correction = TRUE
            )$equal_ci()$statistics$lower,
            44.2,
            tolerance = 0.0001
        )
        expect_equal(
            CoefVarCI$new(
                x, correction = TRUE
            )$equal_ci()$statistics$upper,
            84.9,
            tolerance = 0.0001
        )
        expect_equal(
            CoefVarCI$new(x, correction = FALSE)$norm_ci()$method,
            "cv with normal approximation bootstrap CI"
        )
        expect_equal(
            CoefVarCI$new(
                x, correction = FALSE
            )$norm_ci()$statistics$est,
            57.8,
            tolerance = 0.0001
        )
        expect_true(
            abs(
                CoefVarCI$new(
                    x, correction = FALSE
                )$norm_ci()$statistics$lower -
                    CoefVarCI$new(
                        x, correction = FALSE
                    )$boot_norm_ci_cv()$normal[2]
            )/100 < 0.05
        )
        expect_true(
            abs(
                CoefVarCI$new(
                    x, correction = FALSE
                )$norm_ci()$statistics$upper -
                    CoefVarCI$new(
                        x, correction = FALSE
                    )$boot_norm_ci_cv()$normal[3]
            )/100 < 0.05
        )
        expect_equal(
            CoefVarCI$new(
                x, correction = TRUE
            )$norm_ci()$method,
            "corrected cv with normal approximation bootstrap CI"
        )
        expect_equal(
            CoefVarCI$new(
                x, correction = TRUE
            )$norm_ci()$statistics$est,
            58.1,
            tolerance = 0.0001
        )
        expect_true(
            abs(
                CoefVarCI$new(
                    x, correction = TRUE
                )$norm_ci()$statistics$lower -
                    CoefVarCI$new(
                        x, correction = TRUE
                    )$boot_norm_ci_cv_corr()$normal[2]
            )/100 < 0.05
        )
        expect_true(
            abs(
                CoefVarCI$new(
                    x, correction = TRUE
                )$norm_ci()$statistics$upper -
                    CoefVarCI$new(
                        x, correction = TRUE
                    )$boot_norm_ci_cv_corr()$normal[3]
            )/100 < 0.05
        )
        expect_equal(
            CoefVarCI$new(x, correction = FALSE)$basic_ci()$method,
            "cv with basic bootstrap CI"
        )
        expect_equal(
            CoefVarCI$new(
                x, correction = FALSE
            )$basic_ci()$statistics$est,
            57.8,
            tolerance = 0.0001
        )
        expect_true(
            abs(
                CoefVarCI$new(
                    x, correction = FALSE
                )$basic_ci()$statistics$lower -
                    CoefVarCI$new(
                        x, correction = FALSE
                    )$boot_basic_ci_cv()$basic[4]
            )/100 < 0.05
        )
        expect_true(
            abs(
                CoefVarCI$new(
                    x, correction = FALSE
                )$basic_ci()$statistics$upper -
                    CoefVarCI$new(
                        x, correction = FALSE
                    )$boot_basic_ci_cv()$basic[5]
            )/100 < 0.05
        )
        expect_equal(
            CoefVarCI$new(
                x, correction = TRUE
            )$basic_ci()$method,
            "corrected cv with basic bootstrap CI"
        )
        expect_equal(
            CoefVarCI$new(
                x, correction = TRUE
            )$basic_ci()$statistics$est,
            58.1,
            tolerance = 0.0001
        )
        expect_true(
            abs(
                CoefVarCI$new(
                    x, correction = TRUE
                )$basic_ci()$statistics$lower -
                    CoefVarCI$new(
                        x, correction = TRUE
                    )$boot_basic_ci_cv_corr()$basic[4]
            )/100 < 0.05
        )
        expect_true(
            abs(
                CoefVarCI$new(
                    x, correction = TRUE
                )$basic_ci()$statistics$upper -
                    CoefVarCI$new(
                        x, correction = TRUE
                    )$boot_basic_ci_cv_corr()$basic[5]
            )/100 < 0.05
        )
        expect_equal(
            CoefVarCI$new(x, correction = FALSE)$perc_ci()$method,
            "cv with bootstrap percentile CI"
        )
        expect_equal(
            CoefVarCI$new(
                x, correction = FALSE
            )$perc_ci()$statistics$est,
            57.8,
            tolerance = 0.0001
        )
        expect_true(
            abs(
                CoefVarCI$new(
                    x, correction = FALSE
                )$perc_ci()$statistics$lower -
                    CoefVarCI$new(
                        x, correction = FALSE
                    )$boot_perc_ci_cv()$percent[4]
            )/100 < 0.05
        )
        expect_true(
            abs(
                CoefVarCI$new(
                    x, correction = FALSE
                )$perc_ci()$statistics$upper -
                    CoefVarCI$new(
                        x, correction = FALSE
                    )$boot_perc_ci_cv()$percent[5]
            )/100 < 0.05
        )
        expect_equal(
            CoefVarCI$new(
                x, correction = TRUE
            )$perc_ci()$method,
            "corrected cv with bootstrap percentile CI"
        )
        expect_equal(
            CoefVarCI$new(
                x, correction = TRUE
            )$perc_ci()$statistics$est,
            58.1,
            tolerance = 0.0001
        )
        expect_true(
            abs(
                CoefVarCI$new(
                    x, correction = TRUE
                )$perc_ci()$statistics$lower -
                    CoefVarCI$new(
                        x, correction = TRUE
                    )$boot_perc_ci_cv_corr()$percent[4]
            )/100 < 0.05
        )
        expect_true(
            abs(
                CoefVarCI$new(
                    x, correction = TRUE
                )$perc_ci()$statistics$upper -
                    CoefVarCI$new(
                        x, correction = TRUE
                    )$boot_perc_ci_cv_corr()$percent[5]
            )/100 < 0.05
        )
        expect_equal(
            CoefVarCI$new(x, correction = FALSE)$bca_ci()$method,
            "cv with adjusted bootstrap percentile (BCa) CI"
        )
        expect_equal(
            CoefVarCI$new(
                x, correction = FALSE
            )$bca_ci()$statistics$est,
            57.8,
            tolerance = 0.0001
        )
        expect_true(
            abs(
                CoefVarCI$new(
                    x, correction = FALSE
                )$bca_ci()$statistics$lower -
                    CoefVarCI$new(
                        x, correction = FALSE
                    )$boot_bca_ci_cv()$bca[4]
            )/100 < 0.3
        )
        expect_true(
            abs(
                CoefVarCI$new(
                    x, correction = FALSE
                )$bca_ci()$statistics$upper -
                    CoefVarCI$new(
                        x, correction = FALSE
                    )$boot_bca_ci_cv()$bca[5]
            )/100 < 0.3
        )
        expect_equal(
            CoefVarCI$new(
                x, correction = TRUE
            )$bca_ci()$method,
            "corrected cv with adjusted bootstrap percentile (BCa) CI"
        )
        expect_equal(
            CoefVarCI$new(
                x, correction = TRUE
            )$bca_ci()$statistics$est,
            58.1,
            tolerance = 0.0001
        )
        expect_true(
            abs(
                CoefVarCI$new(
                    x, correction = TRUE
                )$bca_ci()$statistics$lower -
                    CoefVarCI$new(
                        x, correction = TRUE
                    )$boot_bca_ci_cv_corr()$bca[4]
            )/100 < 0.05
        )
        expect_true(
            abs(
                CoefVarCI$new(
                    x, correction = TRUE
                )$bca_ci()$statistics$upper -
                    CoefVarCI$new(
                        x, correction = TRUE
                    )$boot_bca_ci_cv_corr()$bca[5]
            )/100 < 0.05
        )
        expect_equal(
            CoefVarCI$new(x)$all_ci()$method,
            "All methods"
        )
        expect_equal(
            nrow(CoefVarCI$new(x)$all_ci()$statistics),
            12
        )
        expect_equal(
            ncol(CoefVarCI$new(x)$all_ci()$statistics),
            4
        )
    }
)
test_that(
    desc = "detect R6 class", {
        y <- c(
            0.2, 0.5, 1.1, 1.4, 1.8, 2.3, 2.5, 2.7, 3.5, 4.4,
            4.6, 5.4, 5.4, 5.7, 5.8, 5.9, 6.0, 6.6, 7.1, 7.9
        )
        cv_y <- CoefVarCI$new(
           x = y,
           alpha = 0.05,
           R = 1000,
           digits = 2,
           correction = TRUE
        )
        expect_true(
            R6::is.R6(cv_y)
        )
    }
)
