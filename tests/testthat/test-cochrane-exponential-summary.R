context("Cochrane Dichotomous Classical Meta-Analysis - transformed coefficients")

.makeCochraneTestContainer <- function(fit, options) {
  cpp <- jaspBase:::create_cpp_jaspResults("cochrane_exp_summary_test", NULL)
  jr <- jaspBase:::jaspResultsR$new(cpp)
  cpp$setOptions(jsonlite::toJSON(options, auto_unbox = TRUE))
  container <- jaspBase::createJaspContainer()
  jr[["modelContainer"]] <- container
  container[["Model"]] <- jaspBase::createJaspState(fit)
  list(jaspResults = jr, container = container)
}

testthat::test_that("transformed coefficients exponentiate estimate, ci.lb, and ci.ub for z and knha", {
  dat <- metafor::escalc(
    measure = "OR",
    ai = tpos, bi = tneg, ci = cpos, di = cneg,
    data = metadat::dat.bcg
  )

  for (analyzeAs in c("logOr", "logPor", "logRr")) {
    for (estimateTest in c("z", "knha")) {
      fit <- metafor::rma(yi = yi, vi = vi, data = dat, method = "REML", test = estimateTest, level = 95)
      cs  <- stats::coef(summary(fit))
      options <- list(
        analyzeAs           = analyzeAs,
        estimateTest        = estimateTest,
        coefficientEstimate = TRUE,
        coefficientCi       = TRUE,
        coefficientCiLevel  = 0.95
      )
      ctx <- .makeCochraneTestContainer(fit, options)
      jaspCochrane:::.ClassicalMetaAnalysisAddExponentialSummary(ctx$container, ready = TRUE, options = options)

      tbl <- ctx$container[["coeffTableExponentiated"]]$toRObject()
      testthat::expect_equal(tbl$est,   exp(cs[1, "estimate"]), tolerance = 1e-6)
      testthat::expect_equal(tbl$lower, exp(cs[1, "ci.lb"]),    tolerance = 1e-6)
      testthat::expect_equal(tbl$upper, exp(cs[1, "ci.ub"]),    tolerance = 1e-6)
      testthat::expect_true(tbl$lower <= tbl$est && tbl$est <= tbl$upper)
    }
  }
})

testthat::test_that("transformed coefficients are not created for risk difference", {
  dat <- metafor::escalc(
    measure = "RD",
    ai = tpos, bi = tneg, ci = cpos, di = cneg,
    data = metadat::dat.bcg
  )
  fit <- metafor::rma(yi = yi, vi = vi, data = dat, method = "REML", test = "knha", level = 95)
  options <- list(
    analyzeAs           = "Rd",
    estimateTest        = "knha",
    coefficientEstimate = TRUE,
    coefficientCi       = TRUE,
    coefficientCiLevel  = 0.95
  )
  ctx <- .makeCochraneTestContainer(fit, options)
  jaspCochrane:::.ClassicalMetaAnalysisAddExponentialSummary(ctx$container, ready = TRUE, options = options)

  testthat::expect_null(ctx$container[["coeffTableExponentiated"]])
})
