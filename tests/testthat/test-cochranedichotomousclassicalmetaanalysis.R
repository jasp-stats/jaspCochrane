context("Cochrane Classical Dichotomous Outcomes")

# Most of the interface is common across the modules and already tested within Cochrane Classical Continuous Outcomes.
# This tests extend the coverage by considering:
# - different effect size transformations
# - different way of adding estimates


# log(OR) ----
options <- analysisOptions("CochraneDichotomousClassicalMetaAnalysis")
options$addStudy <- FALSE
options$additionalStudies <- list()
options$analyzeAs <- "OR"
options$analyzeData <- "pooled"
options$binWidthType <- "sturges"
options$distPlotDensity <- FALSE
options$distPlotRug <- FALSE
options$forestPlot <- TRUE
options$forestPlotOrder <- "yearDescending"
options$funnelPlot <- FALSE
options$funnelPlotAsymmetryTest <- FALSE
options$keywordsSearch <- ""
options$keywordsSelected <- "Abdominal Abscess"
options$method <- "Restricted ML"
options$modelFit <- FALSE
options$numberOfBins <- 30
options$plotEffectSizes <- TRUE
options$plotResidualsCovariates <- FALSE
options$plotResidualsDependent <- FALSE
options$plotResidualsPredicted <- FALSE
options$plotResidualsQQ <- TRUE
options$plotSampleSizes <- FALSE
options$rSquaredChange <- FALSE
options$regressionCoefficientsConfidenceIntervals <- FALSE
options$regressionCoefficientsConfidenceIntervalsInterval <- 0.95
options$regressionCoefficientsCovarianceMatrix <- FALSE
options$regressionCoefficientsEstimates <- TRUE
options$residualsCasewiseDiagnostics <- FALSE
options$residualsParameters <- TRUE
options$reviews <- list(list(metaAnalyses = list(list(check = FALSE, value = "Intra-peritoneal abscess"),
                                                 list(check = FALSE, value = "Mortality"), list(check = TRUE,
                                                                                                value = "Wound infection")), value = "Abdominal drainage to prevent intra peritoneal abscess after open appendectomy for complicated appendicitis"),
                        list(metaAnalyses = list(list(check = TRUE, value = "Wound infections")),
                             value = "Laparoscopic versus open surgery for suspected appendicitis"))
options$selectionType <- "selectionKeywords"
options$showLabels <- TRUE
options$test <- "z"
options$textSearch <- ""
options$topicsSelected <- list()
options$trimFillPlot <- FALSE
options$module <- "Cochrane"
options$dependent <- "effectSizeOR"
options$wlsWeights <- "effectSEOR"
options$includeConstant <- TRUE
options$studyLabels <- "titleStudy"
options$factors <- list()
options$covariates <- list()
options$modelTerms <- list()
options$components <- list()
set.seed(1)
dataset <- NULL
results <- runAnalysis("CochraneDichotomousClassicalMetaAnalysis", dataset, options)


test_that("Coefficients table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_coeffTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.0386519936770895, "intercept", 0.93627559553263, 0.48344122712573,
                                      0.0799517945684784))
})

test_that("Log(Odds Ratio) plot matches", {
  plotName <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_effectSizePlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "log-odds-ratio-1")
})

test_that("Fixed and Random Effects table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_fixRandTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, "Omnibus test of Model Coefficients", 0.93627559553263, 0.00639228945472017,
                                      9, "Test of Residual Heterogeneity", 7.30321249691888e-05, 34.4981081179699
                                 ))
})

test_that("Forest plot matches", {
  plotName <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_plots"]][["collection"]][["modelContainer_plots_forest"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "forest-plot-1")
})

test_that("Residual Heterogeneity Estimates table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_residualTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1.55038735834507, "<unicode><unicode><unicode><unicode>", 1.2451455169357,
                                      "<unicode><unicode>", 75.4832927918001, "I<unicode><unicode> (%)",
                                      4.07885117486551, "H<unicode><unicode>"))
})

test_that("Meta-Analyses table results match", {
  table <- results[["results"]][["selectedOverviewTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(5, "Wound infection", 2018, 5, "Wound infections", 2018))
})

# log(Peto's OR) & adding studies ----
options <- analysisOptions("CochraneDichotomousClassicalMetaAnalysis")
options$addStudy <- TRUE
options$additionalStudies <- list(list(effectSE = "", effectSize = "", lCI = "", n1 = "52",
                                       n2 = "53", name = "#", titleStudy = "My Awesome Study", uCI = "",
                                       x1 = "42", x2 = "43"))
options$analyzeAs <- "POR"
options$analyzeData <- "individually"
options$binWidthType <- "sturges"
options$distPlotDensity <- FALSE
options$distPlotRug <- FALSE
options$forestPlot <- TRUE
options$forestPlotOrder <- "yearDescending"
options$funnelPlot <- FALSE
options$funnelPlotAsymmetryTest <- FALSE
options$keywordsSearch <- ""
options$keywordsSelected <- "Abdominal Abscess"
options$method <- "Restricted ML"
options$modelFit <- FALSE
options$numberOfBins <- 30
options$plotEffectSizes <- TRUE
options$plotResidualsCovariates <- FALSE
options$plotResidualsDependent <- FALSE
options$plotResidualsPredicted <- FALSE
options$plotResidualsQQ <- TRUE
options$plotSampleSizes <- FALSE
options$rSquaredChange <- FALSE
options$regressionCoefficientsConfidenceIntervals <- FALSE
options$regressionCoefficientsConfidenceIntervalsInterval <- 0.95
options$regressionCoefficientsCovarianceMatrix <- FALSE
options$regressionCoefficientsEstimates <- TRUE
options$residualsCasewiseDiagnostics <- FALSE
options$residualsParameters <- TRUE
options$reviews <- list(list(metaAnalyses = list(list(check = FALSE, value = "Intra-peritoneal abscess"),
                                                 list(check = FALSE, value = "Mortality"), list(check = TRUE,
                                                                                                value = "Wound infection")), value = "Abdominal drainage to prevent intra peritoneal abscess after open appendectomy for complicated appendicitis"),
                        list(metaAnalyses = list(list(check = TRUE, value = "Wound infections")),
                             value = "Laparoscopic versus open surgery for suspected appendicitis"))
options$selectionType <- "selectionKeywords"
options$showLabels <- TRUE
options$test <- "z"
options$textSearch <- ""
options$topicsSelected <- list()
options$trimFillPlot <- FALSE
options$module <- "Cochrane"
options$dependent <- "effectSizePOR"
options$wlsWeights <- "effectSEPOR"
options$includeConstant <- TRUE
options$studyLabels <- "titleStudy"
options$factors <- list()
options$covariates <- list()
options$modelTerms <- list()
options$components <- list()
set.seed(1)
dataset <- NULL
results <- runAnalysis("CochraneDichotomousClassicalMetaAnalysis", dataset, options)


test_that("Coefficients table results match", {
  table <- results[["results"]][["modelContainerWound infection"]][["collection"]][["modelContainerWound infection_coeffTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.688192389574445, "intercept", 0.06044194268516, 0.366536297320385,
                                      1.8775559053921))
})

test_that("Log(Peto's Odds Ratio) plot matches", {
  plotName <- results[["results"]][["modelContainerWound infection"]][["collection"]][["modelContainerWound infection_effectSizePlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "log-peto-s-odds-ratio-2")
})

test_that("Fixed and Random Effects table results match", {
  table <- results[["results"]][["modelContainerWound infection"]][["collection"]][["modelContainerWound infection_fixRandTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, "Omnibus test of Model Coefficients", 0.0604419426851598, 3.52521617787276,
                                      5, "Test of Residual Heterogeneity", 0.0158272358312813, 13.9659380599078
                                 ))
})

test_that("Forest plot matches", {
  plotName <- results[["results"]][["modelContainerWound infection"]][["collection"]][["modelContainerWound infection_plots"]][["collection"]][["modelContainerWound infection_plots_forest"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "forest-plot-2")
})

test_that("Residual Heterogeneity Estimates table results match", {
  table <- results[["results"]][["modelContainerWound infection"]][["collection"]][["modelContainerWound infection_residualTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.509966846216281, "<unicode><unicode><unicode><unicode>", 0.714119630185504,
                                      "<unicode><unicode>", 64.7732192884569, "I<unicode><unicode> (%)",
                                      2.83874932594201, "H<unicode><unicode>"))
})

test_that("Coefficients table results match", {
  table <- results[["results"]][["modelContainerWound infections"]][["collection"]][["modelContainerWound infections_coeffTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-0.893009216958926, "intercept", 0.00893477370988941, 0.341555022391986,
                                      -2.61453984984611))
})

test_that("Log(Peto's Odds Ratio) plot matches", {
  plotName <- results[["results"]][["modelContainerWound infections"]][["collection"]][["modelContainerWound infections_effectSizePlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "log-peto-s-odds-ratio-3")
})

test_that("Fixed and Random Effects table results match", {
  table <- results[["results"]][["modelContainerWound infections"]][["collection"]][["modelContainerWound infections_fixRandTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, "Omnibus test of Model Coefficients", 0.00893477370988943,
                                      6.8358186264333, 5, "Test of Residual Heterogeneity", 0.368730638887039,
                                      5.40272254606942))
})

test_that("Forest plot matches", {
  plotName <- results[["results"]][["modelContainerWound infections"]][["collection"]][["modelContainerWound infections_plots"]][["collection"]][["modelContainerWound infections_plots_forest"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "forest-plot-3")
})

test_that("Residual Heterogeneity Estimates table results match", {
  table <- results[["results"]][["modelContainerWound infections"]][["collection"]][["modelContainerWound infections_residualTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.177653211460119, "<unicode><unicode><unicode><unicode>", 0.421489277989511,
                                      "<unicode><unicode>", 26.5316849774704, "I<unicode><unicode> (%)",
                                      1.36113098509656, "H<unicode><unicode>"))
})

test_that("Meta-Analyses table results match", {
  table <- results[["results"]][["selectedOverviewTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(5, "Wound infection", 2018, 5, "Wound infections", 2018))
})

# log(Risk ratios) ----
options <- analysisOptions("CochraneDichotomousClassicalMetaAnalysis")
options$addStudy <- TRUE
options$additionalStudies <- list(list(effectSE = "", effectSize = "", lCI = "", n1 = "52",
                                       n2 = "53", name = "#", titleStudy = "My Awesome Study", uCI = "",
                                       x1 = "42", x2 = "43"))
options$analyzeAs <- "RR"
options$analyzeData <- "pooled"
options$binWidthType <- "sturges"
options$distPlotDensity <- FALSE
options$distPlotRug <- FALSE
options$forestPlot <- TRUE
options$forestPlotOrder <- "yearDescending"
options$funnelPlot <- FALSE
options$funnelPlotAsymmetryTest <- FALSE
options$keywordsSearch <- ""
options$keywordsSelected <- "Abdominal Abscess"
options$method <- "Restricted ML"
options$modelFit <- FALSE
options$numberOfBins <- 30
options$plotEffectSizes <- TRUE
options$plotResidualsCovariates <- FALSE
options$plotResidualsDependent <- FALSE
options$plotResidualsPredicted <- FALSE
options$plotResidualsQQ <- TRUE
options$plotSampleSizes <- FALSE
options$rSquaredChange <- FALSE
options$regressionCoefficientsConfidenceIntervals <- FALSE
options$regressionCoefficientsConfidenceIntervalsInterval <- 0.95
options$regressionCoefficientsCovarianceMatrix <- FALSE
options$regressionCoefficientsEstimates <- TRUE
options$residualsCasewiseDiagnostics <- FALSE
options$residualsParameters <- TRUE
options$reviews <- list(list(metaAnalyses = list(list(check = FALSE, value = "Intra-peritoneal abscess"),
                                                 list(check = FALSE, value = "Mortality"), list(check = TRUE,
                                                                                                value = "Wound infection")), value = "Abdominal drainage to prevent intra peritoneal abscess after open appendectomy for complicated appendicitis"),
                        list(metaAnalyses = list(list(check = TRUE, value = "Wound infections")),
                             value = "Laparoscopic versus open surgery for suspected appendicitis"))
options$selectionType <- "selectionKeywords"
options$showLabels <- TRUE
options$test <- "z"
options$textSearch <- ""
options$topicsSelected <- list()
options$trimFillPlot <- FALSE
options$module <- "Cochrane"
options$dependent <- "effectSizeRR"
options$wlsWeights <- "effectSERR"
options$includeConstant <- TRUE
options$studyLabels <- "titleStudy"
options$factors <- list()
options$covariates <- list()
options$modelTerms <- list()
options$components <- list()
set.seed(1)
dataset <- NULL
results <- runAnalysis("CochraneDichotomousClassicalMetaAnalysis", dataset, options)


test_that("Coefficients table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_coeffTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.0255763990646939, "intercept", 0.938761402001793, 0.332910247697645,
                                      0.0768267100264298))
})

test_that("Log(Risk Ratio) plot matches", {
  plotName <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_effectSizePlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "log-risk-ratio-4")
})

test_that("Fixed and Random Effects table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_fixRandTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, "Omnibus test of Model Coefficients", 0.938761402001793, 0.00590234337348513,
                                      10, "Test of Residual Heterogeneity", 0.000434787869838816,
                                      31.7853079305509))
})

test_that("Forest plot matches", {
  plotName <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_plots"]][["collection"]][["modelContainer_plots_forest"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "forest-plot-4")
})

test_that("Residual Heterogeneity Estimates table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_residualTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.763039722332326, "<unicode><unicode><unicode><unicode>", 0.873521449268606,
                                      "<unicode><unicode>", 90.9078853781171, "I<unicode><unicode> (%)",
                                      10.998541500931, "H<unicode><unicode>"))
})

test_that("Meta-Analyses table results match", {
  table <- results[["results"]][["selectedOverviewTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(5, "Wound infection", 2018, 5, "Wound infections", 2018))
})
# Risk differences ----
options <- analysisOptions("CochraneDichotomousClassicalMetaAnalysis")
options$addStudy <- TRUE
options$additionalStudies <- list(list(effectSE = "", effectSize = "", lCI = "", n1 = "52",
                                       n2 = "53", name = "#", titleStudy = "My Awesome Study", uCI = "",
                                       x1 = "42", x2 = "43"))
options$analyzeAs <- "RD"
options$analyzeData <- "pooled"
options$binWidthType <- "sturges"
options$distPlotDensity <- FALSE
options$distPlotRug <- FALSE
options$forestPlot <- TRUE
options$forestPlotOrder <- "yearDescending"
options$funnelPlot <- FALSE
options$funnelPlotAsymmetryTest <- FALSE
options$keywordsSearch <- ""
options$keywordsSelected <- "Abdominal Abscess"
options$method <- "Restricted ML"
options$modelFit <- FALSE
options$numberOfBins <- 30
options$plotEffectSizes <- TRUE
options$plotResidualsCovariates <- FALSE
options$plotResidualsDependent <- FALSE
options$plotResidualsPredicted <- FALSE
options$plotResidualsQQ <- TRUE
options$plotSampleSizes <- FALSE
options$rSquaredChange <- FALSE
options$regressionCoefficientsConfidenceIntervals <- FALSE
options$regressionCoefficientsConfidenceIntervalsInterval <- 0.95
options$regressionCoefficientsCovarianceMatrix <- FALSE
options$regressionCoefficientsEstimates <- TRUE
options$residualsCasewiseDiagnostics <- FALSE
options$residualsParameters <- TRUE
options$reviews <- list(list(metaAnalyses = list(list(check = FALSE, value = "Intra-peritoneal abscess"),
                                                 list(check = FALSE, value = "Mortality"), list(check = TRUE,
                                                                                                value = "Wound infection")), value = "Abdominal drainage to prevent intra peritoneal abscess after open appendectomy for complicated appendicitis"),
                        list(metaAnalyses = list(list(check = TRUE, value = "Wound infections")),
                             value = "Laparoscopic versus open surgery for suspected appendicitis"))
options$selectionType <- "selectionKeywords"
options$showLabels <- TRUE
options$test <- "z"
options$textSearch <- ""
options$topicsSelected <- list()
options$trimFillPlot <- FALSE
options$module <- "Cochrane"
options$dependent <- "effectSizeRD"
options$wlsWeights <- "effectSERD"
options$includeConstant <- TRUE
options$studyLabels <- "titleStudy"
options$factors <- list()
options$covariates <- list()
options$modelTerms <- list()
options$components <- list()
set.seed(1)
dataset <- NULL
results <- runAnalysis("CochraneDichotomousClassicalMetaAnalysis", dataset, options)

test_that("Coefficients table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_coeffTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.0264307344539155, "intercept", 0.532499705858921, 0.0423437319836258,
                                      0.624194732390055))
})

test_that("Risk Difference plot matches", {
  plotName <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_effectSizePlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "risk-difference-5")
})

test_that("Fixed and Random Effects table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_fixRandTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, "Omnibus test of Model Coefficients", 0.532499705858921, 0.389619063943493,
                                      10, "Test of Residual Heterogeneity", 5.11711464959007e-06,
                                      42.9306095035547))
})

test_that("Forest plot matches", {
  plotName <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_plots"]][["collection"]][["modelContainer_plots_forest"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "forest-plot-5")
})

test_that("Residual Heterogeneity Estimates table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_residualTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.0154100582693616, "<unicode><unicode><unicode><unicode>", 0.124137255767,
                                      "<unicode><unicode>", 88.472616144041, "I<unicode><unicode> (%)",
                                      8.67499523305153, "H<unicode><unicode>"))
})

test_that("Meta-Analyses table results match", {
  table <- results[["results"]][["selectedOverviewTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(5, "Wound infection", 2018, 5, "Wound infections", 2018))
})
