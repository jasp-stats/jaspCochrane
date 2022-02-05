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
options$reviews <- list(list(metaAnalyses = list(list(check = FALSE, value = "Vitamin E versus placebo (Outcome is common so use RR statistic!)")),
     value = "Vitamin E for intermittent claudication"), list(
       metaAnalyses = list(list(check = FALSE, value = "LMWH versus VKA during allocated treatment (category I and II trials) in participants with VTE"),
                           list(check = FALSE, value = "LMWH versus VKA during allocated treatment (category I and II trials) in participants with DVT"),
                           list(check = FALSE, value = "LMWH versus VKA during allocated treatment (category I and II trials) in participants with PE"),
                           list(check = FALSE, value = "LMWH versus VKA during allocated treatment (category I trials) in participants with VTE"),
                           list(check = FALSE, value = "Category I trials and the same initial treatment in both groups (unfractionated heparin or LMWH)"),
                           list(check = FALSE, value = "Category I trials and initial treatment not the same in both groups (unfractionated heparin compared with LMWH)"),
                           list(check = FALSE, value = "LMWH versus VKA during additional follow-up (category I and II trials)"),
                           list(check = FALSE, value = "LMWH versus VKA during additional nine months of follow-up (category I trials)"),
                           list(check = TRUE, value = "LMWH versus VKA for total period of 12 months of follow-up (category I and II trials)"),
                           list(check = TRUE, value = "LMWH versus VKA for total period of 12 months of follow-up (category I trials)")),
       value = "Vitamin K antagonists versus low-molecular-weight heparin for the long term treatment of symptomatic venous thromboembolism"),
list(metaAnalyses = list(list(check = FALSE, value = "10 mg nomogram versus 5 mg nomogram")),
     value = "Warfarin initiation nomograms for venous thromboembolism"))
options$selectionType <- "selectionTopics"
options$showLabels <- TRUE
options$test <- "z"
options$textSearch <- ""
options$topicsSelected <- "Vascular"
options$trimFillPlot <- FALSE
set.seed(1)
dataset <- NULL
results <- runAnalysis("CochraneDichotomousClassicalMetaAnalysis", dataset, options)


test_that("Coefficients table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_coeffTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.0488032474983105, "intercept", 0.483093482888022, 0.0695861357092754,
                                      0.701335790540317))
})

test_that("Log(Odds Ratio) plot matches", {
  plotName <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_effectSizePlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "log-odds-ratio-1")
})

test_that("Fixed and Random Effects table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_fixRandTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, "Omnibus test of Model Coefficients", 0.483093482888022, 0.491871891092812,
                                      42, "Test of Residual Heterogeneity", 0.639533348034929, 38.1761380082864
                                 ))
})

test_that("Forest Plot matches", {
  plotName <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_plots"]][["collection"]][["modelContainer_plots_forest"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "forest-plot-1")
})

test_that("Residual Heterogeneity Estimates table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_residualTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0, "<unicode><unicode><unicode><unicode>", 0, "<unicode><unicode>",
                                      0, "I<unicode><unicode> (%)", 1, "H<unicode><unicode>"))
})

test_that("Meta-Analyses table results match", {
  table <- results[["results"]][["selectedOverviewTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(29, "LMWH versus VKA for total period of 12 months of follow-up (category I and II trials)",
                                      2017, 14, "LMWH versus VKA for total period of 12 months of follow-up (category I trials)",
                                      2017))
})

# Risk differences & adding studies ----
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
options$reviews <- list(list(metaAnalyses = list(list(check = FALSE, value = "Vitamin E versus placebo (Outcome is common so use RR statistic!)")),
                             value = "Vitamin E for intermittent claudication"), list(
                               metaAnalyses = list(list(check = FALSE, value = "LMWH versus VKA during allocated treatment (category I and II trials) in participants with VTE"),
                                                   list(check = FALSE, value = "LMWH versus VKA during allocated treatment (category I and II trials) in participants with DVT"),
                                                   list(check = FALSE, value = "LMWH versus VKA during allocated treatment (category I and II trials) in participants with PE"),
                                                   list(check = FALSE, value = "LMWH versus VKA during allocated treatment (category I trials) in participants with VTE"),
                                                   list(check = FALSE, value = "Category I trials and the same initial treatment in both groups (unfractionated heparin or LMWH)"),
                                                   list(check = FALSE, value = "Category I trials and initial treatment not the same in both groups (unfractionated heparin compared with LMWH)"),
                                                   list(check = FALSE, value = "LMWH versus VKA during additional follow-up (category I and II trials)"),
                                                   list(check = FALSE, value = "LMWH versus VKA during additional nine months of follow-up (category I trials)"),
                                                   list(check = TRUE, value = "LMWH versus VKA for total period of 12 months of follow-up (category I and II trials)"),
                                                   list(check = TRUE, value = "LMWH versus VKA for total period of 12 months of follow-up (category I trials)")),
                               value = "Vitamin K antagonists versus low-molecular-weight heparin for the long term treatment of symptomatic venous thromboembolism"),
                        list(metaAnalyses = list(list(check = FALSE, value = "10 mg nomogram versus 5 mg nomogram")),
                             value = "Warfarin initiation nomograms for venous thromboembolism"))
options$selectionType <- "selectionTopics"
options$showLabels <- TRUE
options$test <- "z"
options$textSearch <- ""
options$topicsSelected <- "Vascular"
options$trimFillPlot <- FALSE
set.seed(1)
dataset <- NULL
results <- runAnalysis("CochraneDichotomousClassicalMetaAnalysis", dataset, options)


test_that("Coefficients table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_coeffTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.00617951966086318, "intercept", 0.10240810929341, 0.00378348075759956,
                                      1.63328957031191))
})

test_that("Risk Difference plot matches", {
  plotName <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_effectSizePlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "risk-difference-2")
})

test_that("Fixed and Random Effects table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_fixRandTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, "Omnibus test of Model Coefficients", 0.10240810929341, 2.66763482048967,
                                      43, "Test of Residual Heterogeneity", 0.615458508965633, 39.6944675715245
                                 ))
})

test_that("Forest Plot matches", {
  plotName <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_plots"]][["collection"]][["modelContainer_plots_forest"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "forest-plot-2")
})

test_that("Residual Heterogeneity Estimates table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_residualTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(2.06153356760956e-07, "<unicode><unicode><unicode><unicode>",
                                      0.000454041139943239, "<unicode><unicode>", 0.032082460189922,
                                      "I<unicode><unicode> (%)", 1.00032092756336, "H<unicode><unicode>"
                                 ))
})

test_that("Meta-Analyses table results match", {
  table <- results[["results"]][["selectedOverviewTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(29, "LMWH versus VKA for total period of 12 months of follow-up (category I and II trials)",
                                      2017, 14, "LMWH versus VKA for total period of 12 months of follow-up (category I trials)",
                                      2017))
})
