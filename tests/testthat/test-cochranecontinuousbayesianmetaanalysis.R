context("Cochrane Bayesian Continuous Outcomes")

# Most of the interface is common across the modules and already tested within Cochrane Classical Continuous Outcomes.
# This tests tries to checks for any disconnects between the imported GUI, modified options, and R-code.

# output from BMA (mostly skipped on non-windows) ----
options <- analysisOptions("CochraneContinuousBayesianMetaAnalysis")
options$BFComputation <- "integration"
options$addInfo <- TRUE
options$addLines <- FALSE
options$addPrior <- FALSE
options$addStudy <- FALSE
options$additionalStudies <- list()
options$analyzeAs <- "OR"
options$analyzeData <- "individually"
options$bayesFactorType <- "BF10"
options$binWidthType <- "sturges"
options$chainsMCMC <- 4
options$checkForestPlot <- TRUE
options$checkLowerPrior <- FALSE
options$checkUpperPrior <- FALSE
options$direction <- "allPos"
options$distPlotDensity <- FALSE
options$distPlotRug <- FALSE
options$esTable <- TRUE
options$forestPlot <- "plotForestObserved"
options$forestPlotOrder <- "yearDescending"
options$informativeCauchyLocation <- 0
options$informativeCauchyScale <- 0.707
options$informativeNormalMean <- 0
options$informativeNormalStd <- 0.707
options$informativeTDf <- 1
options$informativeTLocation <- 0
options$informativeTScale <- 0.707
options$informativehalfTDf <- 1
options$informativehalfTScale <- 0.707
options$inverseGammaScale <- 0.15
options$inverseGammaShape <- 1
options$iterBridge <- 5000
options$iterMCMC <- 2000
options$keywordsSearch <- ""
options$keywordsSelected <- "5 alpha Reductase Inhibitors"
options$lowerTrunc <- 0
options$method <- "Restricted ML"
options$modelSpecification <- "BMA"
options$numberOfBins <- 30
options$orderForest <- "ascendingForest"
options$plotCumForest <- FALSE
options$plotEffectSizes <- FALSE
options$plotPosterior <- TRUE
options$plotPrior <- TRUE
options$plotSampleSizes <- FALSE
options$plotSeqPM <- FALSE
options$plotSequential <- FALSE
options$postTable <- TRUE
options$priorES <- "cauchy"
options$priorH0FE <- 0.25
options$priorH0RE <- 0.25
options$priorH1FE <- 0.25
options$priorH1RE <- 0.25
options$priorSE <- "inverseGamma"
options$reviews <- list(list(metaAnalyses = list(list(check = TRUE, value = "Peak urine flow (mL/s) at endpoint (f/u = 1 yr)")),
                             value = "Finasteride for benign prostatic hyperplasia"),
                        list(metaAnalyses = list(list(check = FALSE, value = "Mean change from baseline in Ferriman-Gallwey score")),
                             value = "Interventions for hirsutism (excluding laser and photoepilation therapy alone)"))
options$selectionType <- "selectionKeywords"
options$shade <- FALSE
options$showLabels <- TRUE
options$textSearch <- ""
options$topicsSelected <- list()
options$upperTrunc <- 0
options$module <- "Cochrane"
options$effectSize <- "effectSize"
options$standardError <- "effectSE"
options$studyLabels <- "titleStudy"
set.seed(1)
dataset <- NULL
results <- runAnalysis("CochraneContinuousBayesianMetaAnalysis", dataset, options)


test_that("Posterior Estimates per Model table results match", {
  skip_on_os(c("mac","linux","solaris"))
  table <- results[["results"]][["modelContainerPeak urine flow (mL/s) at endpoint (f/u = 1 yr)"]][["collection"]][["modelContainerPeak urine flow (mL/s) at endpoint (f/u = 1 yr)_bmaTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("TRUE", 30.0335588766012, 0.201853034804381, 0.0565682462294596,
                                      0.0905521948697584, "Fixed effects", "<unicode><unicode>", 0.31161566758007,
                                      "TRUE", 0.654066783117989, 0.191761045866901, 0.120548635622948,
                                      -0.0472415708979577, "Random effects", "<unicode><unicode>",
                                      0.432764321976626, 1, "FALSE", 1.37402957630583, 0.160605617777432,
                                      0.102599615630671, 0.0420146404099561, "Random effects", "<unicode><unicode>",
                                      0.429331506076425, 2, "TRUE", 1.11245565995041, 0.196175742865044,
                                      0.098020341600119, -0.0151953269432789, "Averaged", "<unicode><unicode>",
                                      0.391542754584822, 3, "FALSE", 3.36280932069993, "", "", "",
                                      "Averaged", "<unicode><unicode>", ""))
})

test_that("Effect Sizes per Study table results match", {
  skip_on_os(c("mac","linux","solaris"))
  table <- results[["results"]][["modelContainerPeak urine flow (mL/s) at endpoint (f/u = 1 yr)"]][["collection"]][["modelContainerPeak urine flow (mL/s) at endpoint (f/u = 1 yr)_esTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.106885699244501, -0.0615749851346899, 0.262846968019345, 0.0617472223670573,
                                      "Lepor 1996, (Finasteride 5 mg vs Placebo)", 0.208800438601001,
                                      -0.121436345987518, 0.537429984937633, 0.268314169554707, "Yu 1995, (Finasteride 5 mg vs Placebo)",
                                      0.17978710961424, -0.166369662968739, 0.508382278072863, 0.109072955349075,
                                      "Tammela 1993, (Finasteride 5 mg vs Placebo)", 0.289287960651373,
                                      0.137365100433637, 0.44840834170096, 0.33077449237164, "Gormley 1992, (Finasteride 5 mg vs Placebo)"
                                 ))
})

test_that("Observed study effects plot matches", {
  skip_on_os(c("mac","linux","solaris"))
  plotName <- results[["results"]][["modelContainerPeak urine flow (mL/s) at endpoint (f/u = 1 yr)"]][["collection"]][["modelContainerPeak urine flow (mL/s) at endpoint (f/u = 1 yr)_forestContainer"]][["collection"]][["modelContainerPeak urine flow (mL/s) at endpoint (f/u = 1 yr)_forestContainer_forestPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "observed-study-effects")
})

test_that("Effect size plot matches", {
  skip_on_os(c("mac","linux","solaris"))
  plotName <- results[["results"]][["modelContainerPeak urine flow (mL/s) at endpoint (f/u = 1 yr)"]][["collection"]][["modelContainerPeak urine flow (mL/s) at endpoint (f/u = 1 yr)_postContainer"]][["collection"]][["modelContainerPeak urine flow (mL/s) at endpoint (f/u = 1 yr)_postContainer_ES"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "effect-size")
})

test_that("Heterogeneity plot matches", {
  skip_on_os(c("mac","linux","solaris"))
  plotName <- results[["results"]][["modelContainerPeak urine flow (mL/s) at endpoint (f/u = 1 yr)"]][["collection"]][["modelContainerPeak urine flow (mL/s) at endpoint (f/u = 1 yr)_postContainer"]][["collection"]][["modelContainerPeak urine flow (mL/s) at endpoint (f/u = 1 yr)_postContainer_SE"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "heterogeneity")
})

test_that("Model Probabilities table results match", {
  skip_on_os(c("mac","linux","solaris"))
  table <- results[["results"]][["modelContainerPeak urine flow (mL/s) at endpoint (f/u = 1 yr)"]][["collection"]][["modelContainerPeak urine flow (mL/s) at endpoint (f/u = 1 yr)_postTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Fixed H<unicode><unicode><unicode>", 0.0073858789423501, 0.25,
                                      "Fixed H<unicode><unicode><unicode>", 0.221824230070521, 0.25,
                                      "Random H<unicode><unicode><unicode>", 0.465996838128964, 0.25,
                                      "Random H<unicode><unicode><unicode>", 0.304793052858165, 0.25
                                 ))
})

test_that("Effect Size plot matches", {
  skip_on_os(c("mac","linux","solaris"))
  plotName <- results[["results"]][["modelContainerPeak urine flow (mL/s) at endpoint (f/u = 1 yr)"]][["collection"]][["modelContainerPeak urine flow (mL/s) at endpoint (f/u = 1 yr)_priorContainer"]][["collection"]][["modelContainerPeak urine flow (mL/s) at endpoint (f/u = 1 yr)_priorContainer_ES"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "effect-size")
})

test_that("Heterogeneity plot matches", {
  skip_on_os(c("mac","linux","solaris"))
  plotName <- results[["results"]][["modelContainerPeak urine flow (mL/s) at endpoint (f/u = 1 yr)"]][["collection"]][["modelContainerPeak urine flow (mL/s) at endpoint (f/u = 1 yr)_priorContainer"]][["collection"]][["modelContainerPeak urine flow (mL/s) at endpoint (f/u = 1 yr)_priorContainer_SE"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "heterogeneity")
})

test_that("Meta-Analyses table results match", {
  skip_on_os(c("mac","linux","solaris"))
  table <- results[["results"]][["selectedOverviewTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(4, "Peak urine flow (mL/s) at endpoint (f/u = 1 yr)", 2010))
})
