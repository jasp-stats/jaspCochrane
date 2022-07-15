context("Cochrane Bayesian Dichotomous Outcomes")

# Most of the interface is common across the modules and already tested within Cochrane Classical Continuous Outcomes.
# This tests tries to checks for any disconnects between the imported GUI, modified options, and R-code.

# output from BMA (mostly skipped on non-windows) + adding an estimate ----
options <- analysisOptions("CochraneDichotomousBayesianMetaAnalysis")
options$savePath <- ""
options$BFComputation <- "integration"
options$addInfo <- FALSE
options$addLines <- FALSE
options$addPrior <- FALSE
options$addStudy <- FALSE
options$additionalStudies <- list()
options$analyzeAs <- "POR"
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
options$esTable <- FALSE
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
options$keywordsSelected <- "5-Hydroxytryptophan"
options$lowerTrunc <- 0
options$method <- "Restricted ML"
options$modelSpecification <- "BMA"
options$numberOfBins <- 30
options$orderForest <- "ascendingForest"
options$plotCumForest <- FALSE
options$plotEffectSizes <- FALSE
options$plotPosterior <- FALSE
options$plotPrior <- TRUE
options$plotSampleSizes <- FALSE
options$plotSeqPM <- FALSE
options$plotSequential <- FALSE
options$postTable <- FALSE
options$priorES <- "normal"
options$priorH0FE <- 0.25
options$priorH0RE <- 0.25
options$priorH1FE <- 0.25
options$priorH1RE <- 0.25
options$priorSE <- "halfT"
options$reviews <- list(list(metaAnalyses = list(list(check = TRUE, value = "L-Tryptophan and 5-HTP versus placebo for the treatment of depression"),
                                                 list(check = FALSE, value = "Side-effects of L-Tryptophan and 5-HTP versus placebo")),
                             value = "Tryptophan and 5-Hydroxytryptophan for depression"))
options$selectionType <- "selectionKeywords"
options$shade <- FALSE
options$showLabels <- TRUE
options$textSearch <- ""
options$topicsSelected <- list()
options$upperTrunc <- 0
set.seed(1)
dataset <- NULL
results <- runAnalysis("CochraneDichotomousBayesianMetaAnalysis", dataset, options)


test_that("Posterior Estimates per Model table results match", {
  skip_on_os(c("mac","linux","solaris"))
  table <- results[["results"]][["modelContainerL-Tryptophan and 5-HTP versus placebo for the treatment of depression"]][["collection"]][["modelContainerL-Tryptophan and 5-HTP versus placebo for the treatment of depression_bmaTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("TRUE", 3.16240788506859, -0.821086787705721, 0.450510842366538,
                                      -1.72170555981543, "Fixed effects", "<unicode><unicode>", 0.0413181596026469,
                                      "TRUE", 1.71702046182603, -0.64229360446479, 0.566714911598498,
                                      -1.68676281338169, "Random effects", "<unicode><unicode>", 0.584859221037347,
                                      1, "FALSE", 0.754423011439686, 0.741309268545313, 0.90452587189456,
                                      0.020984467006987, "Random effects", "<unicode><unicode>", 3.23422471642881,
                                      2, "TRUE", 2.32191260100373, -0.734210908073228, 0.511125246226745,
                                      -1.68731337941256, "Averaged", "<unicode><unicode>", 0.323038224538838,
                                      3, "FALSE", 0.906996505471206, "", "", "", "Averaged", "<unicode><unicode>",
                                      ""))
})

test_that("Observed study effects plot matches", {
  skip_on_os(c("mac","linux","solaris"))
  plotName <- results[["results"]][["modelContainerL-Tryptophan and 5-HTP versus placebo for the treatment of depression"]][["collection"]][["modelContainerL-Tryptophan and 5-HTP versus placebo for the treatment of depression_forestContainer"]][["collection"]][["modelContainerL-Tryptophan and 5-HTP versus placebo for the treatment of depression_forestContainer_forestPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "observed-study-effects")
})

test_that("Effect Size plot matches", {
  skip_on_os(c("mac","linux","solaris"))
  plotName <- results[["results"]][["modelContainerL-Tryptophan and 5-HTP versus placebo for the treatment of depression"]][["collection"]][["modelContainerL-Tryptophan and 5-HTP versus placebo for the treatment of depression_priorContainer"]][["collection"]][["modelContainerL-Tryptophan and 5-HTP versus placebo for the treatment of depression_priorContainer_ES"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "effect-size")
})

test_that("Heterogeneity plot matches", {
  skip_on_os(c("mac","linux","solaris"))
  plotName <- results[["results"]][["modelContainerL-Tryptophan and 5-HTP versus placebo for the treatment of depression"]][["collection"]][["modelContainerL-Tryptophan and 5-HTP versus placebo for the treatment of depression_priorContainer"]][["collection"]][["modelContainerL-Tryptophan and 5-HTP versus placebo for the treatment of depression_priorContainer_SE"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "heterogeneity")
})

test_that("Meta-Analyses table results match", {
  skip_on_os(c("mac","linux","solaris"))
  table <- results[["results"]][["selectedOverviewTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(2, "L-Tryptophan and 5-HTP versus placebo for the treatment of depression",
                                      2002))
})
