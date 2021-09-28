context("Cochrane Bayesian Dichotomous Outcomes")

# Most of the interface is common across the modules and already tested within Cochrane Classical Continuous Outcomes.
# This tests tries to checks for any disconnects between the imported GUI, modified options, and R-code.

# output from BMA (mostly skipped on non-windows) + adding an estimate ----
options <- analysisOptions("CochraneDichotomousBayesianMetaAnalysis")
options$BFComputation <- "integration"
options$addInfo <- FALSE
options$addLines <- FALSE
options$addPrior <- FALSE
options$addStudy <- TRUE
options$additionalStudies <- list(list(effectSE = "0.05", effectSize = "0.10", lCI = "", n1 = "",
                                       n2 = "", name = "#", titleStudy = "", uCI = "", x1 = "",
                                       x2 = ""))
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
options$keywordsSelected <- "3',5' Cyclic GMP Phosphodiesterases"
options$lowerTrunc <- 0
options$method <- "Restricted ML"
options$modelSpecification <- "BMA"
options$numberOfBins <- 30
options$orderForest <- "ascendingForest"
options$plotCumForest <- FALSE
options$plotEffectSizes <- FALSE
options$plotPosterior <- FALSE
options$plotPrior <- FALSE
options$plotSampleSizes <- FALSE
options$plotSeqPM <- FALSE
options$plotSequential <- FALSE
options$postTable <- FALSE
options$priorES <- "cauchy"
options$priorH0FE <- 0.25
options$priorH0RE <- 0.25
options$priorH1FE <- 0.25
options$priorH1RE <- 0.25
options$priorSE <- "inverseGamma"
options$reviews <- list(list(metaAnalyses = list(list(check = FALSE, value = "Morbidity"),
                                                 list(check = FALSE, value = "Global Efficacy Question- by Selection"),
                                                 list(check = FALSE, value = "Global Efficacy Question- by Attrition"),
                                                 list(check = FALSE, value = "Global Efficacy Question- by Quality"),
                                                 list(check = FALSE, value = "Global Efficacy Question"),
                                                 list(check = FALSE, value = "Global Efficacy Question- by Sponsor"),
                                                 list(check = TRUE, value = "Global Efficacy Question- Subgroup by PDE-Inhibitor Type")),
                             value = "Phosphodiesterase inhibitors for erectile dysfunction in patients with diabetes mellitus"))
options$selectionType <- "selectionKeywords"
options$shade <- FALSE
options$showLabels <- TRUE
options$textSearch <- ""
options$topicsSelected <- list()
options$upperTrunc <- 0
options$module <- "Cochrane"
options$effectSize <- "effectSizeOR"
options$standardError <- "effectSEOR"
options$studyLabels <- "titleStudy"
set.seed(1)
dataset <- NULL
results <- runAnalysis("CochraneDichotomousBayesianMetaAnalysis", dataset, options)


test_that("Posterior Estimates per Model table results match", {
  skip_on_os(c("mac","linux","solaris"))
  table <- results[["results"]][["modelContainerGlobal Efficacy Question- Subgroup by PDE-Inhibitor Type"]][["collection"]][["modelContainerGlobal Efficacy Question- Subgroup by PDE-Inhibitor Type_bmaTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("TRUE", 40144368330.2797, 0.343509606310007, 0.046736420117505,
                                      0.253150705979028, "Fixed effects", "<unicode><unicode>", 0.436673511146516,
                                      "TRUE", 173.565226006847, 1.61586179483041, 0.355484319096574,
                                      0.870456909977746, "Random effects", "<unicode><unicode>", 2.30299092065303,
                                      1, "FALSE", 3.6548456236652e+38, 0.903096432018545, 0.281174580810072,
                                      0.534802898766207, "Random effects", "<unicode><unicode>", 1.64399208680244,
                                      2, "TRUE", 173.565226006845, 1.61586076997473, 0.355526615960756,
                                      0.879970928392517, "Averaged", "<unicode><unicode>", 2.30262455168115,
                                      3, "FALSE", 3.67590309981904e+38, "", "", "", "Averaged", "<unicode><unicode>",
                                      ""))
})

test_that("Observed study effects plot matches", {
  skip_on_os(c("mac","linux","solaris"))
  plotName <- results[["results"]][["modelContainerGlobal Efficacy Question- Subgroup by PDE-Inhibitor Type"]][["collection"]][["modelContainerGlobal Efficacy Question- Subgroup by PDE-Inhibitor Type_forestContainer"]][["collection"]][["modelContainerGlobal Efficacy Question- Subgroup by PDE-Inhibitor Type_forestContainer_forestPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "observed-study-effects")
})

test_that("Meta-Analyses table results match", {
  skip_on_os(c("mac","linux","solaris"))
  table <- results[["results"]][["selectedOverviewTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(8, "Global Efficacy Question- Subgroup by PDE-Inhibitor Type",
                                      2007))
})
