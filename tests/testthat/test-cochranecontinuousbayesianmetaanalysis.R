context("Cochrane Bayesian Continuous Outcomes")

# Most of the interface is common across the modules and already tested within Cochrane Classical Continuous Outcomes.
# This tests tries to checks for any disconnects between the imported GUI, modified options, and R-code.

# output from BMA (mostly skipped on non-windows) ----
options <- analysisOptions("CochraneContinuousBayesianMetaAnalysis")
options$BFComputation <- "integration"
options$addInfo <- TRUE
options$addLines <- TRUE
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
options$checkUpperPrior <- TRUE
options$direction <- "allNeg"
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
options$iterMCMC <- 10000
options$keywordsSearch <- ""
options$keywordsSelected <- "25-Hydroxyvitamin D 2"
options$lowerTrunc <- 0
options$method <- "Restricted ML"
options$modelSpecification <- "CRE"
options$numberOfBins <- 30
options$orderForest <- "ascendingForest"
options$plotCumForest <- FALSE
options$plotEffectSizes <- FALSE
options$plotPosterior <- TRUE
options$plotPrior <- FALSE
options$plotSampleSizes <- FALSE
options$plotSeqPM <- FALSE
options$plotSequential <- FALSE
options$postTable <- TRUE
options$priorES <- "cauchy"
options$priorH0FE <- 0
options$priorH0RE <- 0
options$priorH1FE <- 0
options$priorH1RE <- 0
options$priorSE <- "inverseGamma"
options$reviews <- list(list(metaAnalyses = list(list(check = TRUE, value = "Vitamin D given to infants compared to placebo or no treatment"),
                                                 list(check = FALSE, value = "Vitamin D given to lactating mothers compared to placebo or no treatment"),
                                                 list(check = FALSE, value = "Vitamin D given to infants compared to vitamin D given to lactating mothers"),
                                                 list(check = FALSE, value = "Vitamin D given to infants compared to placebo or no treatment: subgroup and sensitivity analyses"),
                                                 list(check = FALSE, value = "Vitamin D given to lactating mothers compared to placebo or no treatment: subgroup and sensitivity analyses"),
                                                 list(check = FALSE, value = "Vitamin D given to infants compared to vitamin D given to lactating mothers: sensitivity analysis")),
                             value = "Vitamin D supplementation for term breastfed infants to prevent vitamin D deficiency and improve bone health"),
                        list(metaAnalyses = list(list(check = FALSE, value = "Vitamin D supplementation vs. placebo or standard care alone for individuals with moderate to severe disease")),
                             value = "Vitamin D supplementation for the treatment of COVID-19: a living systematic review"))
options$selectionType <- "selectionKeywords"
options$shade <- TRUE
options$showLabels <- TRUE
options$textSearch <- ""
options$topicsSelected <- list()
options$upperTrunc <- 0
set.seed(1)
dataset <- NULL
results <- runAnalysis("CochraneContinuousBayesianMetaAnalysis", dataset, options)


test_that("Posterior Estimates per Model table results match", {
  skip_on_os(c("mac","linux","solaris"))
  table <- results[["results"]][["modelContainerVitamin D given to infants compared to placebo or no treatment"]][["collection"]][["modelContainerVitamin D given to infants compared to placebo or no treatment_bmaTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("TRUE", 2060949.62955526, -0.414167462899442, 0.0720261587969591,
                                      -0.55530638336358, "Fixed effects", "<unicode><unicode>", -0.273173486477111,
                                      1, "TRUE", 155529816.348223, -0.515217675299269, 0.112358976317396,
                                      -0.767084868849968, "Ordered effects", "<unicode><unicode>",
                                      -0.325538505016017, 2, "FALSE", 75.4651225424592, 0.286230036202228,
                                      0.104061970032716, 0.116386575053398, "Ordered effects", "<unicode><unicode>",
                                      0.527909459154274, "TRUE", 1493798968.44883, -0.422622229775255,
                                      0.149423168836986, -0.725903785622723, "Random effects", "<unicode><unicode>",
                                      -0.13495539490391, 2, "FALSE", 724.811003154496, 0.461525661260771,
                                      0.150737344884048, 0.213889774269446, "Random effects", "<unicode><unicode>",
                                      0.80753605573426))
})

test_that("Effect Sizes per Study table results match", {
  skip_on_os(c("mac","linux","solaris"))
  table <- results[["results"]][["modelContainerVitamin D given to infants compared to placebo or no treatment"]][["collection"]][["modelContainerVitamin D given to infants compared to placebo or no treatment_esTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-0.71564590003872, -1.39504611746805, -0.20821623297545, -1.58708972467392,
                                      "Greer 1981 (Vitamin D infant vs Control)", -0.179095075023329,
                                      -0.493059405258439, -0.00710515088855622, 0.729272057876597,
                                      "Greer 1989 (Vitamin D infant vs Control)", -0.574905326538432,
                                      -1.12991534560125, -0.122424175354784, -0.842251105756879, "Alonso 2011 (Vitamin D infant vs Control)",
                                      -0.545713987289034, -0.88570351407723, -0.225591216742649, -0.596406137810261,
                                      "Chandy 2016 (Vitamin D infant vs Control)", -0.747899115802573,
                                      -1.30128458329422, -0.288463341528319, -1.20227996142515, "Greer 1989 (Vitamin D infant vs Control)",
                                      -0.559672446786934, -1.14603218341697, -0.103076906877426, -0.859502002588431,
                                      "Madar 2009 (Vitamin D infant vs Control)", -0.605284507613092,
                                      -1.17855331147401, -0.147487566526947, -0.953091989898511, "Moodley 2015 (Vitamin D infant vs Control)",
                                      -0.787247387001051, -1.12581156027534, -0.459940377974656, -0.943271570519118,
                                      "Rueter 2019 (Vitamin D infant vs Control)", -0.325355520697068,
                                      -0.626654026673559, -0.0485676862651595, -0.230787975412515,
                                      "Chandy 2016 (Vitamin D infant vs Control)", -0.272256916208013,
                                      -0.643729156158691, -0.0157603913073224, 0.172521303407793,
                                      "Greer 1989 (Vitamin D infant vs Control)", -0.451389502587322,
                                      -0.76977177851575, -0.1446903245098, -0.445812559197388, "Chandy 2016 (Vitamin D infant vs Control)",
                                      -0.514851928370068, -1.07770869273127, -0.0714350741355148,
                                      -0.616899730177131, "Greer 1981 (Vitamin D infant vs Control)",
                                      -0.264100111429299, -0.618845543630795, -0.0160363581355242,
                                      0.217072903709612, "Greer 1989 (Vitamin D infant vs Control)",
                                      -0.213423135455196, -0.494577882879403, -0.0131261923419128,
                                      0, "Chandy 2016 (Vitamin D infant vs Control)"))
})

test_that("Observed study effects plot matches", {
  skip_on_os(c("mac","linux","solaris"))
  plotName <- results[["results"]][["modelContainerVitamin D given to infants compared to placebo or no treatment"]][["collection"]][["modelContainerVitamin D given to infants compared to placebo or no treatment_forestContainer"]][["collection"]][["modelContainerVitamin D given to infants compared to placebo or no treatment_forestContainer_forestPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "observed-study-effects")
})

test_that("Effect size plot matches", {
  skip_on_os(c("mac","linux","solaris"))
  plotName <- results[["results"]][["modelContainerVitamin D given to infants compared to placebo or no treatment"]][["collection"]][["modelContainerVitamin D given to infants compared to placebo or no treatment_postContainer"]][["collection"]][["modelContainerVitamin D given to infants compared to placebo or no treatment_postContainer_ES"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "effect-size")
})

test_that("Heterogeneity plot matches", {
  skip_on_os(c("mac","linux","solaris"))
  plotName <- results[["results"]][["modelContainerVitamin D given to infants compared to placebo or no treatment"]][["collection"]][["modelContainerVitamin D given to infants compared to placebo or no treatment_postContainer"]][["collection"]][["modelContainerVitamin D given to infants compared to placebo or no treatment_postContainer_SE"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "heterogeneity")
})

test_that("Model Probabilities table results match", {
  skip_on_os(c("mac","linux","solaris"))
  table <- results[["results"]][["modelContainerVitamin D given to infants compared to placebo or no treatment"]][["collection"]][["modelContainerVitamin D given to infants compared to placebo or no treatment_postTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Fixed H<unicode><unicode><unicode>", 6.05550572676694e-10, 0.25,
                                      "Fixed H<unicode><unicode><unicode>", 0.00124800922843501, 0.25,
                                      "Ordered H<unicode><unicode><unicode>", 0.0941811693579678,
                                      0.25, "Random H<unicode><unicode><unicode>", 0.904570820808047,
                                      0.25))
})

test_that("Meta-Analyses table results match", {
  skip_on_os(c("mac","linux","solaris"))
  table <- results[["results"]][["selectedOverviewTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(14, "Vitamin D given to infants compared to placebo or no treatment",
                                      2020))
})
