context("Cochrane Classical Continuous Outcomes")

# Topics & individual ----
options <- analysisOptions("CochraneContinuousClassicalMetaAnalysis")
options$addStudy <- FALSE
options$additionalStudies <- list()
options$analyzeAs <- "OR"
options$analyzeData <- "individually"
options$binWidthType <- "sturges"
options$distPlotDensity <- TRUE
options$distPlotRug <- TRUE
options$forestPlot <- FALSE
options$forestPlotOrder <- "yearDescending"
options$funnelPlot <- FALSE
options$funnelPlotAsymmetryTest <- FALSE
options$keywordsSearch <- ""
options$keywordsSelected <- list()
options$method <- "Restricted ML"
options$modelFit <- FALSE
options$numberOfBins <- 30
options$plotEffectSizes <- TRUE
options$plotResidualsCovariates <- FALSE
options$plotResidualsDependent <- FALSE
options$plotResidualsPredicted <- FALSE
options$plotResidualsQQ <- TRUE
options$plotSampleSizes <- TRUE
options$rSquaredChange <- FALSE
options$regressionCoefficientsConfidenceIntervals <- FALSE
options$regressionCoefficientsConfidenceIntervalsInterval <- 0.95
options$regressionCoefficientsCovarianceMatrix <- FALSE
options$regressionCoefficientsEstimates <- TRUE
options$residualsCasewiseDiagnostics <- FALSE
options$residualsParameters <- TRUE
options$reviews <- list(list(metaAnalyses = list(list(check = FALSE, value = "SGRQ symptoms (6 months)"),
                                                 list(check = FALSE, value = "Courses of antibiotics (12 months)"),
                                                 list(check = FALSE, value = "SGRQ overall score (6 months)"),
                                                 list(check = FALSE, value = "GP visits/phone contacts for COPD (all or urgent)"),
                                                 list(check = FALSE, value = "SGRQ activity limitation (6 months)"),
                                                 list(check = FALSE, value = "SGRQ overall score (12 months)"),
                                                 list(check = FALSE, value = "FEV1 % predicted"), list(check = FALSE,
                                                                                                       value = "SGRQ impact score (6 months)")), value = "Action plans with brief patient education for exacerbations in chronic obstructive pulmonary disease"),
                        list(metaAnalyses = list(list(check = TRUE, value = "SGRQ"),
                                                 list(check = TRUE, value = "CRQ")), value = "Active mind body movement therapies as an adjunct to or in comparison with pulmonary rehabilitation for people with chronic obstructive pulmonary disease"),
                        list(metaAnalyses = list(list(check = TRUE, value = "% Change from baseline ICS dose")),
                             value = "Addition of anti leukotriene agents to inhaled corticosteroids for adults and adolescents with persistent asthma"),
                        list(metaAnalyses = list(list(check = FALSE, value = "Last ICS dose tolerated (mcg) at 12 -24 weeks"),
                                                 list(check = FALSE, value = "% Change from baseline ICS dose at 12 -24 weeks")),
                             value = "Addition of anti leukotriene agents to inhaled corticosteroids for chronic asthma"),
                        list(metaAnalyses = list(list(check = FALSE, value = "CBT + MI vs TAU: Average global functioning (GAF) score at 12 months (low = poor): Allocation concealment")),
                             value = "Addition of anti leukotriene agents to inhaled corticosteroids in children with persistent asthma")
                        )
options$selectionType <- "selectionTopics"
options$showLabels <- TRUE
options$test <- "z"
options$textSearch <- ""
options$topicsSelected <- "Airways"
options$trimFillPlot <- FALSE
options$module <- "Cochrane"
options$dependent <- "effectSize"
options$wlsWeights <- "effectSE"
options$includeConstant <- TRUE
options$studyLabels <- "titleStudy"
options$factors <- list()
options$covariates <- list()
options$modelTerms <- list()
options$components <- list()
set.seed(1)
dataset <- NULL
results <- runAnalysis("CochraneContinuousClassicalMetaAnalysis", dataset, options)


test_that("Coefficients table results match", {
  table <- results[["results"]][["modelContainer% Change from baseline ICS dose"]][["collection"]][["modelContainer% Change from baseline ICS dose_coeffTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-0.101602150877056, "intercept", 0.137056573306695, 0.0683341995931489,
                                      -1.48684189588785))
})

test_that("Effect Size plot matches", {
  plotName <- results[["results"]][["modelContainer% Change from baseline ICS dose"]][["collection"]][["modelContainer% Change from baseline ICS dose_effectSizePlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "effect-size-1.1")
})

test_that("Fixed and Random Effects table results match", {
  table <- results[["results"]][["modelContainer% Change from baseline ICS dose"]][["collection"]][["modelContainer% Change from baseline ICS dose_fixRandTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, "Omnibus test of Model Coefficients", 0.137056573306695, 2.21069882336736,
                                      3, "Test of Residual Heterogeneity", 0.429436952772588, 2.76414137718455
                                 ))
})

test_that("Residual Heterogeneity Estimates table results match", {
  table <- results[["results"]][["modelContainer% Change from baseline ICS dose"]][["collection"]][["modelContainer% Change from baseline ICS dose_residualTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0, "<unicode><unicode><unicode><unicode>", 0, "<unicode><unicode>",
                                      0, "I<unicode><unicode> (%)", 1, "H<unicode><unicode>"))
})

test_that("Sample Size plot matches", {
  plotName <- results[["results"]][["modelContainer% Change from baseline ICS dose"]][["collection"]][["modelContainer% Change from baseline ICS dose_sampleSizePlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "sample-size-1.1")
})

test_that("Coefficients table results match", {
  table <- results[["results"]][["modelContainerCRQ"]][["collection"]][["modelContainerCRQ_coeffTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.034800310159182, "intercept", 0.755622018047872, 0.111813633298433,
                                      0.311234946335204))
})

test_that("Effect Size plot matches", {
  plotName <- results[["results"]][["modelContainerCRQ"]][["collection"]][["modelContainerCRQ_effectSizePlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "effect-size-1.2")
})

test_that("Fixed and Random Effects table results match", {
  table <- results[["results"]][["modelContainerCRQ"]][["collection"]][["modelContainerCRQ_fixRandTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, "Omnibus test of Model Coefficients", 0.755622018047872, 0.0968671918202777,
                                      3, "Test of Residual Heterogeneity", 0.999236143918076, 0.0202872381871127
                                 ))
})

test_that("Residual Heterogeneity Estimates table results match", {
  table <- results[["results"]][["modelContainerCRQ"]][["collection"]][["modelContainerCRQ_residualTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0, "<unicode><unicode><unicode><unicode>", 0, "<unicode><unicode>",
                                      0, "I<unicode><unicode> (%)", 1, "H<unicode><unicode>"))
})

test_that("Sample Size plot matches", {
  plotName <- results[["results"]][["modelContainerCRQ"]][["collection"]][["modelContainerCRQ_sampleSizePlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "sample-size-1.2")
})

test_that("Coefficients table results match", {
  table <- results[["results"]][["modelContainerSGRQ"]][["collection"]][["modelContainerSGRQ_coeffTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-0.113352384024176, "intercept", 0.116727938891481, 0.0722607946231072,
                                      -1.56865676077036))
})

test_that("Effect Size plot matches", {
  plotName <- results[["results"]][["modelContainerSGRQ"]][["collection"]][["modelContainerSGRQ_effectSizePlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "effect-size-1.3")
})

test_that("Fixed and Random Effects table results match", {
  table <- results[["results"]][["modelContainerSGRQ"]][["collection"]][["modelContainerSGRQ_fixRandTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, "Omnibus test of Model Coefficients", 0.11672793889148, 2.46068403311057,
                                      3, "Test of Residual Heterogeneity", 0.85348102548767, 0.783199921949308
                                 ))
})

test_that("Residual Heterogeneity Estimates table results match", {
  table <- results[["results"]][["modelContainerSGRQ"]][["collection"]][["modelContainerSGRQ_residualTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0, "<unicode><unicode><unicode><unicode>", 0, "<unicode><unicode>",
                                      0, "I<unicode><unicode> (%)", 1, "H<unicode><unicode>"))
})

test_that("Sample Size plot matches", {
  plotName <- results[["results"]][["modelContainerSGRQ"]][["collection"]][["modelContainerSGRQ_sampleSizePlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "sample-size-1.3")
})

test_that("Meta-Analyses table results match", {
  table <- results[["results"]][["selectedOverviewTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(4, "SGRQ", 2018, 4, "CRQ", 2018, 4, "% Change from baseline ICS dose",
                                      2017))
})
# Topics & pooled ----
options <- analysisOptions("CochraneContinuousClassicalMetaAnalysis")
options$addStudy <- FALSE
options$additionalStudies <- list()
options$analyzeAs <- "OR"
options$analyzeData <- "pooled"
options$binWidthType <- "sturges"
options$distPlotDensity <- TRUE
options$distPlotRug <- TRUE
options$forestPlot <- FALSE
options$forestPlotOrder <- "yearDescending"
options$funnelPlot <- FALSE
options$funnelPlotAsymmetryTest <- FALSE
options$keywordsSearch <- ""
options$keywordsSelected <- list()
options$method <- "Restricted ML"
options$modelFit <- FALSE
options$numberOfBins <- 30
options$plotEffectSizes <- TRUE
options$plotResidualsCovariates <- FALSE
options$plotResidualsDependent <- FALSE
options$plotResidualsPredicted <- FALSE
options$plotResidualsQQ <- TRUE
options$plotSampleSizes <- TRUE
options$rSquaredChange <- FALSE
options$regressionCoefficientsConfidenceIntervals <- FALSE
options$regressionCoefficientsConfidenceIntervalsInterval <- 0.95
options$regressionCoefficientsCovarianceMatrix <- FALSE
options$regressionCoefficientsEstimates <- TRUE
options$residualsCasewiseDiagnostics <- FALSE
options$residualsParameters <- TRUE
options$reviews <- list(list(metaAnalyses = list(list(check = FALSE, value = "SGRQ symptoms (6 months)"),
                                                 list(check = FALSE, value = "Courses of antibiotics (12 months)"),
                                                 list(check = FALSE, value = "SGRQ overall score (6 months)"),
                                                 list(check = FALSE, value = "GP visits/phone contacts for COPD (all or urgent)"),
                                                 list(check = FALSE, value = "SGRQ activity limitation (6 months)"),
                                                 list(check = FALSE, value = "SGRQ overall score (12 months)"),
                                                 list(check = FALSE, value = "FEV1 % predicted"), list(check = FALSE,
                                                                                                       value = "SGRQ impact score (6 months)")), value = "Action plans with brief patient education for exacerbations in chronic obstructive pulmonary disease"),
                        list(metaAnalyses = list(list(check = TRUE, value = "SGRQ"),
                                                 list(check = TRUE, value = "CRQ")), value = "Active mind body movement therapies as an adjunct to or in comparison with pulmonary rehabilitation for people with chronic obstructive pulmonary disease"),
                        list(metaAnalyses = list(list(check = TRUE, value = "% Change from baseline ICS dose")),
                             value = "Addition of anti leukotriene agents to inhaled corticosteroids for adults and adolescents with persistent asthma"),
                        list(metaAnalyses = list(list(check = FALSE, value = "Last ICS dose tolerated (mcg) at 12 -24 weeks"),
                                                 list(check = FALSE, value = "% Change from baseline ICS dose at 12 -24 weeks")),
                             value = "Addition of anti leukotriene agents to inhaled corticosteroids for chronic asthma"),
                        list(metaAnalyses = list(list(check = FALSE, value = "CBT + MI vs TAU: Average global functioning (GAF) score at 12 months (low = poor): Allocation concealment")),
                             value = "Addition of anti leukotriene agents to inhaled corticosteroids in children with persistent asthma")
                        )
options$selectionType <- "selectionTopics"
options$showLabels <- TRUE
options$test <- "z"
options$textSearch <- ""
options$topicsSelected <- "Airways"
options$trimFillPlot <- FALSE
options$module <- "Cochrane"
options$dependent <- "effectSize"
options$wlsWeights <- "effectSE"
options$includeConstant <- TRUE
options$studyLabels <- "titleStudy"
options$factors <- list()
options$covariates <- list()
options$modelTerms <- list()
options$components <- list()
set.seed(1)
dataset <- NULL
results <- runAnalysis("CochraneContinuousClassicalMetaAnalysis", dataset, options)


test_that("Coefficients table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_coeffTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-0.083770575083655, "intercept", 0.0648790706859764, 0.0453772859700425,
                                      -1.84609046779393))
})

test_that("Coefficients table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_coeffTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-0.083770575083655, "intercept", 0.0648790706859764, 0.0453772859700425,
                                      -1.84609046779393))
})

test_that("Effect Size plot matches", {
  plotName <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_effectSizePlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "effect-size-2")
})

test_that("Fixed and Random Effects table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_fixRandTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, "Omnibus test of Model Coefficients", 0.0648790706859764, 3.40805001527963,
                                      11, "Test of Residual Heterogeneity", 0.934611306232859, 4.92782857033956
                                 ))
})

test_that("Residual Heterogeneity Estimates table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_residualTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0, "<unicode><unicode><unicode><unicode>", 0, "<unicode><unicode>",
                                      0, "I<unicode><unicode> (%)", 1, "H<unicode><unicode>"))
})

test_that("Sample Size plot matches", {
  plotName <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_sampleSizePlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "sample-size-2")
})

test_that("Meta-Analyses table results match", {
  table <- results[["results"]][["selectedOverviewTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(4, "SGRQ", 2018, 4, "CRQ", 2018, 4, "% Change from baseline ICS dose",
                                      2017))
})
#
# Keywords, pooled & full output ----
options <- analysisOptions("CochraneContinuousClassicalMetaAnalysis")
options$addStudy <- FALSE
options$additionalStudies <- list()
options$analyzeAs <- "OR"
options$analyzeData <- "pooled"
options$binWidthType <- "sturges"
options$distPlotDensity <- FALSE
options$distPlotRug <- FALSE
options$forestPlot <- TRUE
options$forestPlotOrder <- "yearDescending"
options$funnelPlot <- TRUE
options$funnelPlotAsymmetryTest <- TRUE
options$keywordsSearch <- ""
options$keywordsSelected <- "Abatacept"
options$method <- "Maximum Likelihood"
options$modelFit <- TRUE
options$numberOfBins <- 30
options$plotEffectSizes <- FALSE
options$plotResidualsCovariates <- TRUE
options$plotResidualsDependent <- TRUE
options$plotResidualsPredicted <- TRUE
options$plotResidualsQQ <- TRUE
options$plotSampleSizes <- FALSE
options$rSquaredChange <- TRUE
options$regressionCoefficientsConfidenceIntervals <- TRUE
options$regressionCoefficientsConfidenceIntervalsInterval <- 0.95
options$regressionCoefficientsCovarianceMatrix <- TRUE
options$regressionCoefficientsEstimates <- TRUE
options$residualsCasewiseDiagnostics <- TRUE
options$residualsParameters <- TRUE
options$reviews <- list(list(metaAnalyses = list(list(check = TRUE, value = "Kidney function_ blood pressure and lipids")),
                             value = "Belatacept for kidney transplant recipients"), list(
                               metaAnalyses = list(list(check = TRUE, value = "Anti-TNF and non anti-TNF - fatigue continuous measures")),
                               value = "Biologic interventions for fatigue in rheumatoid arthritis"))
options$selectionType <- "selectionKeywords"
options$showLabels <- TRUE
options$test <- "z"
options$textSearch <- ""
options$topicsSelected <- list()
options$trimFillPlot <- TRUE
options$module <- "Cochrane"
options$dependent <- "effectSize"
options$wlsWeights <- "effectSE"
options$includeConstant <- TRUE
options$studyLabels <- "titleStudy"
options$factors <- list()
options$covariates <- list()
options$modelTerms <- list()
options$components <- list()
set.seed(1)
dataset <- NULL
results <- runAnalysis("CochraneContinuousClassicalMetaAnalysis", dataset, options)


test_that("Influence Measures table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_casewiseTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(256.758900155193, 0.000212746521701833, 1.03021384352275, 0.0144860611388681,
                                      0.0151499819764649, "H<unicode><unicode>rslev Petersen 2014, (Biologics vs Control)",
                                      0.0859563337471107, 0.0486049133409606, 1.51499819764649, 253.146277349541,
                                      0.0122693158714781, 1.02053262486484, -0.110568956152155, 0.0167711237274225,
                                      "Li 2013, (Biologics vs Control)", -0.853704342379347, 0.0478627679666199,
                                      1.67711237274225, 250.710018083487, 0.0128022381056349, 1.02468839981339,
                                      -0.112804252858522, 0.0191504386437083, "Weinblatt 2013, (Biologics vs Control)",
                                      -0.817771025894424, 0.0479733010746485, 1.91504386437082, 254.930206533847,
                                      0.00568284190405095, 1.02634495653545, -0.0750203646849006,
                                      0.0164469235097369, "Bae 2013, (Biologics vs Control)", -0.599591330180924,
                                      0.048264468912655, 1.64469235097369, 251.728293877233, 0.0205111549350015,
                                      1.01107631249728, -0.143571367020909, 0.0158710715927452, "Strand 2012a, (Biologics vs Control).1",
                                      -1.12006293990438, 0.0473058806134211, 1.58710715927452, 248.90548962394,
                                      0.0249851514481135, 1.01528454074952, 0.158468113913593, 0.0191920096952943,
                                      "Strand 2012b, (Biologics vs Control)", 1.13999316115204, 0.0473558254812876,
                                      1.91920096952943, 252.404289066925, 0.0172100275735526, 1.01426224391838,
                                      -0.131311858655123, 0.0159431228091238, "Choy 2012, (Biologics vs Control)",
                                      -1.02762798609573, 0.0475090747255353, 1.59431228091238, 256.304173014214,
                                      0.00252153843164606, 1.03802131779359, 0.0496782445563334, 0.019834185828231,
                                      "Pope 2012, (Biologics vs Control)", 0.31856365106424, 0.0487991601253542,
                                      1.9834185828231, 256.39267174296, 0.000683484169222845, 1.03063393863501,
                                      -0.0259603769668349, 0.0158982542895322, "Strand 2012a, (Biologics vs Control).2",
                                      -0.233548035062863, 0.048581860456093, 1.58982542895322, 244.411692122249,
                                      0.0442842582381455, 0.9934009102901, -0.213283902552038, 0.0175971827192768,
                                      "Rigby 2011, (Biologics vs Control).1", -1.53976602679983, 0.0460356510067566,
                                      1.75971827192768, 241.593531622692, 0.0561345805200157, 0.983368404091018,
                                      -0.241479404468332, 0.0175980025086993, "Rigby 2011, (Biologics vs Control).2",
                                      -1.72652740545523, 0.0453814026448671, 1.75980025086993, 256.301533668858,
                                      0.0017385211525267, 1.01756469509878, -0.0415998033453311, 0.00994672261225097,
                                      "Alten 2011, (Biologics vs Control).1", -0.43339108810735, 0.0481280442788687,
                                      0.994672261225097, 256.684946906251, 0.000116712981504096, 1.01964254857096,
                                      -0.0107717521780193, 0.0100286830388844, "Alten 2011, (Biologics vs Control).2",
                                      -0.131164718503263, 0.0482586317139275, 1.00286830388844, 256.737077527907,
                                      0.000425934438086632, 1.01952460691187, 0.0205784509159351,
                                      0.00991381712525428, "Alten 2011, (Biologics vs Control).3",
                                      0.180261885723303, 0.0482586371547993, 0.991381712525428, 250.280737381386,
                                      0.0293972705902594, 1.00631287560011, 0.1724636976671, 0.0170978645439571,
                                      "BENEFIT EXT 2009, (High dosage belatacept vs Low dosage belatacept).1",
                                      1.32908864590372, 0.0469120448494258, 1.70978645439571, 249.053965494682,
                                      0.0351441445100713, 0.999659203728801, 0.189181498642784, 0.016819493442218,
                                      "BENEFIT EXT 2009, (High dosage belatacept vs Low dosage belatacept).2",
                                      1.48110886906999, 0.0464966328526472, 1.6819493442218, 246.561736092165,
                                      0.0436727922873569, 0.99100314285972, 0.211933857891445, 0.0170800518876081,
                                      "BENEFIT EXT 2009, (High dosage belatacept vs Low dosage belatacept).3",
                                      1.65956224074349, 0.0459143451170016, 1.70800518876081, 250.280737381386,
                                      0.0293972705902606, 1.00631287560011, 0.172463697667103, 0.0170978645439571,
                                      "BENEFIT EXT 2009, (High dosage belatacept vs Low dosage belatacept).4",
                                      1.32908864590372, 0.0469120448494258, 1.70978645439571, 246.602378453769,
                                      0.0383963018614036, 0.998840444481388, 0.198113955260577, 0.0180702547902162,
                                      "BENEFIT EXT 2009, (High dosage belatacept vs Low dosage belatacept).5",
                                      1.4956963963809, 0.0463583798171249, 1.80702547902162, 238.992004804505,
                                      0.062570064201187, 0.971919835940184, 0.256848736314587, 0.0180626023675977,
                                      "BENEFIT EXT 2009, (High dosage belatacept vs Low dosage belatacept).6",
                                      1.97857416394165, 0.0446036812620991, 1.80626023675977, 251.175026622074,
                                      0.0309682260691599, 0.989164402665073, 0.177068435059677, 0.00967374849647508,
                                      "Soubrier 2009, (Biologics vs Control)", 1.86328063965825, 0.0462873043892066,
                                      0.967374849647508, 254.225632165082, 0.0110113571130121, 1.01660400974702,
                                      -0.10483249707997, 0.0141529159146166, "Keystone 2009, (Biologics vs Control).1",
                                      -0.879631392151086, 0.0477825993372958, 1.41529159146166, 254.772271332171,
                                      0.00707520959494861, 1.02337527967018, -0.0838132119274531,
                                      0.0156488328047341, "Smolen 2009b, (Biologics vs Control).1",
                                      -0.680046135496165, 0.048124264763368, 1.56488328047341, 246.218917420245,
                                      0.0492648733825903, 0.985247161046461, -0.225270939065758, 0.0155586767916571,
                                      "Smolen 2009a, (Biologics vs Control).1", -1.72654154307625,
                                      0.0456416654813221, 1.55586767916571, 256.260765377887, 0.00113395139044933,
                                      1.02976712205954, -0.0334518351793638, 0.0157348429647496, "Smolen 2009b, (Biologics vs Control).2",
                                      -0.292592865101194, 0.0485362266132882, 1.57348429647496, 256.091034644557,
                                      0.00162870120367314, 1.02981743441052, -0.0400907511165857,
                                      0.0160442189632592, "Fleischmann 2009, (Biologics vs Control)",
                                      -0.341244706342657, 0.0485186293208314, 1.60442189632592, 253.611956959838,
                                      0.0122905870150311, 1.01822668122658, -0.110736832394551, 0.0156048087782541,
                                      "Smolen 2009a, (Biologics vs Control).2", -0.884414056275843,
                                      0.0477908382979721, 1.56048087782541, 254.451539245919, 0.00983435879917216,
                                      1.01783893273627, -0.0990201398281287, 0.0142059778424426, "Keystone 2009, (Biologics vs Control).2",
                                      -0.832019354216927, 0.047859722243755, 1.42059778424426, 248.645624180925,
                                      0.0367474432043119, 0.995939151040742, -0.193537225291285, 0.0155318732327688,
                                      "Smolen 2009a, (Biologics vs Control).3", -1.49895460112772,
                                      0.0463407507435136, 1.55318732327688, 252.754764732988, 0.0161841612938196,
                                      1.01464394040478, -0.127293801709379, 0.0156491018261956, "Smolen 2009a, (Biologics vs Control).4",
                                      -1.00698036075716, 0.0475538272238038, 1.56491018261956, 252.472693782621,
                                      0.0172659009977905, 1.0214934540948, 0.131199054919631, 0.0184485541789981,
                                      "Emery 2009, (Biologics vs Control)", 0.952220947082674, 0.0478120482921233,
                                      1.84485541789981, 255.711369411296, 0.00215326629200768, 1.03232706448763,
                                      -0.046045408010131, 0.0175997472080881, "Strand 2009, (Biologics vs Control).1",
                                      -0.3722423463572, 0.0485776455076052, 1.75997472080881, 255.059191136231,
                                      0.00415725578660867, 1.03010157291772, -0.0640550570872008,
                                      0.0175559425762397, "Strand 2009, (Biologics vs Control).2",
                                      -0.503383352739107, 0.0484351031508587, 1.75559425762396, 248.028573494799,
                                      0.0331497705248206, 1.00464607850675, 0.183497408511907, 0.0181727401315798,
                                      "BENEFIT Study 2008, (High dosage belatacept vs Low dosage belatacept).1",
                                      1.37356257840171, 0.0467303395448194, 1.81727401315797, 239.006777015744,
                                      0.060418702918177, 0.974881946884972, 0.252071759869593, 0.0182837025225947,
                                      "BENEFIT Study 2008, (High dosage belatacept vs Low dosage belatacept).2",
                                      1.9250711330541, 0.0447815289962711, 1.82837025225947, 247.708159176817,
                                      0.0335731519624169, 1.0044474787115, 0.184708650438714, 0.0182898262998734,
                                      "BENEFIT Study 2008, (High dosage belatacept vs Low dosage belatacept).3",
                                      1.37837479301356, 0.0467094128504004, 1.82898262998734, 252.653521611606,
                                      0.0169113875933326, 1.02164349534706, 0.129827925539508, 0.0183523765851441,
                                      "BENEFIT Study 2008, (High dosage belatacept vs Low dosage belatacept).4",
                                      0.944270073787882, 0.0478283717130524, 1.83523765851441, 246.902388598279,
                                      0.0400690684594366, 0.995969108702242, 0.202583828908812, 0.0175697731428361,
                                      "BENEFIT Study 2008, (High dosage belatacept vs Low dosage belatacept).5",
                                      1.55607927841781, 0.0462050452800917, 1.75697731428361, 249.280606663419,
                                      0.0315246436504047, 1.00508264550602, 0.178792327675025, 0.0175708557919741,
                                      "BENEFIT Study 2008, (High dosage belatacept vs Low dosage belatacept).6",
                                      1.36088092934539, 0.0467997002840793, 1.75708557919741, 255.916430173687,
                                      0.00276341998539933, 1.02573512781641, -0.0523124381606421,
                                      0.0146109456997054, "Schiff 2008, (Biologics vs Control).1",
                                      -0.452375539328667, 0.0483485018793264, 1.46109456997054, 256.715886463744,
                                      3.10092056690397e-07, 1.02933695460223, -0.000553264193689591,
                                      0.0147262709451404, "Schiff 2008, (Biologics vs Control).2",
                                      -0.034806746134404, 0.0485761859944575, 1.47262709451404, 255.604098061664,
                                      0.00388538982935963, 1.0255381284042, -0.0620396176742813, 0.0151092310232268,
                                      "Smolen 2008, (Biologics vs Control).1", -0.521929800343459,
                                      0.0483020152794757, 1.51092310232268, 250.539808905952, 0.00756018682136849,
                                      1.03212395268876, -0.0863370319670833, 0.0203356705769133, "Genovese 2008, (Biologics vs Control)",
                                      -0.621519686341653, 0.0483792891532524, 2.03356705769133, 255.5514567218,
                                      0.00549562665406604, 1.03424165699541, 0.0734985553892885, 0.0192256478186889,
                                      "Emery 2008, (Biologics vs Control)", 0.499514499134782, 0.0485929768190185,
                                      1.92256478186889, 256.27065227992, 0.00125669041314626, 1.02811913725309,
                                      -0.0352408036129247, 0.0149799544759186, "Smolen 2008, (Biologics vs Control).2",
                                      -0.312538531551639, 0.0484794484072744, 1.49799544759186, 253.079428801677,
                                      0.0152411435523043, 1.01486541999421, -0.123494765622625, 0.0153108493086891,
                                      "Mittendorf 2007, (Biologics vs Control)", -0.988958523844987,
                                      0.0475910847708756, 1.53108493086891, 256.422944972705, 0.00223380497571596,
                                      1.03751441802125, 0.0467707398886743, 0.019469241055572, "Kremer 2006, (Biologics vs Control)",
                                      0.300972648808837, 0.0487906398503882, 1.9469241055572, 256.712634789136,
                                      1.51356100219563e-06, 1.02916104511727, -0.00122241988256949,
                                      0.0146445859241629, "Emery 2006, (Biologics vs Control).1",
                                      -0.0401970533096796, 0.048570198717454, 1.46445859241629, 256.28087462902,
                                      0.00128608348844561, 1.02738977728451, -0.0356612829184427,
                                      0.0146275507145049, "Emery 2006, (Biologics vs Control).2",
                                      -0.318950289972114, 0.0484555477244226, 1.46275507145048, 252.733808605944,
                                      0.00847974274876072, 1.02838956622099, -0.091600707175371, 0.0189003929756813,
                                      "Cohen 2006, (Biologics vs Control)", -0.67811802022094, 0.0482322410806157,
                                      1.89003929756813, 251.931436874972, 0.0275393080670717, 0.993436575270697,
                                      0.166807316941672, 0.00999444278343705, "Vincenti 2005, (High dosage belatacept vs Low dosage belatacept).1",
                                      1.71560078181525, 0.046545640495785, 0.999444278343704, 254.599448379396,
                                      0.0140346865828046, 1.01353270064432, 0.118454438518125, 0.0130730904974737,
                                      "Vincenti 2005, (High dosage belatacept vs Low dosage belatacept).2",
                                      1.02850369933812, 0.0476544494058704, 1.30730904974737, 253.055428660162,
                                      0.0101345641584019, 1.02479308884698, -0.100308371304219, 0.0178945875290149,
                                      "Genovese 2005, (Biologics vs Control)", -0.756371413744647,
                                      0.048065208737567, 1.78945875290149, 256.181266335117, 0.00112580425291928,
                                      1.03185343844702, -0.0332998119640743, 0.0167762672778823, "Keystone 2004, (Biologics vs Control).1",
                                      -0.284252010301057, 0.0486022993536439, 1.67762672778823, 256.707469499257,
                                      2.70536105218194e-06, 1.03360762862349, 0.00163093476235796,
                                      0.0168377663387273, "Keystone 2004, (Biologics vs Control).2",
                                      -0.0198622221893385, 0.0487128369525161, 1.68377663387273, 256.038047897218,
                                      0.00550286420940921, 1.0241982529187, 0.0738673074704755, 0.0143178018140842,
                                      "Kremer 2003, (Biologics vs Control).1", 0.591482581952114,
                                      0.0482678075713755, 1.43178018140842, 251.700286536309, 0.0240050856352933,
                                      1.00485793596352, -0.155597347639492, 0.0143014771175358, "Kremer 2003, (Biologics vs Control).2",
                                      -1.27170619621916, 0.0470054158989944, 1.43014771175358, 254.501714808653,
                                      0.0111200800506845, 1.00715807573373, -0.105521638041665, 0.00936223167187539,
                                      "Weinblatt 2003, (Biologics vs Control).1", -1.08023189816352,
                                      0.0474855686479746, 0.936223167187539, 256.168561684511, 0.0023897511787245,
                                      1.01602589164234, -0.0487932997037612, 0.00951967343218036,
                                      "Weinblatt 2003, (Biologics vs Control).2", -0.513496722038519,
                                      0.0480559233680478, 0.951967343218035, 256.75519510781, 2.27009888121777e-05,
                                      1.01913352424598, 0.00475129923926994, 0.0096296802460906, "Weinblatt 2003, (Biologics vs Control).3",
                                      0.0232436348307834, 0.0482521386025835, 0.96296802460906, 256.389928325766,
                                      0.00133805677828707, 1.01726946515164, -0.036497650788116, 0.0095783430837501,
                                      "Weinblatt 2003, (Biologics vs Control).4", -0.390130883625723,
                                      0.0481334583561783, 0.95783430837501, 256.21817370403, 0.00214930588600869,
                                      1.01628705718995, -0.0462700486533732, 0.00952369203750704,
                                      "Weinblatt 2003, (Biologics vs Control).5", -0.488349560282988,
                                      0.0480727643254115, 0.952369203750703, 254.196636489082, 0.012786097997586,
                                      1.00556729079197, -0.113201844749441, 0.00934655796849475, "Weinblatt 2003, (Biologics vs Control).6",
                                      -1.15673373688889, 0.0473824311310443, 0.934655796849475, 256.74526563272,
                                      1.03273942637787e-05, 1.02462908489981, 0.00319891676669047,
                                      0.0123652783848771, "Moreland 1999, (Biologics vs Control).1",
                                      0.000575959511724982, 0.0484276079576015, 1.23652783848771,
                                      256.755977380097, 6.21198516769908e-05, 1.02455606067795, 0.00784574766962764,
                                      0.0123206015942062, "Moreland 1999, (Biologics vs Control).2",
                                      0.0421300366267858, 0.0484258433748944, 1.23206015942062))
})

test_that("Coefficients table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_coeffTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-0.326616278353223, -0.390496595522356, "intercept", 1.23025470307434e-23,
                                      0.0325925973100438, -0.262735961184089, -10.0211798171903))
})

test_that("Parameter Covariances table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_covMatTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.00106227739941467, "intercept"))
})

test_that("File Drawer Analysis table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_failSafeTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.05, 8843, "Rosenthal", 6.39169413508078e-83))
})

test_that("Fit measures table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_fitMeasuresTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-5.06670886387491, "Log-Likelihood", 145.462444432728, "Deviance",
                                      14.1334177277498, "AIC", 18.4821922675411, "BIC", 14.3269661148466,
                                      "AICc"))
})

test_that("Fixed and Random Effects table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_fixRandTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, "Omnibus test of Model Coefficients", 1.23025470307435e-23,
                                      100.424044928463, 64, "Test of Residual Heterogeneity", 6.49380648090064e-25,
                                      256.758902309596))
})

test_that("Diagnostic Plots matches", {
  plotName <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_plots"]][["collection"]][["modelContainer_plots_diagnosticPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "diagnostic-plots-3")
})

test_that("Forest plot matches", {
  plotName <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_plots"]][["collection"]][["modelContainer_plots_forest"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "forest-plot-3")
})

test_that("Funnel Plot matches", {
  plotName <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_plots"]][["collection"]][["modelContainer_plots_funnel"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "funnel-plot-3")
})

test_that("Log-likelihood for Ï„Â² plot matches", {
  plotName <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_plots"]][["collection"]][["modelContainer_plots_profile"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "log-likelihood-for-3")
})

test_that("Trim-fill Analysis plot matches", {
  plotName <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_plots"]][["collection"]][["modelContainer_plots_trimFill"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "trim-fill-analysis-3")
})

test_that("Rank correlation test for Funnel plot asymmetry table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_rankTestTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-0.0851370851370851, "Rank test", 0.316296608322314))
})

test_that("Regression test for Funnel plot asymmetry (Egger's test) table results match", {
	table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_regTestTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list("sei", 0.145651238128508, -1.45506510624405))
})

test_that("Residual Heterogeneity Estimates table results match", {
	table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_residualTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(0.0476357293978005, 0.0288045743344235, "<unicode><unicode><unicode><unicode>",
			 0.0771458008910889, 0.218256109645986, 0.169719104211705, "<unicode><unicode>",
			 0.277751329233703, 74.5597146217595, 63.9274692960105, "I<unicode><unicode> (%)",
			 82.5977141620755, 3.93077351583216, 2.77219252568105, "H<unicode><unicode>",
			 5.74637153597788))
})

test_that("Meta-Analyses table results match", {
	table <- results[["results"]][["selectedOverviewTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(14, "Kidney function_ blood pressure and lipids", 2014, 51, "Anti-TNF and non anti-TNF - fatigue continuous measures",
			 2016))
})
# Search & adding estimates ----
options <- analysisOptions("CochraneContinuousClassicalMetaAnalysis")
options$addStudy <- TRUE
options$additionalStudies <- list(list(effectSE = "0.69", effectSize = "0.666", lCI = "",
                                       n1 = "", n2 = "", name = "#", titleStudy = "New study 42", uCI = "",
                                       x1 = "", x2 = ""))
options$analyzeAs <- "OR"
options$analyzeData <- "individually"
options$binWidthType <- "sturges"
options$distPlotDensity <- FALSE
options$distPlotRug <- FALSE
options$forestPlot <- TRUE
options$forestPlotOrder <- "yearDescending"
options$funnelPlot <- FALSE
options$funnelPlotAsymmetryTest <- FALSE
options$keywordsSearch <- ""
options$keywordsSelected <- list()
options$method <- "Restricted ML"
options$modelFit <- FALSE
options$numberOfBins <- 30
options$plotEffectSizes <- FALSE
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
options$reviews <- list(list(metaAnalyses = list(list(check = TRUE, value = "Time to clinical cure - children")),
                             value = "Corticosteroids for pneumonia"), list(metaAnalyses = list(
                               list(check = FALSE, value = "Depression score sub-group analysis children vs adults")),
                               value = "Dance movement therapy for depression"), list(metaAnalyses = list(
                                 list(check = FALSE, value = "Adults versus children")), value = "Dietary marine fatty acids (fish oil) for asthma in adults and children"),
                        list(metaAnalyses = list(list(check = FALSE, value = "Movement Assessment Battery for Children (MABC): Total score")),
                             value = "Immunotherapy for diabetic amyotrophy"), list(
                               metaAnalyses = list(list(check = FALSE, value = "Comparison 1. Children vs adults"),
                                                   list(check = FALSE, value = "Comparison 2. Children vs adults")),
                               value = "Interventions to improve adherence to inhaled steroids for asthma"),
                        list(metaAnalyses = list(list(check = FALSE, value = "Haemoglobin by age of children at start of intervention")),
                             value = "Probiotics for vulvovaginal candidiasis in non pregnant women"),
                        list(metaAnalyses = list(list(check = FALSE, value = "Subgroup analysis (excluding data from children with expressive and receptive difficulties)")),
                             value = "Speech and language therapy interventions for children with primary speech and language delay or disorder"),
                        list(metaAnalyses = list(list(check = FALSE, value = "Total ocular symptom scores: Adults and children")),
                             value = "Sublingual immunotherapy for treating allergic conjunctivitis"),
                        list(metaAnalyses = list(list(check = FALSE, value = "Wechsler Intelligence Scale for Children at 7 year follow up")),
                             value = "Vitamin K prior to preterm birth for preventing neonatal periventricular haemorrhage"),
                        list(metaAnalyses = list(list(check = FALSE, value = "Change in proportion of caries free children (deciduous teeth)"),
                                                 list(check = FALSE, value = "Change in proportion of caries free children (permanent teeth)")),
                             value = "Water fluoridation for the prevention of dental caries"))
options$selectionType <- "selectionTextSearch"
options$showLabels <- TRUE
options$test <- "z"
options$textSearch <- "children"
options$topicsSelected <- list()
options$trimFillPlot <- FALSE
options$module <- "Cochrane"
options$dependent <- "effectSize"
options$wlsWeights <- "effectSE"
options$includeConstant <- TRUE
options$studyLabels <- "titleStudy"
options$factors <- list()
options$covariates <- list()
options$modelTerms <- list()
options$components <- list()
set.seed(1)
dataset <- NULL
results <- runAnalysis("CochraneContinuousClassicalMetaAnalysis", dataset, options)


test_that("Coefficients table results match", {
  table <- results[["results"]][["modelContainerTime to clinical cure - children"]][["collection"]][["modelContainerTime to clinical cure - children_coeffTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-0.503005116461763, "intercept", 0.10555344417809, 0.310783462673643,
                                      -1.61850669959867))
})

test_that("Fixed and Random Effects table results match", {
  table <- results[["results"]][["modelContainerTime to clinical cure - children"]][["collection"]][["modelContainerTime to clinical cure - children_fixRandTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, "Omnibus test of Model Coefficients", 0.10555344417809, 2.61956393664577,
                                      4, "Test of Residual Heterogeneity", 0.00106445593827858, 18.3283183886617
                                 ))
})

test_that("Forest plot matches", {
  plotName <- results[["results"]][["modelContainerTime to clinical cure - children"]][["collection"]][["modelContainerTime to clinical cure - children_plots"]][["collection"]][["modelContainerTime to clinical cure - children_plots_forest"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "forest-plot-4")
})

test_that("Residual Heterogeneity Estimates table results match", {
  table <- results[["results"]][["modelContainerTime to clinical cure - children"]][["collection"]][["modelContainerTime to clinical cure - children_residualTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.364802042504976, "<unicode><unicode><unicode><unicode>", 0.603988445671749,
                                      "<unicode><unicode>", 80.816211136108, "I<unicode><unicode> (%)",
                                      5.21273460156879, "H<unicode><unicode>"))
})

test_that("Meta-Analyses table results match", {
  table <- results[["results"]][["selectedOverviewTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(4, "Time to clinical cure - children", 2017))
})
