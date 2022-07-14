context("Cochrane Classical Continuous Outcomes")

# Topics & individual ----
options <- analysisOptions("CochraneContinuousClassicalMetaAnalysis")
options$savePath <- ""
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
options$reviews <- list(list(metaAnalyses = list(list(check = FALSE, value = "All caffeine doses versus placebo")),
                             value = "Caffeine for asthma"), list(metaAnalyses = list(
                               list(check = FALSE, value = "Diamox versus control")),
                               value = "Carbonic anhydrase inhibitors for hypercapnic ventilatory failure in chronic obstructive pulmonary disease"),
                        list(metaAnalyses = list(list(check = TRUE, value = "Beta-blocker vs Placebo"),
                                                 list(check = TRUE, value = "Beta-blocker + agonist vs Placebo + agonist")),
                             value = "Cardioselective beta-blockers for chronic obstructive pulmonary disease"),
                        list(metaAnalyses = list(list(check = TRUE, value = "Beta-blocker vs placebo: Single dose"),
                                                 list(check = FALSE, value = "Beta-blocker + agonist vs placebo + agonist: Single dose"),
                                                 list(check = FALSE, value = "Beta-blocker vs placebo: Longer duration"),
                                                 list(check = FALSE, value = "Beta-blocker + agonist vs placebo + agonist: Longer duration")),
                             value = "Cardioselective beta-blockers for reversible airway disease")
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
  table <- results[["results"]][["modelContainerBeta-blocker + agonist vs Placebo + agonist"]][["collection"]][["modelContainerBeta-blocker + agonist vs Placebo + agonist_coeffTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.0671412793022783, "intercept", 0.700215879124941, 0.174379767138875,
                                      0.385029068474483))
})

test_that("Effect Size plot matches", {
  plotName <- results[["results"]][["modelContainerBeta-blocker + agonist vs Placebo + agonist"]][["collection"]][["modelContainerBeta-blocker + agonist vs Placebo + agonist_effectSizePlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "effect-size-1")
})

test_that("Fixed and Random Effects table results match", {
  table <- results[["results"]][["modelContainerBeta-blocker + agonist vs Placebo + agonist"]][["collection"]][["modelContainerBeta-blocker + agonist vs Placebo + agonist_fixRandTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, "Omnibus test of Model Coefficients", 0.700215879124941, 0.148247383570328,
                                      4, "Test of Residual Heterogeneity", 0.840651306213028, 1.42034531625708
                                 ))
})

test_that("Residual Heterogeneity Estimates table results match", {
  table <- results[["results"]][["modelContainerBeta-blocker + agonist vs Placebo + agonist"]][["collection"]][["modelContainerBeta-blocker + agonist vs Placebo + agonist_residualTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0, "<unicode><unicode><unicode><unicode>", 0, "<unicode><unicode>",
                                      0, "I<unicode><unicode> (%)", 1, "H<unicode><unicode>"))
})

test_that("Sample Size plot matches", {
  plotName <- results[["results"]][["modelContainerBeta-blocker + agonist vs Placebo + agonist"]][["collection"]][["modelContainerBeta-blocker + agonist vs Placebo + agonist_sampleSizePlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "sample-size-1")
})

test_that("Coefficients table results match", {
  table <- results[["results"]][["modelContainerBeta-blocker vs Placebo"]][["collection"]][["modelContainerBeta-blocker vs Placebo_coeffTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.26730424969743, "intercept", 0.00730813643062205, 0.0996486633113405,
                                      2.68246698766314))
})

test_that("Effect Size plot matches", {
  plotName <- results[["results"]][["modelContainerBeta-blocker vs Placebo"]][["collection"]][["modelContainerBeta-blocker vs Placebo_effectSizePlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "effect-size-2")
})

test_that("Fixed and Random Effects table results match", {
  table <- results[["results"]][["modelContainerBeta-blocker vs Placebo"]][["collection"]][["modelContainerBeta-blocker vs Placebo_fixRandTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, "Omnibus test of Model Coefficients", 0.00730813643062204,
                                      7.19562913990254, 20, "Test of Residual Heterogeneity", 0.209912229340732,
                                      24.7816601131121))
})

test_that("Residual Heterogeneity Estimates table results match", {
  table <- results[["results"]][["modelContainerBeta-blocker vs Placebo"]][["collection"]][["modelContainerBeta-blocker vs Placebo_residualTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.0476362715837521, "<unicode><unicode><unicode><unicode>", 0.21825735172899,
                                      "<unicode><unicode>", 23.6455819833473, "I<unicode><unicode> (%)",
                                      1.30968190967274, "H<unicode><unicode>"))
})

test_that("Sample Size plot matches", {
  plotName <- results[["results"]][["modelContainerBeta-blocker vs Placebo"]][["collection"]][["modelContainerBeta-blocker vs Placebo_sampleSizePlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "sample-size-2")
})

test_that("Coefficients table results match", {
  table <- results[["results"]][["modelContainerBeta-blocker vs placebo: Single dose"]][["collection"]][["modelContainerBeta-blocker vs placebo: Single dose_coeffTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.595834738352915, "intercept", 3.54882738072525e-12, 0.0856813791640737,
                                      6.95407501800286))
})

test_that("Effect Size plot matches", {
  plotName <- results[["results"]][["modelContainerBeta-blocker vs placebo: Single dose"]][["collection"]][["modelContainerBeta-blocker vs placebo: Single dose_effectSizePlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "effect-size-3")
})

test_that("Fixed and Random Effects table results match", {
  table <- results[["results"]][["modelContainerBeta-blocker vs placebo: Single dose"]][["collection"]][["modelContainerBeta-blocker vs placebo: Single dose_fixRandTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, "Omnibus test of Model Coefficients", 3.54882738072523e-12,
                                      48.3591593560114, 31, "Test of Residual Heterogeneity", 0.294837034198571,
                                      34.7247687518672))
})

test_that("Residual Heterogeneity Estimates table results match", {
  table <- results[["results"]][["modelContainerBeta-blocker vs placebo: Single dose"]][["collection"]][["modelContainerBeta-blocker vs placebo: Single dose_residualTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.0575667982363892, "<unicode><unicode><unicode><unicode>", 0.239930819688487,
                                      "<unicode><unicode>", 25.2565970193632, "I<unicode><unicode> (%)",
                                      1.33791071870124, "H<unicode><unicode>"))
})

test_that("Sample Size plot matches", {
  plotName <- results[["results"]][["modelContainerBeta-blocker vs placebo: Single dose"]][["collection"]][["modelContainerBeta-blocker vs placebo: Single dose_sampleSizePlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "sample-size-3")
})

test_that("Meta-Analyses table results match", {
  table <- results[["results"]][["selectedOverviewTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(21, "Beta-blocker vs Placebo", 2005, 5, "Beta-blocker + agonist vs Placebo + agonist",
                                      2005, 32, "Beta-blocker vs placebo: Single dose", 2002))
})

# Topics & pooled ----
options <- analysisOptions("CochraneContinuousClassicalMetaAnalysis")
options$savePath <- ""
options$addStudy <- FALSE
options$additionalStudies <- list()
options$analyzeAs <- "OR"
options$analyzeData <- "pooled"
options$binWidthType <- "sturges"
options$distPlotDensity <- TRUE
options$distPlotRug <- TRUE
options$forestPlot <- TRUE
options$forestPlotOrder <- "yearDescending"
options$funnelPlot <- TRUE
options$funnelPlotAsymmetryTest <- TRUE
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
options$regressionCoefficientsConfidenceIntervals <- TRUE
options$regressionCoefficientsConfidenceIntervalsInterval <- 0.95
options$regressionCoefficientsCovarianceMatrix <- TRUE
options$regressionCoefficientsEstimates <- TRUE
options$residualsCasewiseDiagnostics <- TRUE
options$residualsParameters <- TRUE
options$reviews <- list(list(metaAnalyses = list(list(check = FALSE, value = "All caffeine doses versus placebo")),
                             value = "Caffeine for asthma"), list(metaAnalyses = list(
                               list(check = FALSE, value = "Diamox versus control")),
                               value = "Carbonic anhydrase inhibitors for hypercapnic ventilatory failure in chronic obstructive pulmonary disease"),
                        list(metaAnalyses = list(list(check = TRUE, value = "Beta-blocker vs Placebo"),
                                                 list(check = TRUE, value = "Beta-blocker + agonist vs Placebo + agonist")),
                             value = "Cardioselective beta-blockers for chronic obstructive pulmonary disease"),
                        list(metaAnalyses = list(list(check = TRUE, value = "Beta-blocker vs placebo: Single dose"),
                                                 list(check = FALSE, value = "Beta-blocker + agonist vs placebo + agonist: Single dose"),
                                                 list(check = FALSE, value = "Beta-blocker vs placebo: Longer duration"),
                                                 list(check = FALSE, value = "Beta-blocker + agonist vs placebo + agonist: Longer duration")),
                             value = "Cardioselective beta-blockers for reversible airway disease")
)
options$selectionType <- "selectionTopics"
options$showLabels <- FALSE
options$test <- "z"
options$textSearch <- ""
options$topicsSelected <- "Airways"
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
                                 list(74.2163066877263, 0.00247499588326312, 1.02839269032903, -0.0494991982288924,
                                      0.0163328861112609, "Chang 2010 (Treatment vs Control)", -0.311332059645642,
                                      0.070304428939096, 1.63328861112609, 70.9643074385565, 0.0450393937756795,
                                      0.997821077089257, 0.214102606843439, 0.0170503797163148, "Hawkins 2009 (Treatment vs Control).1",
                                      1.50456693408017, 0.0635551410149018, 1.70503797163149, 70.9643074385565,
                                      0.0450393937756795, 0.997821077089257, 0.214102606843439, 0.0170503797163148,
                                      "Hawkins 2009 (Treatment vs Control).2", 1.50456693408017, 0.0635551410149018,
                                      1.70503797163149, 74.2163482394012, 0.00303877433277168, 1.03437977836642,
                                      -0.0547222971680464, 0.0197975118287366, "Hawkins 2009 (Treatment + Agonist vs Control + Agonist)",
                                      -0.306810016978024, 0.0708213825854669, 1.97975118287366, 72.5437223610549,
                                      0.0270838992569777, 1.01977138981166, -0.164664603700886, 0.020397185332455,
                                      "van der Woude 2005 (Treatment vs Control)", -1.14691652116087,
                                      0.0675319945492349, 2.03971853324551, 71.3883576177881, 0.0400762066750412,
                                      1.00880165259861, -0.201478748163556, 0.0203264510202067, "van der Woude 2005 (Treatment + Agonist vs Control + Agonist)",
                                      -1.46557215223747, 0.0651847608629551, 2.03264510202067, 73.5353840570588,
                                      0.0134179744794565, 1.02682106594167, -0.115401262079148, 0.0188407612133889,
                                      "Fogari 1990 (Treatment vs Control).1", -0.78908366261643, 0.0694027355371948,
                                      1.88407612133889, 73.5353840570588, 0.0134179744794565, 1.02682106594167,
                                      -0.115401262079148, 0.0188407612133889, "Fogari 1990 (Treatment vs Control).2",
                                      -0.78908366261643, 0.0694027355371948, 1.88407612133889, 73.5353840570588,
                                      0.0134179744794565, 1.02682106594167, -0.115401262079148, 0.0188407612133889,
                                      "Fogari 1990 (Treatment vs Control).3", -0.78908366261643, 0.0694027355371948,
                                      1.88407612133889, 73.8253698897134, 0.0093286732883742, 1.0293613199121,
                                      -0.0961020489375339, 0.0188208999849662, "Fogari 1990 (Treatment + Agonist vs Control + Agonist)",
                                      -0.635704653149761, 0.0699559296348043, 1.88208999849662, 73.1096091528431,
                                      0.0126938237608577, 1.01704713969344, 0.11264990781793, 0.0164240262340646,
                                      "Tantucci 1990 (Beta-blocker vs Control)", 0.8738963949098,
                                      0.0678338177292276, 1.64240262340646, 72.7332002778952, 0.0192913038548494,
                                      1.01855400305091, 0.139036605480221, 0.0200953427414206, "Chodosh 1988 (Beta-blocker vs Control)",
                                      0.959714733805185, 0.0673373136024176, 2.00953427414206, 72.7553430666095,
                                      0.0167318255008644, 1.01217547275499, 0.129519272050256, 0.0151838516813632,
                                      "Lammers 1988 (Beta-blocker vs Control)", 1.02251483403449,
                                      0.0670590331925187, 1.51838516813632, 68.9700774938487, 0.0772022532032681,
                                      0.996973029480923, -0.286975794170603, 0.0323793809254422, "Dorow 1986a (Treatment vs Control).1",
                                      -1.69653349748108, 0.0598282464125606, 3.23793809254423, 68.9700774938487,
                                      0.0772022532032681, 0.996973029480923, -0.286975794170603, 0.0323793809254422,
                                      "Dorow 1986a (Treatment vs Control).2", -1.69653349748108, 0.0598282464125606,
                                      3.23793809254423, 74.167004341907, 0.000594764132084809, 1.02839754277458,
                                      0.0242677785321267, 0.0170467350965568, "Chatterjee 1986 (Beta-blocker vs Control)",
                                      0.250501781820277, 0.0701455082328201, 1.70467350965568, 72.1458353906624,
                                      0.0279923928116273, 1.01104665364979, 0.168003306541521, 0.0189614920486935,
                                      "Doshan 1986a (Beta-blocker vs Control).1", 1.15954936558891,
                                      0.0659740296246299, 1.89614920486935, 68.5637171275575, 0.110414626182263,
                                      0.986484071083235, 0.345110460036079, 0.0306352442069393, "Doshan 1986b (Beta-blocker vs Control).1",
                                      1.66226924557885, 0.0579881214006411, 3.06352442069393, 74.2971028495282,
                                      0.00114640593596647, 1.04007388985568, -0.0335226368009147,
                                      0.0227001008842729, "Falliers 1986 (Beta-blocker vs Control)",
                                      -0.133925293878391, 0.0714006445865528, 2.27001008842729, 73.0165598422853,
                                      0.0134407548746357, 1.01465920888307, 0.115975656099412, 0.0153078395591464,
                                      "Lammers 1986a (Beta-blocker vs Control).1", 0.924564620274221,
                                      0.067567252038416, 1.53078395591464, 69.3478243827071, 0.0596716737543347,
                                      0.989660803159518, -0.248353158498685, 0.0201214399984496, "Doshan 1986a (Beta-blocker vs Control).2",
                                      -1.912449825978, 0.06111984194085, 2.01214399984496, 66.3187018682702,
                                      0.104058697750564, 0.964492316805353, -0.343356039938673, 0.0322805735166649,
                                      "Doshan 1986b (Beta-blocker vs Control).2", -2.11935273733805,
                                      0.0528395535254366, 3.22805735166649, 73.9952169970628, 0.0021518488876978,
                                      1.0245846335064, 0.0462249141187154, 0.0158534482868648, "Lammers 1986a (Beta-blocker vs Control).2",
                                      0.416807397186194, 0.0695886853267677, 1.58534482868648, 74.167004341907,
                                      0.000594764132084809, 1.02839754277458, 0.0242677785321267,
                                      0.0170467350965568, "Chatterjee 1986 (Treatment vs Control).1",
                                      0.250501781820277, 0.0701455082328201, 1.70467350965568, 74.167004341907,
                                      0.000594764132084809, 1.02839754277458, 0.0242677785321267,
                                      0.0170467350965568, "Chatterjee 1986 (Treatment vs Control).2",
                                      0.250501781820277, 0.0701455082328201, 1.70467350965568, 74.2006209736621,
                                      0.000393667245476289, 1.02084847618512, 0.0197878092922376,
                                      0.0125274867067075, "Greefhorst 1984 (Beta-blocker vs Control).1",
                                      0.232879773445888, 0.0695237581422367, 1.25274867067075, 74.2326994587801,
                                      0.000198293307310857, 1.02122384469317, 0.0140422077733721,
                                      0.0125641567581217, "Lammers 1984 (Beta-blocker vs Control)",
                                      0.184065452120162, 0.0695968166351225, 1.25641567581217, 74.2250162712369,
                                      0.000242162031224046, 1.02113166637642, 0.0155184057055276,
                                      0.0125548945934239, "Greefhorst 1984 (Beta-blocker vs Control).2",
                                      0.196619728807753, 0.069578933193242, 1.2554894593424, 73.9060912165217,
                                      0.00245061136570536, 1.01365543136027, 0.0494542529280149, 0.00963201204897796,
                                      "Fenster 1983 (Treatment vs Control).1", 0.532556537090978,
                                      0.0686101468331597, 0.963201204897796, 73.9060912165217, 0.00245061136570536,
                                      1.01365543136027, 0.0494542529280149, 0.00963201204897796, "Fenster 1983 (Treatment vs Control).2",
                                      0.532556537090978, 0.0686101468331597, 0.963201204897796, 73.9060912165217,
                                      0.00245061136570536, 1.01365543136027, 0.0494542529280149, 0.00963201204897796,
                                      "Fenster 1983 (Treatment vs Control).3", 0.532556537090978,
                                      0.0686101468331597, 0.963201204897796, 74.0986119865371, 0.00503097122223785,
                                      1.03164798921196, -0.0704949851906758, 0.018774067126689, "Adam 1982 (Treatment vs Control).1",
                                      -0.438277367344187, 0.0704606208127077, 1.8774067126689, 72.1864926133508,
                                      0.02623720779802, 1.0095507834894, 0.162582617171148, 0.0173690690804112,
                                      "Sorbini 1982 (Treatment vs Control)", 1.17355399082842, 0.0660074615058932,
                                      1.73690690804112, 74.0832369833192, 0.00126580351055356, 1.03243111655953,
                                      0.0353541599845424, 0.0198704235012059, "Ranchod 1982 (Treatment vs Control)",
                                      0.315953197139334, 0.0703836974527717, 1.98704235012059, 74.0986119865371,
                                      0.00503097122223797, 1.03164798921196, -0.0704949851906767,
                                      0.018774067126689, "Adam 1982 (Treatment vs Control).2", -0.438277367344187,
                                      0.0704606208127077, 1.8774067126689, 74.0986119865371, 0.00503097122223797,
                                      1.03164798921196, -0.0704949851906767, 0.018774067126689, "Adam 1982 (Treatment vs Control).3",
                                      -0.438277367344187, 0.0704606208127077, 1.8774067126689, 74.1388877295474,
                                      0.00432773891564713, 1.03196437799651, -0.0653724604415775,
                                      0.018762069946043, "Adam 1982 (Treatment + Agonist vs Control + Agonist)",
                                      -0.399534296256909, 0.0705317063398148, 1.8762069946043, 74.1275656461537,
                                      0.00370501080962051, 1.02595138298161, -0.0606148590248512,
                                      0.0152632334979323, "Adam 1982 (Beta-blocker vs Control)", -0.419500692594092,
                                      0.0700161619383111, 1.52632334979323, 70.6056590496759, 0.0516173908039586,
                                      0.994757168686258, 0.22961896391248, 0.0174300435071264, "Lawrence 1982 (Beta-blocker vs Control)",
                                      1.58167907466203, 0.0628129174262109, 1.74300435071264, 74.1275656461537,
                                      0.00370501080962072, 1.02595138298161, -0.0606148590248529,
                                      0.0152632334979323, "Adam 1982 (Treatment vs Control).4", -0.419500692594092,
                                      0.0700161619383111, 1.52632334979323, 74.1275656461537, 0.00370501080962072,
                                      1.02595138298161, -0.0606148590248529, 0.0152632334979323, "Adam 1982 (Treatment vs Control).5",
                                      -0.419500692594092, 0.0700161619383111, 1.52632334979323, 70.6056590496759,
                                      0.051617390803957, 0.994757168686259, 0.229618963912476, 0.0174300435071264,
                                      "Lawrence 1982 (Treatment vs Control)", 1.58167907466203, 0.0628129174262109,
                                      1.74300435071264, 73.1411208059497, 0.0128649081758192, 1.02027851868016,
                                      0.113341520259152, 0.0184373584638573, "Ellis 1981 (Beta-blocker vs Control)",
                                      0.835745249044581, 0.0680809806869157, 1.84373584638573, 73.3784131007685,
                                      0.00783130021140818, 1.0131774176779, 0.0884647761564427, 0.0119612712460824,
                                      "Lofdahl 1981 (Beta-blocker vs Control)", 0.811573518846702,
                                      0.0679901989911409, 1.19612712460824, 72.9952889122486, 0.011687319463979,
                                      1.01000300944109, 0.108171259519429, 0.0117543599336305, "van den Bergh 1981 (Beta-blocker vs Control)",
                                      0.978463083142031, 0.0673497128625727, 1.17543599336305, 73.3784131007685,
                                      0.00783130021140895, 1.0131774176779, 0.0884647761564471, 0.0119612712460824,
                                      "Lofdahl 1981 (Treatment vs Control)", 0.811573518846703, 0.0679901989911409,
                                      1.19612712460824, 74.2738671145519, 0.00121059725340881, 1.02672944529559,
                                      -0.0346374002142886, 0.0151805907209975, "Sinclair 1979 (Treatment vs Control).1",
                                      -0.20636671605731, 0.0702029041078104, 1.51805907209975, 74.2738671145519,
                                      0.00121059725340881, 1.02672944529559, -0.0346374002142886,
                                      0.0151805907209975, "Sinclair 1979 (Treatment vs Control).2",
                                      -0.20636671605731, 0.0702029041078104, 1.51805907209975, 72.8251710952439,
                                      0.0192508286584448, 1.01612429655494, -0.138714524585575, 0.0152981343356898,
                                      "Sinclair 1979 (Treatment + Agonist vs Control + Agonist)",
                                      -1.10897403494801, 0.0678855599875052, 1.52981343356898, 74.2394612366012,
                                      0.00219216879634069, 1.03035272736998, -0.0465517433350437,
                                      0.017371718560954, "Ruffin 1979 (Beta-blocker vs Control)",
                                      -0.27463793799472, 0.0704952751119884, 1.73717185609541, 72.4857774670945,
                                      0.021344991194059, 1.012268212777, -0.146208129670052, 0.014077756343403,
                                      "McGavin 1978 (Treatment vs Control).1", -1.23780039460094,
                                      0.0673245421470167, 1.4077756343403, 72.4857774670945, 0.021344991194059,
                                      1.012268212777, -0.146208129670052, 0.014077756343403, "McGavin 1978 (Treatment vs Control).2",
                                      -1.23780039460094, 0.0673245421470167, 1.4077756343403, 73.8412696330363,
                                      0.00380089747703866, 1.02467452215269, 0.0614459751946433, 0.0168121536814861,
                                      "Benson 1978 (Beta-blocker vs Control).1", 0.515645669560809,
                                      0.0693936187801322, 1.68121536814862, 73.5620303646976, 0.00703311273929418,
                                      1.02169032369443, 0.0836895766649781, 0.0166525788059758, "Benson 1978 (Beta-blocker vs Control).2",
                                      0.672012899967702, 0.0687848646923103, 1.66525788059758, 74.2238247160437,
                                      0.000255590899056538, 1.01886772429619, 0.0159518278728841,
                                      0.0112642853115355, "Johnsson 1975 (Beta-blocker vs Control)",
                                      0.204850964860935, 0.0693765977228084, 1.12642853115355, 74.100759930066,
                                      0.00116037142937484, 1.02385956630612, 0.0339477110730314, 0.0148238776761944,
                                      "Skinner 1975b (Beta-blocker vs Control).1", 0.333311760537654,
                                      0.0696621872433641, 1.48238776761945, 73.7893669135974, 0.00418687469847367,
                                      1.02058027674926, 0.0645633466137692, 0.0146055820283544, "Skinner 1975b (Beta-blocker vs Control).2",
                                      0.567433736399188, 0.0690021656392609, 1.46055820283544, 74.2238247160437,
                                      0.000255590899056538, 1.01886772429619, 0.0159518278728841,
                                      0.0112642853115355, "Johnsson 1975 (Treatment vs Control)",
                                      0.204850964860935, 0.0693765977228084, 1.12642853115355))
})

test_that("Coefficients table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_coeffTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.427823301140219, 0.302263994613462, "intercept", 2.41788288484453e-11,
                                      0.0640620475524543, 0.553382607666975, 6.67826454953559))
})

test_that("Parameter Covariances table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_covMatTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.00410394593661292, "intercept"))
})

test_that("Effect Size plot matches", {
  plotName <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_effectSizePlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "effect-size-4")
})

test_that("Fixed and Random Effects table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_fixRandTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, "Omnibus test of Model Coefficients", 2.41788288484454e-11,
                                      44.5992173935837, 57, "Test of Residual Heterogeneity", 0.061569711358508,
                                      74.3035177863279))
})

test_that("Forest Plot matches", {
  plotName <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_plots"]][["collection"]][["modelContainer_plots_forest"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "forest-plot-1")
})

test_that("Funnel Plot matches", {
  plotName <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_plots"]][["collection"]][["modelContainer_plots_funnel"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "funnel-plot")
})

test_that("Trim-Fill Analysis plot matches", {
  plotName <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_plots"]][["collection"]][["modelContainer_plots_trimFill"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "trim-fill-analysis")
})

test_that("Regression test for Funnel plot asymmetry (Egger's test) table results match", {
	table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_regTestTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list("sei", 0.00414279190054522, 2.86707930191739))
})

test_that("Residual Heterogeneity Estimates table results match", {
	table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_residualTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(0.067758559438411, 0, "<unicode><unicode><unicode><unicode>",
			 0.117763759893266, 0.260304743403594, 0, "<unicode><unicode>",
			 0.343167247698941, 29.5613137716801, 0, "I<unicode><unicode> (%)",
			 42.176153031321, 1.41967440556543, 1, "H<unicode><unicode>",
			 1.72939029902604))
})

test_that("Sample Size plot matches", {
	plotName <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_sampleSizePlot"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	jaspTools::expect_equal_plots(testPlot, "sample-size-4")
})

test_that("Meta-Analyses table results match", {
	table <- results[["results"]][["selectedOverviewTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(21, "Beta-blocker vs Placebo", 2005, 5, "Beta-blocker + agonist vs Placebo + agonist",
			 2005, 32, "Beta-blocker vs placebo: Single dose", 2002))
})

# Keywords, pooled & full output ----
options <- analysisOptions("CochraneContinuousClassicalMetaAnalysis")
options$savePath <- ""
options$addStudy <- FALSE
options$additionalStudies <- list()
options$analyzeAs <- "OR"
options$analyzeData <- "pooled"
options$binWidthType <- "sturges"
options$distPlotDensity <- FALSE
options$distPlotRug <- FALSE
options$forestPlot <- FALSE
options$forestPlotOrder <- "yearDescending"
options$funnelPlot <- FALSE
options$funnelPlotAsymmetryTest <- FALSE
options$keywordsSearch <- ""
options$keywordsSelected <- "Abatacept"
options$method <- "DerSimonian-Laird"
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
options$reviews <- list(list(metaAnalyses = list(list(check = TRUE, value = "Abatacept (2 mg/kg and 10 mg/kg) + DMARDs/biologic versus placebo + DMARDs/biologic"),
                                                 list(check = TRUE, value = "Abatacept (2 mg/kg) + etanercept versus placebo + etanercept")),
                             value = "Abatacept for rheumatoid arthritis"), list(metaAnalyses = list(
                               list(check = FALSE, value = "Any dosage belatacept versus calcineurin inhibitor (CNI)"),
                               list(check = FALSE, value = "High versus low dosage belatacept")),
                               value = "Belatacept for kidney transplant recipients"), list(
                                 metaAnalyses = list(list(check = FALSE, value = "All biologics"),
                                                     list(check = FALSE, value = "Subgroup comparison: anti-TNF vs non-anti-TNF")),
                                 value = "Biologic interventions for fatigue in rheumatoid arthritis"))
options$selectionType <- "selectionKeywords"
options$showLabels <- TRUE
options$test <- "z"
options$textSearch <- ""
options$topicsSelected <- list()
options$trimFillPlot <- FALSE
set.seed(1)
dataset <- NULL
results <- runAnalysis("CochraneContinuousClassicalMetaAnalysis", dataset, options)


test_that("Coefficients table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_coeffTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.474385610114257, "intercept", 9.30391580528086e-10, 0.0775023931759078,
                                      6.12091563466356))
})

test_that("Fixed and Random Effects table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_fixRandTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, "Omnibus test of Model Coefficients", 9.3039158052809e-10,
                                      37.4656082066689, 31, "Test of Residual Heterogeneity", 3.1659108547955e-105,
                                      593.350523196551))
})

test_that("Residual Heterogeneity Estimates table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_residualTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.177785938737719, "<unicode><unicode><unicode><unicode>", 0.421646698952712,
                                      "<unicode><unicode>", 94.7754322633788, "I<unicode><unicode> (%)",
                                      19.1403394579533, "H<unicode><unicode>"))
})

test_that("Meta-Analyses table results match", {
  table <- results[["results"]][["selectedOverviewTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(30, "Abatacept (2 mg/kg and 10 mg/kg) + DMARDs/biologic versus placebo + DMARDs/biologic",
                                      2009, 2, "Abatacept (2 mg/kg) + etanercept versus placebo + etanercept",
                                      2009))
})
# Search & adding estimates ----
options <- analysisOptions("CochraneContinuousClassicalMetaAnalysis")
options$savePath <- ""
options$addStudy <- TRUE
options$additionalStudies <- list(list(effectSE = "0.10", effectSize = "0.15", lCI = "", n1 = "",
                                       n2 = "", name = "#", titleStudy = "New Study XXX", uCI = "",
                                       x1 = "", x2 = ""))
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
options$reviews <- list(list(metaAnalyses = list(list(check = TRUE, value = "Ethanol versus placebo/control"),
                                                 list(check = TRUE, value = "Ethanol versus other tocolytic drug")),
                             value = "Ethanol for preventing preterm birth in threatened preterm labor"))
options$selectionType <- "selectionTextSearch"
options$showLabels <- TRUE
options$test <- "z"
options$textSearch <- "ethanol"
options$topicsSelected <- list()
options$trimFillPlot <- FALSE
set.seed(1)
dataset <- NULL
results <- runAnalysis("CochraneContinuousClassicalMetaAnalysis", dataset, options)


test_that("Coefficients table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_coeffTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.639976445378039, "intercept", 0.371239290354961, 0.715733715316363,
                                      0.894154392454687))
})

test_that("Fixed and Random Effects table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_fixRandTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, "Omnibus test of Model Coefficients", 0.371239290354961, 0.799512077546011,
                                      6, "Test of Residual Heterogeneity", 2.78578296305851e-52, 255.464557611796
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
                                 list(3.52408630674703, "<unicode><unicode><unicode><unicode>", 1.87725499246827,
                                      "<unicode><unicode>", 98.9541105466252, "I<unicode><unicode> (%)",
                                      95.6123992620151, "H<unicode><unicode>"))
})

test_that("Meta-Analyses table results match", {
  table <- results[["results"]][["selectedOverviewTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, "Ethanol versus placebo/control", 2015, 5, "Ethanol versus other tocolytic drug",
                                      2015))
})
