#
# Copyright (C) 2013-2025 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

# This is a generated file. Don't change it!

#' Classical Continuous Outcomes
#'
CochraneContinuousClassicalMetaAnalysis <- function(
          data = NULL,
          version = "1",
          addStudy = FALSE,
          additionalStudies = list(),
          analyzeAs = "logOr",
          analyzeData = "individually",
          binWidthType = "sturges",
          casewiseDiagnostics = FALSE,
          changeTargetGroup = FALSE,
          coefficientCi = FALSE,
          coefficientCiLevel = 0.95,
          coefficientEstimate = TRUE,
          covarianceMatrix = FALSE,
          diagnosticPlot = FALSE,
          diagnosticQqPlot = TRUE,
          distPlotDensity = FALSE,
          distPlotRug = FALSE,
          estimateTest = "z",
          failSafeN = FALSE,
          fitMeasure = FALSE,
          forestPlot = FALSE,
          forestPlotLabel = TRUE,
          forestPlotOrder = "yearDescending",
          funnelPlot = FALSE,
          funnelPlotRankTestAsymmetry = FALSE,
          funnelPlotRegressionTestAsymmetry = FALSE,
          keywordsSearch = "",
          keywordsSelected = list(types = list(), value = list()),
          method = "Restricted ML",
          numberOfBins = 30,
          plotEffectSizes = FALSE,
          plotHeight = 320,
          plotSampleSizes = FALSE,
          plotWidth = 480,
          profilePlot = FALSE,
          residualParameter = TRUE,
          reviews = list(),
          savePath = "",
          selectionType = "selectionTopics",
          targetGroup = list(),
          textSearch = "",
          topicsSelected = list(types = list(), value = list()),
          trimFillAnalysis = FALSE) {

   options <- mget(names(formals(sys.function())), envir = environment(), inherits = FALSE)
   options[["data"]] <- NULL
   options[["version"]] <- NULL


   if (!jaspBase::jaspResultsCalledFromJasp() && !is.null(data)) {
      jaspBase::storeDataSet(data)
   }

   optionsWithFormula <- c("additionalStudies", "analyzeAs", "binWidthType", "estimateTest", "forestPlotOrder", "keywordsSelected", "method", "reviews", "targetGroup", "textSearch", "topicsSelected")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   # Options not given in the call get their default from the form, which can depend on the other options.
   options <- options[names(options) %in% names(match.call())[-1L]]
   return(jaspBase::runWrappedAnalysis("jaspCochrane", "CochraneContinuousClassicalMetaAnalysis", "CochraneContinuousClassicalMetaAnalysis.qml", options, version, FALSE))
}