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

#' Bayesian Dichotomous Outcomes
#'
CochraneDichotomousBayesianMetaAnalysis <- function(
          data = NULL,
          version = "1",
          addStudy = FALSE,
          additionalStudies = list(),
          analyzeAs = "logOr",
          analyzeData = "individually",
          bayesFactorComputation = "integration",
          bayesFactorType = "BF10",
          bfSequentialPlot = FALSE,
          binWidthType = "sturges",
          bridgeSamplingSamples = 5000,
          cauchyLocation = 0,
          cauchyScale = 0.707,
          chains = 4,
          changeTargetGroup = FALSE,
          constrainedRandomDirection = "positive",
          cumulativeForestPlot = FALSE,
          cumulativeForestPlotPrior = FALSE,
          distPlotDensity = FALSE,
          distPlotRug = FALSE,
          effectSizePerStudy = FALSE,
          forestPlot = TRUE,
          forestPlotEffect = "observed",
          forestPlotLabel = TRUE,
          forestPlotOrder = "yearDescending",
          forestPlotRowOrder = "ascending",
          halfTDf = 1,
          halfTScale = 0.707,
          inverseGammaScale = 0.15,
          inverseGammaShape = 1,
          keywordsSearch = "",
          keywordsSelected = list(types = list(), value = list()),
          method = "Restricted ML",
          model = "averaging",
          modelProbability = FALSE,
          modelProbabilitySequentialPlot = FALSE,
          normalMean = 0,
          normalSd = 0.707,
          numberOfBins = 30,
          plotEffectSizes = FALSE,
          plotHeight = 320,
          plotSampleSizes = FALSE,
          plotWidth = 480,
          priorEffectSize = "cauchy",
          priorModelProbabilityFixedAlternative = 0.25,
          priorModelProbabilityFixedNull = 0.25,
          priorModelProbabilityRandomAlternative = 0.25,
          priorModelProbabilityRandomNull = 0.25,
          priorPlot = FALSE,
          priorPosterior = FALSE,
          priorPosteriorAdditionalInfo = FALSE,
          priorPosteriorCi = FALSE,
          priorPosteriorFixedAndRandom = FALSE,
          priorStandardError = "inverseGamma",
          reviews = list(),
          samples = 2000,
          savePath = "",
          seed = 1,
          selectionType = "selectionTopics",
          setSeed = FALSE,
          tDf = 1,
          tLocation = 0,
          tScale = 0.707,
          targetGroup = list(),
          textSearch = "",
          topicsSelected = list(types = list(), value = list()),
          truncationLowerBound = FALSE,
          truncationLowerBoundValue = 0,
          truncationUpperBound = FALSE,
          truncationUpperBoundValue = 0) {

   options <- mget(names(formals(sys.function())), envir = environment(), inherits = FALSE)
   options[["data"]] <- NULL
   options[["version"]] <- NULL


   if (!jaspBase::jaspResultsCalledFromJasp() && !is.null(data)) {
      jaspBase::storeDataSet(data)
   }

   optionsWithFormula <- c("additionalStudies", "analyzeAs", "binWidthType", "forestPlotOrder", "keywordsSelected", "method", "reviews", "targetGroup", "textSearch", "topicsSelected")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   # Options not given in the call get their default from the form, which can depend on the other options.
   options <- options[names(options) %in% names(match.call())[-1L]]
   return(jaspBase::runWrappedAnalysis("jaspCochrane", "CochraneDichotomousBayesianMetaAnalysis", "CochraneDichotomousBayesianMetaAnalysis.qml", options, version, FALSE))
}