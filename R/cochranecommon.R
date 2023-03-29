#
# Copyright (C) 2019 University of Amsterdam
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

# This module imports the analysis functionality from the Meta-Analysis module.
# Most of functions implemented here provide data search functionality.
#' @import jaspBase
#' @importFrom jaspDescriptives .plotMarginal
#' @importFrom jaspMetaAnalysis .ClassicalMetaAnalysisCommon .BayesianMetaAnalysisCommon
#' @export CochraneContinuousClassicalMetaAnalysis
#' @export CochraneDichotomousClassicalMetaAnalysis
#' @export CochraneContinuousBayesianMetaAnalysis
#' @export CochraneDichotomousBayesianMetaAnalysis

CochraneCommon   <- function(jaspResults, dataset, options, type) {

  options[["module"]] <- "Cochrane"
  options[["type"]]   <- type

  if (type == "bayesianDichotomous")
    .cochranePriorWarning(jaspResults)

  ### work with the database and dependent menus
  # load the database
  if (is.null(jaspResults[["database"]]))
    .cochraneLoadDatabase(jaspResults, type)

  # create the qml options based on the database
  if (is.null(jaspResults[["sourceTopics"]]))
    .cochraneCreateDatabaseTopics(jaspResults)
  if (is.null(jaspResults[["sourceKeywords"]]))
    .cochraneCreateDatabaseKeywords(jaspResults, options)

  # prepare additional qml gadget for filtering the selected data set
  if (is.null(jaspResults[["selectorGadget"]]))
    .cochraneCreateSelectorGadget(jaspResults, options)

  if (is.null(jaspResults[["targetGroupGadget"]]))
    .cochraneCreatetargetGroupGadget(jaspResults, options)


  # create data set based on the database and selection
  if (is.null(jaspResults[["dataset"]]))
    dataset <- .cochraneSelectDataset(jaspResults, options)
  else
    dataset <- jaspResults[["dataset"]][["object"]]

  # sort the data for the classical forest plots (Bayesian analysis does it inside)
  if (type %in% c("classicalContinuous", "classicalDichotomous"))
    dataset   <- .cochraneSortData(dataset, options)

  # add data
  if (!is.null(dataset) && options[["addStudy"]]) {
    if (type %in% c("classicalContinuous", "bayesianContinuous"))
      dataset <- .cochraneAddContinuousData(dataset, options)
    else if (type %in% c("classicalDichotomous", "bayesianDichotomous"))
      dataset <- .cochraneAddDichotomousData(dataset, options)
  }

  # overview table
  if (is.null(jaspResults[["selectedOverviewTable"]]))
    .cochraneSelectedMetaAnalysesOverviewTable(jaspResults, dataset, options)

  #if (is.null(jaspResults[["selectedOutcomesOverview"]]) && options[["outcomeSummaryTable"]])
  #  .cochraneSelectedOutcomesOverviewTable(jaspResults, options)

  ### add additional arguments for the classical/Bayesian meta-analysis
  if (type %in% c("classicalContinuous", "classicalDichotomous"))
    options <- .cochraneEmulateClassicalMetaAnalysisOptions(options)
  else if (type %in% c("bayesianContinuous", "bayesianDichotomous"))
    options <- .cochraneEmulateBayesianMetaAnalysisOptions(options)

  ready   <- .cochraneReady(options, dataset)

  ### export the data
  if (options[["savePath"]] != "" && is.null(jaspResults[["dataSaved"]]))
    .cochraneExportData(jaspResults, dataset, options)

  # do not run on an empty data set
  if (is.null(dataset))
    return()

  ### apply the classical meta-analysis to the data set
  if (options[["analyzeData"]] == "individually") {

    selection  <- unique(dataset[,"titleMetaAnalysis"])
    selection  <- selection[selection != "_add"]

    startProgressbar(length(selection))

    for (title in sort(selection, decreasing = TRUE)) {

      tempDataset   <- dataset[dataset[,"titleMetaAnalysis"] %in% c("_add", title),]
      tempContainer <- .cochraneGetOutputContainer(jaspResults, title)

      if (nrow(tempDataset) < 2) {
        .cochraneSetDataError(tempContainer)
        progressbarTick()
        next
      }

      # overview figures
      if (options[["plotEffectSizes"]])
        .cochraneDecriptivePlot(tempContainer, tempDataset, "effectSize", options, type)

      # overview figures
      if (options[["plotSampleSizes"]])
        .cochraneDecriptivePlot(tempContainer, tempDataset, "sampleSize", options, type)

      if (type %in% c("classicalContinuous", "classicalDichotomous"))
        .ClassicalMetaAnalysisCommon(tempContainer, tempDataset, ready, options)
      else if (type %in% c("bayesianContinuous", "bayesianDichotomous"))
        .BayesianMetaAnalysisCommon(tempContainer, tempDataset, ready, options)

      # add exponentiated estimates for dichotomous outcomes
      if (type == "classicalDichotomous")
        .ClassicalMetaAnalysisAddExponentialSummary(tempContainer, ready, options)
      else if (type == "bayesianDichotomous")
        .BayesianMetaAnalysisAddExponentialSummary(tempContainer, ready, options)


      progressbarTick()
    }

  } else if (options[["analyzeData"]] == "pooled") {

    container <- .cochraneGetOutputContainer(jaspResults)

    if (nrow(dataset) < 2) {
      .cochraneSetDataError(container)
      return()
    }

    # overview figures
    if (options[["plotEffectSizes"]])
      .cochraneDecriptivePlot(container, dataset, "effectSize", options, type)

    # overview figures
    if (options[["plotSampleSizes"]])
      .cochraneDecriptivePlot(container, dataset, "sampleSize", options, type)

    if (type %in% c("classicalContinuous", "classicalDichotomous"))
      .ClassicalMetaAnalysisCommon(container, dataset, ready, options)
    else if (type %in% c("bayesianContinuous", "bayesianDichotomous"))
      .BayesianMetaAnalysisCommon(container, dataset, ready, options)

    # add exponentiated estimates for dichotomous outcomes
    if (type == "classicalDichotomous")
      .ClassicalMetaAnalysisAddExponentialSummary(container, ready, options)
    else if (type == "bayesianDichotomous")
      .BayesianMetaAnalysisAddExponentialSummary(container, ready, options)
  }


  return()
}

.cochraneDataDependencies       <- c("selectionType", "topicsSelected", "keywordsSelected", "textSearch", "analyzeData",
                                     "addStudy", "additionalStudies", "reviews", "analyzeAs", "changeTargetGroup", "targetGroup")
.cochraneLoadDatabase           <- function(jaspResults, type) {

  database         <- createJaspState()
  if (type %in% c("classicalContinuous", "bayesianContinuous"))
    database$object  <- readRDS(system.file("database/continuous.RDS", package = "jaspCochrane"))
  else if (type %in% c("classicalDichotomous", "bayesianDichotomous"))
    database$object  <- readRDS(system.file("database/dichotomous.RDS", package = "jaspCochrane"))

  jaspResults[["database"]] <- database

  return()
}
.cochraneAddContinuousData      <- function(dataset, options) {

  if (length(options[["additionalStudies"]]) == 0)
    return(dataset)

  additionalEstimates <- sapply(options[["additionalStudies"]], function(study)unlist(study), simplify = F)
  additionalEstimates <- data.frame(do.call(rbind, additionalEstimates))

  for (i in 1:ncol(additionalEstimates))
    additionalEstimates[,i] <- as.character(additionalEstimates[,i])

  for (col in c("effectSize", "effectSizeSe", "lCI", "uCI"))
    additionalEstimates[,col] <- as.numeric(additionalEstimates[,col])

  additionalEstimates <- additionalEstimates[!is.na(additionalEstimates[,"effectSize"]),]
  additionalEstimates <- additionalEstimates[!is.na(additionalEstimates[,"effectSizeSe"]) | (!is.na(additionalEstimates[,"lCI"]) & !is.na(additionalEstimates[,"uCI"])),]

  if (nrow(additionalEstimates) == 0)
    return(dataset)

  for (i in 1:nrow(additionalEstimates))
    if (is.na(additionalEstimates[i,"effectSizeSe"]) && all(is.numeric(unlist(additionalEstimates[i, c("lCI", "uCI")])))) {
      if (additionalEstimates[i,"lCI"] > additionalEstimates[i,"effectSize"] || additionalEstimates[i,"uCI"] < additionalEstimates[i,"effectSize"])
        .quitAnalysis(gettext("The effect size does not lie within the confidence interval in one of the specified studies."))
      additionalEstimates[i,"effectSizeSe"] <- (additionalEstimates[i,"uCI"] - additionalEstimates[i,"lCI"]) / (qnorm(.975) * 2)
    }


  if (any(additionalEstimates[,"effectSizeSe"] < 0))
    .quitAnalysis(gettext("One of the specified studies has a negative standard error."))

  additionalEstimates <- additionalEstimates[,c("effectSize",  "effectSizeSe", "studyLabel")]
  additionalEstimates$studyLabel        <- paste0("_add", additionalEstimates$studyLabel)
  additionalEstimates$studyYear         <- NA
  additionalEstimates$match             <- "_add"
  additionalEstimates$metaAnalysisId    <- "_add"
  additionalEstimates$titleReview       <- "_add"
  additionalEstimates$titleMetaAnalysis <- "_add"
  additionalEstimates$titleGroup        <- "_add"
  additionalEstimates$sampleSize        <- NA
  additionalEstimates <- additionalEstimates[,colnames(dataset)]

  dataset <- rbind(dataset, additionalEstimates)

  return(dataset)
}
.cochraneAddDichotomousData     <- function(dataset, options) {

  if (length(options[["additionalStudies"]]) == 0)
    return(dataset)

  additionalEstimates <- sapply(options[["additionalStudies"]], function(study)unlist(study), simplify = F)
  additionalEstimates <- data.frame(do.call(rbind, additionalEstimates))

  for (i in 1:ncol(additionalEstimates))
    additionalEstimates[,i] <- as.character(additionalEstimates[,i])

  for (col in c("effectSize", "effectSizeSe", "x1", "x2", "n1", "n2"))
    additionalEstimates[,col] <- as.numeric(additionalEstimates[,col])

  additionalEstimates <- additionalEstimates[(!is.na(additionalEstimates[,"effectSizeSe"]) & !is.na(additionalEstimates[,"effectSize"])) | (!is.na(additionalEstimates[,"x1"]) & !is.na(additionalEstimates[,"x2"]) & !is.na(additionalEstimates[,"n1"]) & !is.na(additionalEstimates[,"n2"])),]

  if (nrow(additionalEstimates) == 0)
    return(dataset)

  for (i in 1:nrow(additionalEstimates))
    if (is.na(additionalEstimates[i,"effectSizeSe"]) && all(is.numeric(unlist(additionalEstimates[i, c("x1", "x2", "n1", "n2")])))) {
      if (any(unlist(additionalEstimates[i, c("x1", "x2", "n1", "n2")]) < 0))
        .quitAnalysis(gettext("All specified frequencies need to be larger than zero."))
      if (any(unlist(additionalEstimates[i, c("n1", "n2")]) < unlist(additionalEstimates[i, c("x1", "x2")])))
        .quitAnalysis(gettext("The number of events must be lower than the number of observations."))

      tempMeasure <- with(
        additionalEstimates[i,],
        metafor::escalc(
          measure = .cochraneEffectSizeTypeOption(options),
          ai     = x1,
          ci     = x2,
          n1i    = n1,
          n2i    = n2
        ))

      additionalEstimates$effectSize[i]    <- tempMeasure[1,1]
      additionalEstimates$effectSizeSe[i]  <- sqrt(tempMeasure[1,2])
    }

  if (any(additionalEstimates[,"effectSizeSe"] < 0))
    .quitAnalysis(gettext("One of the specified studies has a negative standard error."))

  additionalEstimates                   <- additionalEstimates[,c("effectSize",  "effectSizeSe", "studyLabel")]
  additionalEstimates$studyLabel        <- paste0("_add", additionalEstimates$studyLabel)
  additionalEstimates$studyYear         <- NA
  additionalEstimates$match             <- "_add"
  additionalEstimates$metaAnalysisId    <- "_add"
  additionalEstimates$titleReview       <- "_add"
  additionalEstimates$titleMetaAnalysis <- "_add"
  additionalEstimates$titleGroup        <- "_add"
  additionalEstimates$sampleSize        <- NA

  additionalEstimates <- additionalEstimates[,colnames(dataset)]

  dataset <- rbind(dataset, additionalEstimates)

  return(dataset)
}
.cochraneSortData               <- function(dataset, options) {

  if (is.null(dataset))
    return(dataset)

  if (options[["forestPlotOrder"]] == "yearAscending") {
    dataset <- dataset[order(dataset[,"studyYear"]),]
  } else if (options[["forestPlotOrder"]] == "yearDescending") {
    dataset <- dataset[order(dataset[,"studyYear"], decreasing = TRUE),]
  } else if (options[["forestPlotOrder"]] == "effectSizeAscending") {
    dataset <- dataset[order(dataset[,"effectSize"]),]
  } else if (options[["forestPlotOrder"]] == "effectSizeDescending") {
    dataset <- dataset[order(dataset[,"effectSize"], decreasing = TRUE),]
  }

  return(dataset)
}
.cochraneSelectDataset          <- function(jaspResults, options) {

  dataset <- createJaspState()
  dataset$dependOn(c("reviews", "analyzeAs", "changeTargetGroup", "targetGroup"))
  jaspResults[["dataset"]] <- dataset

  # obtain the selected meta-analyses
  selectedOutcomes <- .cochraneExtractReviewsOptions(options)

  if (length(selectedOutcomes) == 0)
    return()

  # select the data set
  studies <- jaspResults[["database"]]$object[["studies"]]

  # multiple meta-analyses have the same title / outcome - match based on the combination of review/meta-analysis/title
  studies$match          <- with(studies,          paste0(titleReview, "---", titleMetaAnalysis,  "---", titleGroup))
  selectedOutcomes$match <- with(selectedOutcomes, paste0(titleReview, "---", titleMetaAnalysis,  "---", titleGroup))

  studies <- studies[studies$match %in% selectedOutcomes$match,]

  # compute effect sizes
  studies <- .cochraneProcessDataset(studies, options)

  dataset[["object"]] <- studies

  return(studies)
}
.cochraneProcessDataset         <- function(dataset, options) {

  # change the default groups if appropriate
  if (options[["changeTargetGroup"]]) {

    # find meta-analyses that need to be changed
    targetGroups <- do.call(rbind, lapply(options[["targetGroup"]], function(review){
      metaAnalysisGroup <- do.call(rbind, lapply(review$metaAnalysesGroups, function(metaAnalysis){
        return(data.frame(
          "titleMetaAnalysis"     = metaAnalysis[["value"]],
          "selectedGroup"         = metaAnalysis[["checkMetaGroup"]],
          "selectedEqualDefault"  = metaAnalysis[["checkMetaGroup"]] == metaAnalysis[["metaAnalysesGroupChoice"]][[1]]
        ))
      }))
      return(cbind(
        "titleReview" = review[["value"]],
        metaAnalysisGroup
      ))
    }))
    toChange      <- targetGroups[!targetGroups$selectedEqualDefault,]

    # change the corresponding groups
    if (nrow(toChange) > 0) {

      toChange$changeMatch <- with(toChange, paste0(titleReview, "---", titleMetaAnalysis))
      dataset$changeMatch  <- with(dataset, paste0(titleReview, "---", titleMetaAnalysis))

      if (options[["type"]] %in% c("classicalContinuous", "bayesianContinuous")) {
        dataset[dataset$changeMatch %in% toChange$changeMatch, c(
          "group1Mean",
          "group2Mean",
          "group1Sd",
          "group2Sd",
          "group1SampleSize",
          "group2SampleSize",
          "group1Label",
          "group2Label"
        )] <- dataset[dataset$changeMatch %in% toChange$changeMatch, c(
          "group2Mean",
          "group1Mean",
          "group2Sd",
          "group1Sd",
          "group2SampleSize",
          "group1SampleSize",
          "group2Label",
          "group1Label"
        )]
      } else if (options[["type"]] %in% c("classicalDichotomous", "bayesianDichotomous")) {
        dataset[dataset$changeMatch %in% toChange$changeMatch, c(
          "group1Events",
          "group2Events",
          "group1SampleSize",
          "group2SampleSize",
          "group1Label",
          "group2Label"
        )] <- dataset[dataset$changeMatch %in% toChange$changeMatch, c(
          "group2Events",
          "group1Events",
          "group2SampleSize",
          "group1SampleSize",
          "group2Label",
          "group1Label"
        )]
      }
      dataset$changeMatch <- NULL
    }
  }

  # compute appropriate effect size
  if (options[["type"]] %in% c("classicalContinuous", "bayesianContinuous")) {
    effectSizes <- data.frame(with(
      dataset,
      metafor::escalc(
        measure = "SMD",
        m1i     = group1Mean,
        m2i     = group2Mean,
        sd1i    = group1Sd,
        sd2i    = group2Sd,
        n1i     = group1SampleSize,
        n2i     = group2SampleSize
      )))
  } else if (options[["type"]] %in% c("classicalDichotomous", "bayesianDichotomous")) {
    effectSizes <- with(
      dataset,
      metafor::escalc(
        measure = .cochraneEffectSizeTypeOption(options),
        ai      = group1Events,
        ci      = group2Events,
        n1i     = group1SampleSize,
        n2i     = group2SampleSize
      ))
  }

  # bind together with additional information
  dataset$effectSize   <- effectSizes[,"yi"]
  dataset$effectSizeSe <- sqrt(effectSizes[,"vi"])
  dataset$sampleSize   <- dataset$group1SampleSize + dataset$group2SampleSize
  dataset$studyLabel   <- sprintf("%1$s (%2$s vs. %3$s)", dataset$titleStudy, dataset$group1Label, dataset$group2Label)

  # remove no longer needed information
  if (options[["type"]] %in% c("classicalContinuous", "bayesianContinuous")) {
    dataset$group1Mean        <- NULL
    dataset$group2Mean        <- NULL
    dataset$group1Sd          <- NULL
    dataset$group2Sd          <- NULL
    dataset$group1SampleSize  <- NULL
    dataset$group2SampleSize  <- NULL
    dataset$titleStudy        <- NULL
    dataset$group1Label       <- NULL
    dataset$group2Label       <- NULL
  } else if (options[["type"]] %in% c("classicalDichotomous", "bayesianDichotomous")) {
    dataset$group1Events      <- NULL
    dataset$group2Events      <- NULL
    dataset$group1SampleSize  <- NULL
    dataset$group2SampleSize  <- NULL
    dataset$titleStudy        <- NULL
    dataset$group1Label       <- NULL
    dataset$group2Label       <- NULL
  }

  return(dataset)
}
.cochraneSelectedMetaAnalysesOverviewTable  <- function(jaspResults, dataset, options) {

  # create overview from the selected meta-analyses
  selectedOverviewTable <- createJaspTable(title = gettext("Meta-Analyses"))
  selectedOverviewTable$addColumnInfo(name = "titleMetaAnalysis",     title = gettext("Meta-Analysis"),      type = "string")
  selectedOverviewTable$addColumnInfo(name = "year",                  title = gettext("Year"),               type = "integer")
  selectedOverviewTable$addColumnInfo(name = "nStudies",              title = gettext("Number of studies"),  type = "integer")
  selectedOverviewTable$position <- 1
  selectedOverviewTable$dependOn("reviews")
  jaspResults[["selectedOverviewTable"]] <- selectedOverviewTable

  # obtain the selected meta-analyses
  selectedMetaAnalyses <- .cochraneExtractReviewsOptions(options)

  if (length(selectedMetaAnalyses) == 0)
    return()

  # load the reviews meta-data
  reviews      <- jaspResults[["database"]]$object[["reviews"]]

  # summarize results only on the meta-analysis level
  dataset       <- dataset[dataset$match != "_add",]
  dataset$match <- with(dataset, paste0(titleReview, "---", titleMetaAnalysis))

  metaAnalyses <- do.call(rbind, lapply(unique(dataset$match), function(match) {

    tempReviewTitle       <- dataset[dataset$match == match, "titleReview"][1]
    tempMetaAnalysisTitle <- dataset[dataset$match == match, "titleMetaAnalysis"][1]

    return(data.frame(
      titleMetaAnalysis = tempMetaAnalysisTitle,
      year              = reviews[[tempReviewTitle]][["year"]],
      nStudies          = nrow(dataset[dataset$match == match,])
    ))
  }))

  selectedOverviewTable$setData(metaAnalyses)

  return()
}
.cochraneSelectedOutcomesOverviewTable      <- function(jaspResults, options) {

  # create outcomes overview from the selected meta-analyses
  selectedOutcomesOverview <- createJaspContainer("Outcome Summary")
  selectedOutcomesOverview$position <- 2
  selectedOutcomesOverview$dependOn(c("reviews", "outcomeSummaryTable"))
  jaspResults[["selectedOutcomesOverview"]] <- selectedOutcomesOverview

  # obtain the selected meta-analyses
  selectedOutcomes <- .cochraneExtractReviewsOptions(options)

  if (length(selectedOutcomes) == 0)
    return()

  # load the reviews meta-data
  reviews      <- jaspResults[["database"]]$object[["reviews"]]

  # summarize results only on the meta-analysis level
  selectedOutcomes$match <- with(selectedOutcomes, paste0(titleReview, "---", titleMetaAnalysis))
  selectedMetaAnalyses   <- selectedOutcomes[!duplicated(selectedOutcomes$match), ]

  for (match in selectedMetaAnalyses$match) {

    tempReviewTitle       <- selectedMetaAnalyses[selectedMetaAnalyses$match == match, "titleReview"]
    tempMetaAnalysisTitle <- selectedMetaAnalyses[selectedMetaAnalyses$match == match, "titleMetaAnalysis"]
    tempOutcomesTitle     <- selectedOutcomes[selectedOutcomes$match == match, "titleOutcome"]

    tempReview    <- reviews[[tempReviewTitle]]
    tempNOutcomes <- tempReview[["nStudiesOutcomes"]][[tempMetaAnalysisTitle]][tempOutcomesTitle]

    selectedOutcomesOverviewTable <- createJaspTable(title = tempMetaAnalysisTitle)
    selectedOutcomesOverviewTable$addColumnInfo(name = "titleOutcome",  title = gettext("Outcome"),            type = "string")
    selectedOutcomesOverviewTable$addColumnInfo(name = "nStudies",      title = gettext("Number of studies"),  type = "integer")

    selectedOutcomesOverviewTable$setData(data.frame(
      titleOutcome = names(tempNOutcomes),
      nStudies     = unname(tempNOutcomes)
    ))

    selectedOutcomesOverview[[paste0("selectedOutcomesOverviewTable", which(selectedMetaAnalyses$match == match))]] <- selectedOutcomesOverviewTable

  }

  return()
}
.cochraneReady                  <- function(options, dataset) {

  # don't even try running the analysis before the selector gadget was generated and updated
  if (length(options[["reviews"]]) == 0)
    return(FALSE)

  return( !(nrow(dataset) == 0 || is.null(dataset)) )
}
.cochraneDecriptivePlot         <- function(container, dataset, variable, options, type) {

  if (!is.null(container[[paste0(variable,"Plot")]]))
    return()

  descriptivePlot <- createJaspPlot(
    plot         = .plotMarginal(
      column         = dataset[[variable]],
      variableName   = .cochraneGetPlotVariableName(variable, options, type),
      displayDensity = options[["distPlotDensity"]],
      rugs           = options[["distPlotRug"]],
      binWidthType   = options[["binWidthType"]],
      numberOfBins   = options[["numberOfBins"]]),
    width        = 300,
    aspectRatio  = 1,
    title        = .cochraneGetPlotVariableName(variable, options, type),
    position     = if (variable == "effectSize") -2 else -1,
    dependencies = c("distPlotDensity", "distPlotRug", "binWidthType", "numberOfBins", if (variable == "effectSize") "plotEffectSizes" else "plotSampleSizes")
  )

  container[[paste0(variable,"Plot")]] <- descriptivePlot

  return()
}
.cochraneGetPlotVariableName    <- function(variable, options, type) {

  if (type %in% c("classicalContinuous", "bayesianContinuous")) {
    if (variable == "effectSize")
      return(gettext("Effect Size"))
    else if (variable == "sampleSize")
      return(gettext("Sample Size"))
  } else if (type %in% c("classicalDichotomous", "bayesianDichotomous")) {
    if (variable == "effectSize")
      return(.cochraneGetDichotomousEffectSizeNameOption(options))
    else if (variable == "sampleSize")
      return(gettext("Sample Size"))
  }
}
.cochraneGetOutputContainer     <- function(jaspResults, title = "") {

  if (!is.null(jaspResults[[paste0("modelContainer",title)]])) {
    modelContainer <- jaspResults[[paste0("modelContainer",title)]]
  } else {
    modelContainer <- createJaspContainer(title)
    modelContainer$dependOn(c(.cochraneDataDependencies, "addStudy", "additionalStudies", "analyzeData", "reviews",
                              "forestPlotOrder", "studyLabels",
                              "method", "test", "regressionCoefficientsConfidenceIntervalsInterval"))
    jaspResults[[paste0("modelContainer",title)]] <- modelContainer
  }
  return(modelContainer)
}
.cochraneCreateDatabaseTopics   <- function(jaspResults) {

  database <- jaspResults[["database"]]$object

  jaspResults[["sourceTopics"]] <- createJaspQmlSource(
    "sourceTopics",
    sort(unique(sapply(database[["reviews"]], function(review) review[["topic"]])))
  )

  return()
}
.cochraneCreateDatabaseKeywords <- function(jaspResults, options, maxKeywords = Inf) {

  database <- jaspResults[["database"]]$object

  keywords <- sort(unique(unlist(do.call(c, lapply(database[["reviews"]], function(review) review[["keywords"]])))))

  if (options[["keywordsSearch"]] == "")
    keywords <- keywords
  else if (substr(options[["keywordsSearch"]], 1 ,1) == "_")
    keywords <- keywords[grepl(substr(options[["keywordsSearch"]], 2, nchar(options[["keywordsSearch"]])), keywords, ignore.case = TRUE)]
  else
    keywords <- names(unlist(sapply(keywords, function(keyword) {
      keyword <- gsub("\n", ",", gsub(";", ",", gsub(" ", ",", keyword)))
      keyword <- unlist(strsplit(keyword, split = ","))
      keyword <- keyword[keyword != ""]
      return(keyword[tolower(keyword) == tolower(options[["keywordsSearch"]])])
    })))


  keywords <- sort(keywords)
  # limit the maximum number of rendered keywords to prevent laggy qml
  if (length(keywords) > maxKeywords) {
    if (!is.null(jaspResults[["selectedOverviewTable"]]))
      jaspResults[["selectedOverviewTable"]]$addFootnote(
        message = gettextf(
        "%1$i %2$s not shown in the search results (the limit is %3$i). Please, narrow your search terms.",
        length(keywords) - maxKeywords,
        if (length(keywords) - maxKeywords == 1) gettext("keyword was") else gettext("keywords were"),
        maxKeywords),
        symbol = "Note: ")
    keywords <- keywords[1:maxKeywords]
  }

  jaspResults[["sourceKeywords"]] <- createJaspQmlSource(
    "sourceKeywords",
    keywords,
    "keywordsSearch"
  )

  return()
}
.cochraneCreateSelectorGadget   <- function(jaspResults, options) {

  # perform the search on reviews in case of topics/keywords or meta-analyses in the case of titles
  database <- jaspResults[["database"]]$object

  if (options[["selectionType"]] == "selectionTopics") {

    indexing       <- database[["reviews"]]
    selectedTitles <- unname(unlist(sapply(indexing, function(indx) {
      if (indx[["topic"]] %in% options[["topicsSelected"]])
        return(indx[["titleReview"]])
      else
        return(NULL)
    })))


  } else if (options[["selectionType"]] == "selectionKeywords") {

    indexing       <- database[["reviews"]]
    selectedTitles <- unname(unlist(sapply(indexing, function(indx) {
      if (any(indx[["keywords"]] %in% options[["keywordsSelected"]]))
        return(indx[["titleReview"]])
      else
        return(NULL)
    })))

  } else if (options[["selectionType"]] == "selectionTextSearch") {

    indexing   <- database[["reviews"]]
    textSearch <- options[["textSearch"]]
    textSearch <- gsub("\n", ",", gsub(";", ",", gsub(" ", ",", textSearch)))
    textSearch <- unlist(strsplit(textSearch, split = ","))
    textSearch <- textSearch[textSearch != ""]
    textSearch <- tolower(textSearch)

    if (length(textSearch) == 0)
      return()

    searchPositive <- textSearch[substr(textSearch, 1, 1) != "-"]
    searchNegative <- textSearch[substr(textSearch, 1, 1) == "-"]
    searchNegative <- substr(searchNegative, 2, nchar(searchNegative))

    indexingTitles <- do.call(rbind, lapply(indexing, function(review) {
      return(data.frame(
        titleReview       = review$titleReview,
        titleMetaAnalysis = names(review$metaAnalyses)
      ))
    }))

    textSearchPositive <- apply(matrix(sapply(searchPositive, function(text) {
      return(
        grepl(text, tolower(indexingTitles$titleReview), fixed = TRUE) |
          grepl(text, tolower(indexingTitles$titleMetaAnalysis), fixed = TRUE)
        )
    }), ncol = length(searchPositive)), 1, any)

    textSearchNegative <- apply(matrix(sapply(searchNegative, function(text) {
      return(
        grepl(text, tolower(indexingTitles$titleReview), fixed = TRUE) |
          grepl(text, tolower(indexingTitles$titleMetaAnalysis), fixed = TRUE)
        )
    }), ncol = length(searchNegative)), 1, any)

    if (length(textSearchNegative) == 0)
      selected <- textSearchPositive
    else
      selected <- textSearchPositive & !textSearchNegative

    indexingTitles <- indexingTitles[selected,]

    if (nrow(indexingTitles) == 0){
      jaspResults[["selectedOverviewTable"]]$addFootnote(gettext(
        "There is no matching meta-analysis title for the specified text search"), symbol = "Note: ")
      return()
    }

  }


  if (options[["selectionType"]] %in% c("selectionTopics", "selectionKeywords")) {

    selectedReviews  <- indexing[selectedTitles]
    reviewsStructure <- lapply(selectedReviews, function(review) review[["metaAnalyses"]])

  } else if (options[["selectionType"]] == "selectionTextSearch") {

    selectedReviews <- indexing[unique(indexingTitles$titleReview)]
    for(titleReview in names(selectedReviews)) {
      selectedReviews[[titleReview]]$metaAnalyses <- selectedReviews[[titleReview]]$metaAnalyses[indexingTitles$titleMetaAnalysis[indexingTitles$titleReview == titleReview]]
    }
    reviewsStructure <- lapply(selectedReviews, function(review) review[["metaAnalyses"]])

  }

  if (length(reviewsStructure) > 0) {
    jaspResults[["selectorGadget"]] <- createJaspQmlSource("selectorGadget", reviewsStructure)
    jaspResults[["selectorGadget"]]$dependOn(c("selectionType", "topicsSelected", "keywordsSelected", "textSearch"))
  }

  return()
}
.cochraneCreatetargetGroupGadget <- function(jaspResults, options) {

  # obtain the selected meta-analyses
  selectedMetaAnalyses <- .cochraneExtractReviewsOptions(options, groups = FALSE)

  if (length(selectedMetaAnalyses) == 0)
    return()

  # load the reviews meta-data
  reviews <- jaspResults[["database"]]$object[["reviews"]]
  reviews <- reviews[names(reviews) %in% unique(selectedMetaAnalyses$titleReview)]

  reviewsStructure <- lapply(names(reviews), function(titleReview) {
    titleMetaAnalysis <- selectedMetaAnalyses[selectedMetaAnalyses$titleReview == titleReview,"titleMetaAnalysis"]
    return(reviews[[titleReview]][["metaAnalysesGroups"]][titleMetaAnalysis])
  })
  names(reviewsStructure) <- names(reviews)

  if (length(reviewsStructure) > 0) {
    jaspResults[["targetGroupGadget"]] <- createJaspQmlSource("targetGroupGadget", reviewsStructure)
    jaspResults[["targetGroupGadget"]]$dependOn(c("selectionType", "topicsSelected", "keywordsSelected", "textSearch", "reviews"))
  }

  return()
}
.cochraneEmulateClassicalMetaAnalysisOptions <- function(options) {

  options[["effectSize"]]     <- "effectSize"
  options[["effectSizeSe"]]   <- "effectSizeSe"
  options[["interceptTerm"]]  <- TRUE
  options[["studyLabel"]]     <- "studyLabel"
  options[["factors"]]        <- list()
  options[["covariates"]]     <- list()
  options[["modelTerms"]]     <- list()
  options[["components"]]     <- list()

  return(options)
}
.cochraneEmulateBayesianMetaAnalysisOptions  <- function(options) {

  options[["effectSize"]]     <- "effectSize"
  options[["effectSizeSe"]]   <- "effectSizeSe"
  options[["studyLabel"]]     <- "studyLabel"

  return(options)
}
.cochraneGetDichotomousEffectSizeNameOptions <- function(options, exponentiated = FALSE) {
  if (exponentiated)
    return(switch(
      options[["analyzeAs"]],
      "logOr"  = gettext("Odds Ratio"),
      "logPor" = gettext("Peto's Odds Ratio"),
      "Rd"     = gettext("Risk Difference"),
      "logRr"  = gettext("Risk Ratio")
    ))
  else
    return(switch(
      options[["analyzeAs"]],
      "logOr"  = gettext("Log(Odds Ratio)"),
      "logPor" = gettext("Log(Peto's Odds Ratio)"),
      "Rd"     = gettext("Risk Difference"),
      "logRr"  = gettext("Log(Risk Ratio)")
    ))
}
.cochranePriorWarning           <- function(jaspResults) {

  if (!is.null(jaspResults[["priorWarning"]]))
    return()

  priorWarning <- createJaspHtml()
  priorWarning$position <- 0

  priorWarning[["text"]] <- gettext("Please note that the default prior distributions for the effect sizes and heterogeneity were not developed for dichotomous outcomes. As a consequence, the Bayes factors for the presence/absence of the effect/heterogeneity are not guaranteed to behave well.")
  jaspResults[["priorWarning"]] <- priorWarning

  return()
}
.cochraneEffectSizeTypeOption   <- function(options) {
  return(switch(
    options[["analyzeAs"]],
    "logOr"  = "OR",
    "logPor" = "PETO",
    "logRr"  = "RR",
    "Rd"     = "RD"
  ))
}
.cochraneExtractReviewsOptions  <- function(options, groups = TRUE) {
  # loop over reviews
  selectedReviewsMetaAnalyses <- do.call(rbind, lapply(options[["reviews"]], function(review) {
    # loop over meta-analyses
    do.call(rbind, lapply(review[["metaAnalyses"]], function(metaAnalyses) {
      # check if meta-analysis is selected
      if (!is.null(metaAnalyses[["checkMeta"]]) && metaAnalyses[["checkMeta"]])
        if(groups){
          # dispatch according presence/absence of subgroup analyses
          if (length(metaAnalyses[["outcome"]]) == 0){
            # deal with no subgroups
            return(data.frame(
              titleReview      = review[["value"]],
              titleMetaAnalysis = metaAnalyses[["value"]],
              titleGroup       = ""
            ))
          } else {
            return(do.call(rbind, lapply(metaAnalyses[["outcome"]], function(outcome) {
              # deal with subgroups
              if (outcome[["checkOutcome"]])
                return(data.frame(
                  titleReview      = review[["value"]],
                  titleMetaAnalysis = metaAnalyses[["value"]],
                  titleGroup       = outcome[["value"]]
                ))
            })))
          }
        }else{
          return(data.frame(
            titleReview      = review[["value"]],
            titleMetaAnalysis = metaAnalyses[["value"]]
          ))
        }
    }))
  }))

  return(selectedReviewsMetaAnalyses)
}
.cochraneSetDataError           <- function(container) {

  errorTable <- createJaspTable(title = gettext("Meta-Analyses"))
  errorTable$setError(gettext("At least two effect size estimates are required to compute a meta-analysis."))
  container[["errorTable"]] <- errorTable

  return()
}
.cochraneExportData             <- function(jaspResults, dataset, options) {

  if (is.null(jaspResults[["dataSaved"]])) {
    dataSaved <- createJaspState()
    dataSaved$dependOn(c(.cochraneDataDependencies, "savePath"))
    jaspResults[["dataSaved"]] <- dataSaved
  }

  columnsToExport <- c("studyLabel","effectSize","effectSizeSe","sampleSize","studyYear","titleReview","titleMetaAnalysis",	"titleGroup")

  if (options[["analyzeData"]] == "individually") {
    selection  <- unique(dataset[,"titleMetaAnalysis"])
    selection  <- selection[selection != "_add"]

    for (i in seq_along(selection)) {
      title         <- sort(selection, decreasing = TRUE)[i]
      tempDataset   <- dataset[dataset[,"titleMetaAnalysis"] %in% c("_add", title),]
      utils::write.csv(
        x         = tempDataset[,columnsToExport],
        file      = gsub(".csv", paste0("-", i, ".csv"), options[["savePath"]], fixed = TRUE),
        row.names = FALSE
      )
    }

  }else{
    utils::write.csv(
      x         = dataset[,columnsToExport],
      file      = options[["savePath"]],
      row.names = FALSE
    )
  }

  dataSaved[["object"]] <- TRUE
  return()
}
.ClassicalMetaAnalysisAddExponentialSummary <- function(container, ready, options) {
  # reconstruct the table as the original one did not have to contain CIs
  if (!options[["analyzeAs"]] %in% c("logOr", "logPor", "logRr"))
    return()

  if (!is.null(container[["coeffTableExponentiated"]]))
    return()

  coeffTableExponentiated <- createJaspTable(gettextf("Transformed Coefficients: %1$s", .cochraneGetDichotomousEffectSizeNameOptions(options, exponentiated = TRUE)))
  coeffTableExponentiated$dependOn(c("coefficientEstimate", "coefficientCi"))
  coeffTableExponentiated$position <- 2.1
  container[["coeffTableExponentiated"]] <- coeffTableExponentiated
  ci <- gettextf("%g%% Confidence Interval", 100 * options$coefficientCiLevel)
  coeffTableExponentiated$addColumnInfo(name = "name",  type = "string", title = "")
  coeffTableExponentiated$addColumnInfo(name = "est",   type = "number", title = gettext("Estimate"))
  coeffTableExponentiated$addColumnInfo(name = "lower", type = "number", title = "Lower", overtitle = ci)
  coeffTableExponentiated$addColumnInfo(name = "upper", type = "number", title = "Upper", overtitle = ci)

  if (!ready)
    return()

  coeff <- coef(summary(container[["Model"]]$object))
  container[["coeffTableExponentiated"]]$addRows(list(
    name  = "intercept",
    est   = exp(coeff[1, 1]),
    lower = exp(coeff[1, 5]),
    upper = exp(coeff[1, 6])
  ))

  return()
}
.BayesianMetaAnalysisAddExponentialSummary  <- function(container, ready, options) {

  # exponentiate estimates from the previous table
  if (!options[["analyzeAs"]] %in% c("logOr", "logPor", "logRr"))
    return()

  if (!is.null(container[["coeffTableExponentiated"]]))
    return()

  coeffTableExponentiated <- createJaspTable(gettextf("Transformed Posterior Estimates per Model: %1$s", .cochraneGetDichotomousEffectSizeNameOptions(options, exponentiated = TRUE)))
  coeffTableExponentiated$dependOn(c("coefficientEstimate", "coefficientCi"))
  coeffTableExponentiated$position <- 3.1
  container[["coeffTableExponentiated"]] <- coeffTableExponentiated
  ci <- gettextf("%g%% Credible Interval", 100 * 0.95)
  coeffTableExponentiated$addColumnInfo(name = "model",     type = "string", title = "")
  coeffTableExponentiated$addColumnInfo(name = "parameter", type = "string", title = "")
  coeffTableExponentiated$addColumnInfo(name = "ES",        type = "number", title = gettext("Estimate"))
  coeffTableExponentiated$addColumnInfo(name = "lb",        type = "number", title = "Lower", overtitle = ci)
  coeffTableExponentiated$addColumnInfo(name = "ub",        type = "number", title = "Upper", overtitle = ci)

  if (!ready)
    return()

  tableData <- data.frame(container[["bmaTable"]]$toRObject())
  tableData <- tableData[tableData$.isNewGroup, ]                     # remove tau estimates
  tableData <- tableData[,c("model", "parameter", "ES", "lb", "ub")]  # remove unnecessary columns
  # get the exponential transformation
  for(col in c("ES", "lb", "ub")){
    tableData[,col] <- exp(as.numeric(tableData[,col]))
  }

  coeffTableExponentiated$setData(tableData)

  return()
}
