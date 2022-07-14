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
    .cochraneSelectedOverviewTable(jaspResults, options)

  ### add additional arguments for the classical/Bayesian meta-analysis
  if (type %in% c("classicalContinuous", "classicalDichotomous"))
    options <- .cochraneEmulateClassicalMetaAnalysisOptions(options, type)
  else if (type %in% c("bayesianContinuous", "bayesianDichotomous"))
    options <- .cochraneEmulateBayesianMetaAnalysisOptions(options, type)

  ready   <- .cochraneReady(options, dataset)


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
  }


  return()
}

.cochraneDataDependencies       <- c("selectionType", "topicsSelected", "keywordsSelected", "textSearch", "analyzeData",
                                     "addStudy", "additionalStudies", "analyzeAs")
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

  for (col in c("effectSize", "effectSE", "lCI", "uCI"))
    additionalEstimates[,col] <- as.numeric(additionalEstimates[,col])

  additionalEstimates <- additionalEstimates[!is.na(additionalEstimates[,"effectSize"]),]
  additionalEstimates <- additionalEstimates[!is.na(additionalEstimates[,"effectSE"]) | (!is.na(additionalEstimates[,"lCI"]) & !is.na(additionalEstimates[,"uCI"])),]

  if (nrow(additionalEstimates) == 0)
    return(dataset)

  for (i in 1:nrow(additionalEstimates))
    if (is.na(additionalEstimates[i,"effectSE"]) && all(is.numeric(unlist(additionalEstimates[i, c("lCI", "uCI")])))) {
      if (additionalEstimates[i,"lCI"] > additionalEstimates[i,"effectSize"] || additionalEstimates[i,"uCI"] < additionalEstimates[i,"effectSize"])
        .quitAnalysis(gettext("The effect size does not lie within the confidence interval in one of the specified studies."))
      additionalEstimates[i,"effectSE"] <- (additionalEstimates[i,"uCI"] - additionalEstimates[i,"lCI"]) / (qnorm(.975) * 2)
    }


  if (any(additionalEstimates[,"effectSE"] < 0))
    .quitAnalysis(gettext("One of the specified studies has a negative standard error."))

  additionalEstimates <- additionalEstimates[,c("effectSize",  "effectSE", "titleStudy")]
  additionalEstimates$titleStudy        <- paste0("_add", additionalEstimates$titleStudy)
  additionalEstimates$studyYear         <- NA
  additionalEstimates$reviewYear        <- NA
  additionalEstimates$doi               <- "_add"
  additionalEstimates$titleReview       <- "_add"
  additionalEstimates$titleMetaAnalysis <- "_add"
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

  for (col in c("effectSize", "effectSE", "x1", "x2", "n1", "n2"))
    additionalEstimates[,col] <- as.numeric(additionalEstimates[,col])

  additionalEstimates <- additionalEstimates[(!is.na(additionalEstimates[,"effectSE"]) & !is.na(additionalEstimates[,"effectSize"])) | (!is.na(additionalEstimates[,"x1"]) & !is.na(additionalEstimates[,"x2"]) & !is.na(additionalEstimates[,"n1"]) & !is.na(additionalEstimates[,"n2"])),]

  if (nrow(additionalEstimates) == 0)
    return(dataset)

  for (i in 1:nrow(additionalEstimates))
    if (is.na(additionalEstimates[i,"effectSE"]) && all(is.numeric(unlist(additionalEstimates[i, c("x1", "x2", "n1", "n2")])))) {
      if (any(unlist(additionalEstimates[i, c("x1", "x2", "n1", "n2")]) < 0))
        .quitAnalysis(gettext("All specified frequencies need to be larger than zero."))
      if (any(unlist(additionalEstimates[i, c("n1", "n2")]) < unlist(additionalEstimates[i, c("x1", "x2")])))
        .quitAnalysis(gettext("The number of events must be lower than the number of observations."))

      tempMeasure <- with(
        additionalEstimates[i,],
        metafor::escalc(
          measure = if (options[["analyzeAs"]] == "POR") "PETO" else options[["analyzeAs"]],
          ai   = x1,
          ci   = x2,
          n1i  = n1,
          n2i  = n2
        ))

      additionalEstimates$effectSize[i]  <- tempMeasure[1,1]
      additionalEstimates$effectSE[i]    <- sqrt(tempMeasure[1,2])
    }

  if (any(additionalEstimates[,"effectSE"] < 0))
    .quitAnalysis(gettext("One of the specified studies has a negative standard error."))

  additionalEstimates <- additionalEstimates[,c("effectSize",  "effectSE", "titleStudy")]
  colnames(additionalEstimates)[1:2]    <- paste0(colnames(additionalEstimates)[1:2], options[["analyzeAs"]])
  for (notComputed in c("OR","POR","RR","RD")[!c("OR","POR","RR","RD") %in% options[["analyzeAs"]]])
    additionalEstimates[,paste0(c("effectSize",  "effectSE"), notComputed)] <- NA
  additionalEstimates$titleStudy        <- paste0("_add", additionalEstimates$titleStudy)
  additionalEstimates$studyYear         <- NA
  additionalEstimates$reviewYear        <- NA
  additionalEstimates$doi               <- "_add"
  additionalEstimates$titleReview       <- "_add"
  additionalEstimates$titleMetaAnalysis <- "_add"
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
  dataset$dependOn("reviews")
  jaspResults[["dataset"]] <- dataset

  # obtain the selected meta-analyses
  selectedReviewsMetaAnalyses <- .cochraneExtractReviewsOptions(options)

  if (length(selectedReviewsMetaAnalyses) == 0)
    return()

  # select the data set
  studies <- jaspResults[["database"]]$object[["studies"]]

  # multiple meta-analyses have the same title - first select by review, then by meta-analysis
  studies <- do.call(rbind, lapply(1:nrow(selectedReviewsMetaAnalyses), function(i) {
    tempReview <- studies[studies[,"titleReview"] == selectedReviewsMetaAnalyses[i, "review"],]
    return(tempReview[tempReview[,"titleMetaAnalysis"] == selectedReviewsMetaAnalyses[i, "metaAnalysis"],])
  }))

  dataset[["object"]] <- studies

  return(studies)
}
.cochraneSelectedOverviewTable  <- function(jaspResults, options) {

  # create overview from the selected meta-analyses
  selectedOverviewTable <- createJaspTable(title = gettext("Meta-Analyses"))
  selectedOverviewTable$addColumnInfo(name = "title",    title = gettext("Title"),             type = "string")
  selectedOverviewTable$addColumnInfo(name = "year",     title = gettext("Year"),              type = "integer")
  selectedOverviewTable$addColumnInfo(name = "nStudies", title = gettext("Number of studies"), type = "integer")
  selectedOverviewTable$position <- 1
  selectedOverviewTable$dependOn("reviews")
  jaspResults[["selectedOverviewTable"]] <- selectedOverviewTable

  # obtain the selected meta-analyses
  selectedReviewsMetaAnalyses <- .cochraneExtractReviewsOptions(options)

  if (length(selectedReviewsMetaAnalyses) == 0)
    return()

  reviews      <- jaspResults[["database"]]$object[["reviews"]]
  metaAnalyses <- lapply(1:nrow(selectedReviewsMetaAnalyses), function(i) {
    tempReview <- reviews[[selectedReviewsMetaAnalyses[i,"review"]]]
    return(list(
      title    = selectedReviewsMetaAnalyses[i, "metaAnalysis"],
      year     = tempReview[["year"]],
      nStudies = tempReview[["nStudies"]][tempReview[["titleMetaAnalyses"]] == selectedReviewsMetaAnalyses[i, "metaAnalysis"]]
    ))
  })

  for (i in seq_along(metaAnalyses))
    selectedOverviewTable$addRows(metaAnalyses[[i]])

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
      column         = .cochraneGetPlotVariable(dataset, variable, options, type),
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
      return(switch(
        options[["analyzeAs"]],
        "OR"  = gettext("Log(Odds Ratio)"),
        "POR" = gettext("Log(Peto's Odds Ratio)"),
        "RD"  = gettext("Risk Difference"),
        "RR"  = gettext("Log(Risk Ratio)")
      ))
    else if (variable == "sampleSize")
      return(gettext("Sample Size"))
  }
}
.cochraneGetPlotVariable        <- function(dataset, variable, options, type) {

  if (type %in% c("classicalContinuous", "bayesianContinuous")) {
    return(dataset[[variable]])
  } else if (type %in% c("classicalDichotomous", "bayesianDichotomous")) {
    if (variable == "effectSize")
      return(dataset[[paste0(variable, options[["analyzeAs"]])]])
    else if (variable == "sampleSize")
      return(dataset[[variable]])
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

    indexing   <- database[["metaAnalyses"]]
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

    indexingTitles <- tolower(unname(sapply(indexing, function(indx) indx[["titleMetaAnalysis"]])))

    textSearchPositive <- apply(matrix(sapply(searchPositive, function(text) {
      return(grepl(text, indexingTitles, fixed = TRUE))
    }), ncol = length(searchPositive)), 1, any)

    textSearchNegative <- apply(matrix(sapply(searchNegative, function(text) {
      return(grepl(text, indexingTitles, fixed = TRUE))
    }), ncol = length(searchNegative)), 1, any)

    if (length(textSearchNegative) == 0)
      selectedTitles <- sapply(indexing, function(inx) inx[["titleMetaAnalysis"]])[textSearchPositive]
    else
      selectedTitles <- sapply(indexing, function(inx) inx[["titleMetaAnalysis"]])[textSearchPositive & !textSearchNegative]

    if (length(selectedTitles) == 0){
      jaspResults[["selectedOverviewTable"]]$addFootnote(gettext(
        "There is no matching meta-analysis title for the specified text search"), symbol = "Note: ")
      return()
    }

  }


  if (options[["selectionType"]] %in% c("selectionTopics", "selectionKeywords")) {

    selectedReviews  <- indexing[selectedTitles]
    reviewsStructure <- lapply(selectedReviews, function(review) review[["titleMetaAnalyses"]])

  } else if (options[["selectionType"]] == "selectionTextSearch") {

    selectedMetaAnalyses <- indexing[selectedTitles]
    selectedMetaAnalyses <- do.call(rbind, lapply(selectedMetaAnalyses, function(metaAnalysis) c("review" = metaAnalysis[["titleReview"]], "metaAnalysis" = metaAnalysis[["titleMetaAnalysis"]])))
    reviewsStructure     <- split(selectedMetaAnalyses[,"metaAnalysis"], selectedMetaAnalyses[,"review"])

  }

  if (length(reviewsStructure) > 0) {
    jaspResults[["selectorGadget"]] <- createJaspQmlSource("selectorGadget", reviewsStructure)
    jaspResults[["selectorGadget"]]$dependOn(c("selectionType", "topicsSelected", "keywordsSelected", "textSearch"))
  }

  return()
}
.cochraneEmulateClassicalMetaAnalysisOptions <- function(options, type) {

  if (type == "classicalContinuous") {
    options[["dependent"]]       <- "effectSize"
    options[["wlsWeights"]]      <- "effectSE"
  } else if (type == "classicalDichotomous") {
    options[["dependent"]]       <- paste0("effectSize", options[["analyzeAs"]])
    options[["wlsWeights"]]      <- paste0("effectSE",   options[["analyzeAs"]])
  }

  options[["includeConstant"]] <- TRUE
  options[["studyLabels"]]     <- "titleStudy"
  options[["factors"]]         <- list()
  options[["covariates"]]      <- list()
  options[["modelTerms"]]      <- list()
  options[["components"]]      <- list()

  return(options)
}
.cochraneEmulateBayesianMetaAnalysisOptions  <- function(options, type) {

  if (type == "bayesianContinuous") {
    options[["effectSize"]]       <- "effectSize"
    options[["standardError"]]    <- "effectSE"
  } else if (type == "bayesianDichotomous") {
    options[["effectSize"]]       <- paste0("effectSize", options[["analyzeAs"]])
    options[["standardError"]]    <- paste0("effectSE",   options[["analyzeAs"]])
  }

  options[["studyLabels"]]     <- "titleStudy"

  return(options)
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
.cochraneExtractReviewsOptions  <- function(options) {

  selectedReviewsMetaAnalyses <- do.call(rbind, lapply(options[["reviews"]], function(review) {
    do.call(rbind, lapply(review[["metaAnalyses"]], function(metaAnalyses) {
      if (metaAnalyses[["check"]])
        return(c(review = review[["value"]], metaAnalysis = metaAnalyses[["value"]]))
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
