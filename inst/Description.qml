import QtQuick
import JASP.Module

Description
{
	title		: qsTr("Cochrane Meta-Analyses")
	description	: qsTr("This module allows to analyze Cochrane medical datasets.")
	icon		: "cochraneLogo.svg"
	requiresData: false
	hasWrappers:  false
	preloadData:  false


	GroupTitle
	{
		title:	qsTr("Classical")
		icon:	"cochraneLogo.svg"
	}

	Analysis
	{
		menu:	qsTr("Continuous Outcomes")
		title:	qsTr("Classical Continuous Outcomes")
		func:	"CochraneContinuousClassicalMetaAnalysis"
	}

	Analysis
	{
		menu:	qsTr("Dichotomous Outcomes")
		title:	qsTr("Classical Dichotomous Outcomes")
		func:	"CochraneDichotomousClassicalMetaAnalysis"
	}

	GroupTitle
	{
		title:	qsTr("Bayesian")
		icon:	"cochraneLogo2.svg"
	}

	Analysis
	{
		menu:	qsTr("Continuous Outcomes")
		title:	qsTr("Bayesian Continuous Outcomes")
		func:	"CochraneContinuousBayesianMetaAnalysis"
	}

	Analysis
	{
		menu:	qsTr("Dichotomous Outcomes")
		title:	qsTr("Bayesian Dichotomous Outcomes")
		func:	"CochraneDichotomousBayesianMetaAnalysis"
	}

}
