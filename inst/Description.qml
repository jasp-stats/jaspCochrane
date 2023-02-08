import QtQuick 		2.12
import JASP.Module 	1.0

Description
{
	name		: "jaspCochrane"
	title		: qsTr("Cochrane Meta-Analyses")
	description	: qsTr("This module allows to analyze Cochrane medical datasets.")
	icon		: "cochraneLogo.svg"
	version		: "0.17.1"
	author		: "JASP Team"
	maintainer	: "JASP Team <info@jasp-stats.org>"
	website		: "jasp-stats.org"
	license		: "GPL (>= 2)"
	requiresData: false
		

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
