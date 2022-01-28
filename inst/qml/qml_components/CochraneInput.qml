//
// Copyright (C) 2013-2018 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//
import QtQuick			2.8
import QtQuick.Layouts	1.3
import JASP.Controls	1.0
import JASP.Widgets		1.0
import JASP				1.0
import "../qml_components" as MA

Section
{
	title: 		qsTr("Database")
	expanded:	true

	property string analysisType:	"classicalContinuous"


	VariablesForm
	{
		preferredHeight:	300 * preferencesModel.uiScale
		visible:			selectionTopics.checked

		AvailableVariablesList
		{
			name:	"topicsMenu"
			title:	qsTr("Topics")
			source:	[{ rSource: "sourceTopics"}]
		}

		AssignedVariablesList
		{
			name:	"topicsSelected"
			title:	qsTr("Selected")
		}
	}

	Group
	{
		visible:			selectionKeywords.checked
		Layout.columnSpan:	2

		VariablesForm
		{
			preferredHeight:	300 * preferencesModel.uiScale
			

			AvailableVariablesList
			{
				height:	250
				name:	"keywordsMenu"
				title:	qsTr("Keywords")
				source:	[{ rSource: "sourceKeywords"}]
			}

			AssignedVariablesList
			{
				name:	"keywordsSelected"
				title:	qsTr("Selected")
			}
		}

		TextField
		{
			name:	"keywordsSearch"
			label:	qsTr("Search")
			value:	""
		}
	}

	TextArea
	{
		Layout.columnSpan:	2
		height:		300 * preferencesModel.uiScale
		visible:	selectionTextSearch.checked
		text:		""
		name:		"textSearch"
		textType:	JASP.TextTypeSource
	}


	RadioButtonGroup
	{
		columns:			3
		name:				"selectionType"
		title:				qsTr("Select systematic reviews based on")
		id:					dataType

		RadioButton
		{
			value:		"selectionTopics"
			label:		qsTr("Topics")
			id:			selectionTopics
			checked:	true
		}

		RadioButton
		{
			value:		"selectionKeywords"
			label:		qsTr("Keywords")
			id:			selectionKeywords
		}

		RadioButton
		{
			value:		"selectionTextSearch"
			label:		qsTr("Search titles")
			id:			selectionTextSearch
		}
	}

	DropDown
	{
		name:				"analyzeAs"
		label:				qsTr("Analyze as")
		visible:			analysisType === "classicalDichotomous" || analysisType === "bayesianDichotomous"
		values:
		[
			{label: qsTr("Log(odds ratios)"),			value: "OR"},
			{label: qsTr("Log(Peto's odds ratios)"),	value: "POR"},
			{label: qsTr("Log(risk ratios)"),			value: "RR"},
			{label: qsTr("Risk differences"),			value: "RD"	}
		]
	}

	ComponentsList
	{
		title:				qsTr("Selected Reviews/Meta-Analyses")
		name:				"reviews"
		rSource: 			"selectorGadget"
		implicitHeight:		350 * preferencesModel.uiScale
		Layout.columnSpan:	2

		rowComponent: Item
		{
			height:	textLable.height + comp.height
			width:	parent.width
			
			Text
			{
				id:		textLable
				text:	rowValue
			}
			
			ComponentsList
			{
				id:				comp
				anchors.top:	textLable.bottom
				name:			"metaAnalyses"
				rSource:		[{name: "selectorGadget", use: rowValue }]
				rowComponent:	CheckBox { name: "check"; label: rowValue }
			}
		}
	}

	Group
	{

		RadioButtonGroup
		{
			columns:	1
			name:		"analyzeData"
			title:		qsTr("Analyze data")

			RadioButton
			{
				value:		"individually"
				label:		qsTr("Individually")
			}

			RadioButton
			{
				value:		"pooled"
				label:		qsTr("Pooled")
			}
		}

		MA.ClassicalMetaAnalysisMethod
		{
			visible:	analysisType === "classicalContinuous" || analysisType === "classicalDichotomous"
		}
	}

	Group
	{
		title: 	qsTr("Distribution plot")
		CheckBox
		{
			name:		"plotEffectSizes"
			label:		qsTr("Effect sizes")
			id:			plotEffectSizes
		}

		CheckBox
		{
			name:		"plotSampleSizes"
			label:		qsTr("Sample sizes")
			id:			plotSampleSizes
		}

		CheckBox
		{
			name:		"distPlotDensity"
			label:		qsTr("Add density")
			enabled:	plotEffectSizes.checked || plotSampleSizes.checked
		}

		CheckBox
		{
			name:		"distPlotRug"
			label:		qsTr("Add rug marks")
			enabled:	plotEffectSizes.checked || plotSampleSizes.checked
		}

		DropDown
		{
			name:		"binWidthType"
			label:		qsTr("Bin width type")
			enabled:	plotEffectSizes.checked || plotSampleSizes.checked
			indexDefaultValue: 0
			values:
			[
				{label: qsTr("Sturges"),				value: "sturges"},
				{label: qsTr("Scott"),					value: "scott"},
				{label: qsTr("Doane"),					value: "doane"},
				{label: qsTr("Freedman-Diaconis"),		value: "fd"	},
				{label: qsTr("Manual"),					value: "manual"}
			]
			id: binWidthType
		}

		DoubleField
		{
			name:			"numberOfBins"
			label:			qsTr("Number of bins")
			defaultValue:	30
			min:			3;
			max:			10000;
			enabled:		binWidthType.currentValue === "manual" && (plotEffectSizes.checked || plotSampleSizes.checked)
		}
	}

	CheckBox
	{
		Layout.columnSpan:	2
		name:				"addStudy"
		label:				qsTr("Add estimates")
		id:					addStudy
	}

	ColumnLayout
	{
		spacing:				0
		visible:				addStudy.checked
		Layout.preferredWidth:	parent.width
		Layout.columnSpan:		2

		RowLayout
		{
			Label
			{
				text:					qsTr("Study")
				Layout.preferredWidth:	200 * preferencesModel.uiScale; Layout.leftMargin: 5 * preferencesModel.uiScale
			}
			Label
			{
				text:					qsTr("Effect Size")
				Layout.preferredWidth:  50 * preferencesModel.uiScale
			}
			Label
			{
				text:					qsTr("Standard Error")
				Layout.preferredWidth:	if (analysisType === "classicalContinuous" || analysisType === "bayesianContinuous") {100 * preferencesModel.uiScale} else {75 * preferencesModel.uiScale}
			}
			Label
			{
				text:					if (analysisType === "classicalContinuous" || analysisType === "bayesianContinuous") {qsTr("Confidence Interval")} else {qsTr("Frequencies")}
			}
		}

		ComponentsList
		{
			name:					"additionalStudies"
			optionKey:				"name"
			Layout.maximumHeight:	200
			rowComponent: 			RowLayout
			{
				Row
				{
					spacing:				4 * preferencesModel.uiScale
					Layout.preferredWidth:	200 * preferencesModel.uiScale
					TextField
					{
						name: 				"titleStudy"
						useExternalBorder: 	true
					}
				}

				Row
				{
					spacing:				4 * preferencesModel.uiScale
					Layout.preferredWidth: 50 * preferencesModel.uiScale
					TextField
					{
						name:				"effectSize"
						value:				""
						fieldWidth: 		40 * preferencesModel.uiScale
						useExternalBorder:	false
						showBorder: 		true
					}
				}

				Row
				{
					spacing:				 4 * preferencesModel.uiScale
					Layout.preferredWidth:	if (analysisType === "classicalContinuous" || analysisType === "bayesianContinuous") {100 * preferencesModel.uiScale} else {75 * preferencesModel.uiScale}
					TextField
					{
						name:				"effectSE"
						id:					effectSE
						value:				""
						fieldWidth: 		40 * preferencesModel.uiScale
						useExternalBorder:	false
						showBorder: 		true
						//enabled:			lCI.value === "" && uCI.value === ""
					}
				}

				Row
				{
					spacing:				4 * preferencesModel.uiScale
					Layout.preferredWidth:	if (analysisType === "classicalContinuous" || analysisType === "bayesianContinuous") {155 * preferencesModel.uiScale} else {150 * preferencesModel.uiScale}
					TextField
					{
						name:				"lCI"
						label:				qsTr("lCI")
						id:					lCI
						visible:			analysisType === "classicalContinuous" || analysisType === "bayesianContinuous"
						value:				""
						fieldWidth: 		40 * preferencesModel.uiScale
						useExternalBorder:	false
						showBorder: 		true
						//enabled:			effectSE.value === ""
					}
					TextField
					{
						name:				"uCI"
						label:				qsTr("uCI")
						visible:			analysisType === "classicalContinuous" || analysisType === "bayesianContinuous"
						id:					uCI
						value:				""
						fieldWidth: 		40 * preferencesModel.uiScale
						useExternalBorder:	false
						showBorder: 		true
						//enabled:			effectSE.value === ""
					}
					TextField
					{
						name:				"x1"
						label:				qsTr("x₁")
						id:					x1
						visible:			analysisType === "classicalDichotomous" || analysisType === "bayesianDichotomous"
						value:				""
						fieldWidth: 		30 * preferencesModel.uiScale
						useExternalBorder:	false
						showBorder: 		true
						//enabled:			effectSE.value === ""
					}
					TextField
					{
						name:				"n1"
						label:				qsTr("n₁")
						id:					n1
						visible:			analysisType === "classicalDichotomous" || analysisType === "bayesianDichotomous"
						value:				""
						fieldWidth: 		30 * preferencesModel.uiScale
						useExternalBorder:	false
						showBorder: 		true
						//enabled:			effectSE.value === ""
					}
					TextField
					{
						name:				"x2"
						label:				qsTr("x₂")
						visible:			analysisType === "classicalDichotomous" || analysisType === "bayesianDichotomous"
						id:					x2
						value:				""
						fieldWidth: 		30 * preferencesModel.uiScale
						useExternalBorder:	false
						showBorder: 		true
						//enabled:			effectSE.value === ""
					}
					TextField
					{
						name:				"n2"
						label:				qsTr("n₂")
						visible:			analysisType === "classicalDichotomous" || analysisType === "bayesianDichotomous"
						id:					n2
						value:				""
						fieldWidth: 		30 * preferencesModel.uiScale
						useExternalBorder:	false
						showBorder: 		true
						//enabled:			effectSE.value === ""
					}
				}
			}
		}
	}
}