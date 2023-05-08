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
import jaspMetaAnalysis 1.0

Section
{
	title: 		qsTr("Database")
	expanded:	true

	property string analysisType:	"classicalContinuous"


	RadioButtonGroup
	{
		columns:			3
		name:				"selectionType"
		title:				qsTr("Select Systematic Reviews Based On")
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
			name:		"topicsSelected"
			title:		qsTr("Selected")
			maxRows:	5
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
				name:		"keywordsSelected"
				title:		qsTr("Selected")
				maxRows:	10
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


	ComponentsList
	{
		id:					reviews
		title:				qsTr("Selected Reviews/Meta-Analyses")
		name:				"reviews"
		rSource: 			"selectorGadget"
		implicitHeight:		350 * preferencesModel.uiScale
		Layout.columnSpan:	2

		rowComponent: Item
		{
			height:	textReview.height + metaAnalysisComp.height
			width:	reviews.width - 20 * preferencesModel.uiScale

			Text
			{
				id:			textReview
				text:		rowValue
				wrapMode:	Text.Wrap
				width:		parent.width
			}

			ComponentsList
			{
				id:				metaAnalysisComp
				anchors.top:	textReview.bottom
				name:			"metaAnalyses"
				rSource:		[{name: "selectorGadget", use: rowValue }]

				property string metaValue: rowValue

				rowComponent:	Item
				{
					height:	checkMeta.height + outcomeComp.height
					width:	reviews.width - 20 * preferencesModel.uiScale

					CheckBox
					{
						name:	"checkMeta"
						label:	rowValue
						id:		checkMeta
					}

					ComponentsList
					{
						id:				outcomeComp
						visible:		checkMeta.checked && count > 0
						height:			visible ? implicitHeight: 0
						anchors.top:	checkMeta.bottom
						anchors.left:	checkMeta.left
						anchors.leftMargin: 20 * preferencesModel.uiScale
						name:			"outcome"
						rSource:		[{name: "selectorGadget", use: metaAnalysisComp.metaValue + "." + rowValue }]
						rowComponent:	CheckBox { name: "checkOutcome"; label: rowValue; checked: rowIndex == 0}
						//addBorder:	false
					}
				}
			}
		}
	}

	CheckBox
	{
		name:				"changeTargetGroup"
		id:					changeTargetGroup
		checked:			false
		label:				qsTr("Change target group")
		Layout.columnSpan:	2
	}

	ComponentsList
	{
		id:					targetGroup
		visible:			changeTargetGroup.checked
		title:				""
		name:				"targetGroup"
		rSource: 			"targetGroupGadget"
		implicitHeight:		350 * preferencesModel.uiScale
		Layout.columnSpan:	2

		rowComponent: Item
		{
			height:	textReviewGroup.height + metaAnalysisCompGroup.height
			width:	targetGroup.width - 20 * preferencesModel.uiScale

			Text
			{
				id:			textReviewGroup
				text:		rowValue
				wrapMode:	Text.Wrap
				width:		parent.width
			}

			ComponentsList
			{
				id:				metaAnalysisCompGroup
				anchors.top:	textReviewGroup.bottom
				name:			"metaAnalysesGroups"
				rSource:		[{name: "targetGroupGadget", use: rowValue }]

				rowComponent: RadioButtonGroup
				{
					id:		checkMetaGroup
					name:	"checkMetaGroup"
					title:	rowValue

					ComponentsList
					{
						name:			"metaAnalysesGroupChoice"
						rSource:		[{name: "targetGroupGadget", use: textReviewGroup.text + "." +  checkMetaGroup.title}]
						width:			targetGroup.width - 40 * preferencesModel.uiScale

						rowComponent: RadioButton { name: rowValue; label: rowValue }
					}

				}
			}

		}
	}


	Group
	{

		/*CheckBox
		{
			name:		"outcomeSummaryTable"
			label:		qsTr("Outcome Summary Table")
		}*/


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

		DropDown
		{
			name:				"analyzeAs"
			label:				qsTr("Analyze as")
			visible:			analysisType === "classicalDichotomous" || analysisType === "bayesianDichotomous"
			values:
			[
				{label: qsTr("Log(odds ratios)"),			value: "logOr"},
				{label: qsTr("Log(Peto's odds ratios)"),	value: "logPor"},
				{label: qsTr("Log(risk ratios)"),			value: "logRr"},
				{label: qsTr("Risk differences"),			value: "Rd"	}
			]
		}

		ClassicalMetaAnalysisMethod
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

		Group
		{
			title:		qsTr("Options")
			enabled:	plotEffectSizes.checked || plotSampleSizes.checked

			CheckBox
			{
				name:		"distPlotDensity"
				label:		qsTr("Add density")
			}

			CheckBox
			{
				name:		"distPlotRug"
				label:		qsTr("Add rug marks")
			}

			DropDown
			{
				name:		"binWidthType"
				label:		qsTr("Bin width type")
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
				enabled:		binWidthType.currentValue === "manual"
			}
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
				Layout.preferredWidth:	50 * preferencesModel.uiScale
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
						name: 				"studyLabel"
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
						id:					effectSize
						value:				""
						fieldWidth: 		40 * preferencesModel.uiScale
						useExternalBorder:	false
						showBorder: 		true
						enabled:			x1.value === "" && n1.value === "" && x2.value === "" && n2.value === ""
					}
				}

				Row
				{
					spacing:				 4 * preferencesModel.uiScale
					Layout.preferredWidth:	if (analysisType === "classicalContinuous" || analysisType === "bayesianContinuous") {100 * preferencesModel.uiScale} else {75 * preferencesModel.uiScale}
					TextField
					{
						name:				"effectSizeSe"
						id:					effectSE
						value:				""
						fieldWidth: 		40 * preferencesModel.uiScale
						useExternalBorder:	false
						showBorder: 		true
						enabled:			x1.value === "" && n1.value === "" && x2.value === "" && n2.value === "" && lCI.value === "" && uCI.value === ""
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
						enabled:			effectSE.value === ""
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
						enabled:			effectSE.value === ""
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
						enabled:			effectSize.value === "" && effectSE.value === ""
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
						enabled:			effectSize.value === "" && effectSE.value === ""
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
						enabled:			effectSize.value === "" && effectSE.value === ""
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
						enabled:			effectSize.value === "" && effectSE.value === ""
					}
				}
			}
		}
	}

	FileSelector
	{
		Layout.columnSpan:	2
		label: 				qsTr("Export the data set")
		name:				"savePath"
		filter:				"*.csv"
		save:				true
	}
}
