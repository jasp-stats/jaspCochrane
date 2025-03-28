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
import QtQuick
import QtQuick.Layouts
import JASP.Controls
import JASP

Section
{
	title: qsTr("Statistics")
	property string module:	"metaAnalysis"

	Group
	{
		title: qsTr("Regression Coefficients")
		CheckBox
		{
			name: "coefficientEstimate";
			text: qsTr("Estimates");
			checked: true
			onClicked: { if (!checked && estimatesConfInt.checked) estimatesConfInt.click() }
			CheckBox
			{
				id: estimatesConfInt
				name: "coefficientCi"; text: qsTr("Confidence intervals")
				CIField { name: "coefficientCiLevel"; label: qsTr("Interval") }
			}
		}
		DropDown { name: "estimateTest"; label: qsTr("Test"); values: [ "z", "knha"]; }
		CheckBox { name: "covarianceMatrix"; text: qsTr("Covariance matrix") }

	}
	Group
	{
		title: qsTr("Model Fit")
		CheckBox { name: "fitMeasure";				text: qsTr("Fit measures") }
		CheckBox
		{
			id:			forestPlot
			name: 		"forestPlot"
			text: 		qsTr("Forest plot")
			
			CheckBox
			{
				name:		"forestPlotLabel"
				text:		qsTr("Show labels")
				checked:	true
				enabled: 	forestPlot.checked	
				visible:	module == "cochrane"
			}

			DropDown
			{
				name:			"forestPlotOrder"
				label:			qsTr("Ordering")
				enabled: 		forestPlot.checked
				visible:		module == "cochrane"
				currentIndex:	1
				values: [
					{ label: qsTr("Year (ascending)")			, value: "yearAscending"			},
					{ label: qsTr("Year (descending)")			, value: "yearDescending"			},
					{ label: qsTr("Effect size (ascending)")	, value: "effectSizeAscending"		},
					{ label: qsTr("Effect size (descending)")	, value: "effectSizeDescending"		}
				]
			}
		
		}
		CheckBox { name: "funnelPlot";				text: qsTr("Funnel plot") }
		CheckBox { name: "funnelPlotRankTestAsymmetry";			text: qsTr("Rank test for funnel plot asymmetry") }
		CheckBox { name: "funnelPlotRegressionTestAsymmetry"; text: qsTr("Regression test for funnel plot asymmetry") }
	}

	Group
	{
		title: qsTr("Residuals Model")
		CheckBox { name: "residualParameter"; text: qsTr("Residuals parameters"); checked: true;}
	}
}
