# Mendelian randomization of dyslipidemia on cognitive impairment among older Americans

## Citation Information
Fu M, Bakulski KM, Higgins C, Ware EB. Mendelian randomization of dyslipidemia on cognitive impairment among older Americans. medRxiv. Published online October 21, 2020:2020.10.20.20216036. doi:10.1101/2020.10.20.20216036

## This github repository contains the data management and analytic scripts to produce the following manuscript

## Abstract
**Background**: Altered lipid metabolism may be a risk factor for dementia, and blood cholesterol level has a strong genetic component. We tested the hypothesis that dyslipidemia (either low levels of high-density lipoprotein cholesterol (HDL-C) or high total cholesterol) is associated with cognitive status and domains, and assessed causality using genetic predisposition to dyslipidemia as an instrumental variable. **Methods**: Using data from European and African genetic ancestry participants in the Health and Retirement Study, we selected observations at the first non-missing biomarker assessment (waves 2006-2012). Cognition domains were assessed using episodic memory, mental status, and vocabulary tests. Overall cognitive status was categorized in three levels (normal, cognitive impairment non-dementia, dementia). Based on 2018 clinical guidelines, we compared low HDL-C or high total cholesterol to normal levels. Polygenic scores for dyslipidemia were used as instrumental variables in a Mendelian randomization framework. Multivariable logistic regressions and Wald-type ratio estimators were used to examine associations. **Results**: Among European ancestry participants (n = 8,781), at risk HDL-C levels were associated with higher odds of cognitive impairment (OR = 1.20, 95% CI: 1.03, 1.40) and worse episodic memory, specifically. Using cumulative genetic risk for HDL-C levels as a valid instrumental variable, a significant causal estimate was observed between at risk low HDL-C levels and higher odds of dementia (OR = 2.15, 95% CI: 1.16, 3.99). No significant associations were observed between total cholesterol levels and cognitive status. No significant associations were observed in the African ancestry sample (n = 2,101). **Conclusion**: Our study demonstrates low blood HDL-C is a potential causal risk factor for impaired cognition during aging in non-Hispanic whites of European ancestry. Dyslipidemia can be modified by changing diets, health behaviors, and therapeutic strategies, which can improve cognitive aging. Studies on low density lipoprotein cholesterol, the timing of cholesterol effects on cognition, and larger studies in non-European ancestries are needed.

## Script File
Data_cleaning.Rmd: Manipulating variables and preparing analytic sample

Main_analysis.Rmd: Building descriptive table and doing the main analysis

Sensitivity_analysis.Rmd: Code for sensitivity analysis

forest_afric.R: Creating forest plot in African genetic ancestry participants

forest_europ.R: Creating forest plot in European genetic ancestry participants

spline_plot.R: Making spline line chart





