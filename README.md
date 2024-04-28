# Effects of environmental change on population growth: monitoring time-varying carrying capacity in free-ranging spotted hyenas

This repository includes code and plots for associated with the paper Bailey et al. 2024 currently in pre-print (DOI XXX). This code relies on two separate repositories: [SHIM](https://github.com/hyenaproject/SHIM) and [hyenaR](https://github.com/hyenaproject/hyenaR) for working with data from the Ngorongoro Hyena Project.

Below is a description of the different folders.

## STEP0_prepare_data

Generate all data needed for analysis. This code requires access to the Ngorongoro Hyena Project database, which is not publicly available. The final output of all these data extraction tasks is available on Zenodo (https://doi.org/10.5281/zenodo.10955615). These data should be downloaded into folder 'data' for the following scripts.

This folder includes:
- starting_population.R - Generate a snapshot of spotted hyena population at time 0. Output saved as data/starting_data.RDS.
.RDS file needed because we have nested (list) columns for selections.
- model_fit.Rmd - Extract data used to fit all models. Output saved as data/model_data.RDS. Test model fitting with different link functions and exponents.
- mechanistic_data.Rmd - Extract environmental data to run mechanistic models, including data on lions, prey abundance, and disease. Output saved as 'data/mechanistic_model_data.csv'.
- lambdaN_data.Rmd - Extract data on lambda population size, used to estimate K using traditional Ricker and Beverton Holt models. Output saved as 'data/supp_data1_alternativeK_data.csv'
- demographic_data.R - Extract demographic data, number of juveniles, adult males, and adult females, and total population over time. Outputs are saved as 'data/Nplot_data_separate_1month.RDS' (counts separated by age and sex at monthly resolution), 'data/Nplot_data_separate_year.RDS' (counts separated by age and sex at yearly resolution), 'data/Nplot_data_month.RDS' (total population count at monthly resolution), 'data/Nplot_data_year.RDS' (total population count at yearly resolution).

These data are used throughout further analyses

## STEP1_estimate_K

Code used to estimate time varying carrying capacity of spotted hyenas in Ngorongoro Crater using the Spotted Hyena Individual-based Model (SHIM). We estimate carrying capacity in 3 scenarios:
- Time varying carrying capacity for each year.
- Stable (non-time varying) carrying capacity that uses marginal predictions within the simulation.
- Elasticity of time varying carrying capacity to individual vital rates.

The outputs of all these simulations are stored as .txt files within the sub-folders.

## STEP2_analysis_and_figures

Use estimated carrying capacity values and raw observational data from Ngorongoro Hyena Project to generate figures and further analyses. Folder includes:
- fig1_theory_plot.R - Generate a plot showing the theory behind time-varying carrying capacity
- fig2_N_and_sexratio.R - Generate a plot showing observed change in populaton size, sex and age ratio of the population over time.
- fig3_NK_v_time.R - Generate a plot showing observed change in time varying carrying capacity and population size over time.
- temporal_trends.Rmd - Estimate rate of change over time in Kt.
- fig4_dN_dK.Rmd - Generate plots showing the relationship between annual changes in carrying capacity and population size.
- fig5_VR_trends.R - Generate a plot showing trends in all observed vital rates within the population over time.
- VR_trends.Rmd - Estimate rate of change in vital rates over time.
- fig6_mechanistic_analysis.Rmd - Estiamte the relationship between time varying carrying capacity and environmental variables (e.g. prey abundance, disease). Generate plots to show this relationship.

## STEP3_supplementary_analysis

All other plots and analyses not included in the main text.

- SX_alternative_K_methods: Comparison of estimated carrying capacity using individual based modelling compared to classical methods (Ricker and Beverton-Holt models).
- SX_misc_analysis:
  - Correlation between K (population) and K (clan) (corr_Kt_Ktc.R)
  - Visualisation of density dependence as an emergent property of simulated vital rates (est_dd.R)
- SX_model_tests:
- SX_pattern_oriented_modelling: Compare patterns of individual-based model with real world data from Ngorongoro Hyena Project.
- SX_elasticity: Analyse results of elasticity analysis with simulations.
