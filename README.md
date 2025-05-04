# Effects of environmental change on population growth: monitoring time-varying carrying capacity in free-ranging spotted hyenas

This repository includes code and plots associated with the paper 'Effects of environmental change on population growth' (available in pre-print https://doi.org/10.1101/2024.04.11.589105).
This code relies on two separate repositories: [SHIM](https://github.com/hyenaproject/SHIM) and [hyenaR](https://github.com/hyenaproject/hyenaR) for working with data from the Ngorongoro Hyena Project.
These packages can be installed from source (see SETUP below).

## SETUP

Install the R packages `SHIM` and `hyenaR` from source using available `tar.gz` files provided in this repository.
To install these packages from source, use the following code in R:

```
## Install SHIM
install.packages("SHIM_0.5.22.tar.gz", repos = NULL, type="source")

## Install hyenaR
install.packages("hyenaR_0.10.0.tar.gz", repos = NULL, type="source")
```

Once SHIM and hyenaR are installed, the analysis for the paper can be recreated. Below is a description of each step of the process contained in different folders in this repository.

## STEP0_prepare_data (optional)

Generate all data needed for analysis. This step is optional as its code requires access to the Ngorongoro Hyena Project database, which is not publicly available.
The final output of all these data extraction tasks is available on Zenodo (https://zenodo.org/doi/10.5281/zenodo.10955614).
To skip this step 0 and directly proceed to step 1, the data hosted on Zenodo should be downloaded and placed into a folder `data` at the root of this repository.

The folder `STEP0_prepare_data` includes:

- `01_fit_VR_models.Rmd` 
    - Extract data and fit VR models (comparing predictive accuracy).
    Output saved as `data/model_list.RDS`.
- `02_starting_population.Rmd` 
    - Generate a snapshot of spotted hyena population at time 0. Output saved as `data/starting_data.RDS`. A `.RDS` file is needed (rather than e.g. `.csv`) because we use nested (list) columns for selections.
- `03_pattern_oriented_modelling.Rmd`
    - Use models generated in step 01. Run a single simulation covering an identical period to the real observed data.
    Compare emergent properties to understand how well our simulation is able to recreate the Ngorongoro Crater population.
    Outputs save in folder `data/POM` and `plots/POM`
- `04_demographic_data.Rmd`
    - Extract demographic data, number of juveniles, adult males, and adult females, and total population over time.
- `05_lambdaN_data.Rmd`
    - Extract data on lambda population size, required in later steps to estimate K using traditional Ricker and Beverton Holt models.
    Output saved as `data/supp_data1_alternativeK_data.csv`.
- `06_mechanistic_data.Rmd`
    - Extract environmental data required by later steps to run mechanistic models, including data on lions, prey abundance, and disease.
    Output saved as `data/mechanistic_model_data.csv`.

These data are used throughout further analyses.

## STEP1_estimateK

Code used to estimate time varying carrying capacity of spotted hyenas in Ngorongoro Crater using the Spotted Hyena Individual-based Model (SHIM).
These are the key results of the publication and are used for most further analysis and plotting. There are two key files:

- `run_simulation.R`:
  - Run simulations used to estimate Kt for each eyar. Outputs saved as `.txt` files in folders for each year.
  - Run simulations used to estimate stable (non-time varying) carrying capacity that uses marginal predictions within the simulation.
  Outputs saved as `.txt` files in folder 'marginal'
- `run_simulation_elasticity_oddsratio.R`
  - Run simulations to estimate elasticity of time varying carrying capacity estimates to variation in individual vital rates. Outputs saved as
  `.txt` files in folder 'elasticity_oddsratio'.
- `estimateK.Rmd`:
  - Estimate Kt from simulation outputs, using burn-ins to only incorporate simulations once at equilibrium.
  Output saved as `./data/Kplot_data`.

## STEP2_analysis_and_figures

Use estimated carrying capacity values and raw observational data from Ngorongoro Hyena Project to generate figures and further analyses. Folder includes:

- `K_theory_plot.R`
    - Generate a plot showing the theory behind time-varying carrying capacity.
- `demographic_plots.R` 
  - Generate a plot showing observed change in population size, sex and age ratio of the population over time.
- `NK_v_time.R`
  - Generate a plot showing observed change in time varying carrying capacity and population size over time.
- `temporal_trends.Rmd`
  - Estimate rate of change over time in Kt.
- `fig4_dN_dK.Rmd`
  - Generate plots showing the relationship between annual changes in carrying capacity and population size.
- `fig5_VR_trends.R`
  - Generate a plot showing trends in all observed vital rates within the population over time.
- `VR_trends.Rmd`
  - Estimate rate of change in vital rates over time.
- `fig6_mechanistic_analysis.Rmd`
  - Estimate the relationship between time varying carrying capacity and environmental variables (e.g. prey abundance, disease). Generate plots to show this relationship.

## STEP3_supplementary_analysis

All other plots and analyses not included in the main text.

- `SX_alternative_K_methods`
  - Comparison of estimated carrying capacity using individual based modelling compared to classical methods (Ricker and Beverton-Holt models).
- `SX_misc_analysis`
  - Correlation between K (population) and K (clan) (`corr_Kt_Ktc.R`).
  - Visualisation of density dependence as an emergent property of simulated vital rates (`est_dd.R`).
- `SX_model_tests`
- `SX_elasticity`
  - Analyse results of elasticity analysis with simulations.
