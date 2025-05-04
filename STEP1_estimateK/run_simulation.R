### IF WE WANT TO USE GELMAN-RUBIN STAT AS OUR ASSESSMENT OF CONVERGENCE
### THEN WE SHOULD RUN SIMULATIONS STARTING IN MULTIPLE LOCATIONS
### CURRENTLY WE CAN ONLY PROVIDE A STATIC VALUE AS START POP
### WE NEED TO WRITE CUSTOM CODE FOR THIS TO WORK. IF IT'S WORTHWHILE
### WE CAN FORMALIZE THIS

#PREPARE PACKAGES AND LOAD DATA ####
options(future.rng.onMisuse = "ignore", scipen = 200)

## Currently allows for more flexible rank fn (e.g. identify natals to take just natal rank)
# remotes::install_github(repo = "hyenaproject/SHIM", force = TRUE, ref = "v0.5.21")

#Load required libraries
library(SHIM)
library(dplyr)
library(hyenaR)
library(ggplot2)
library(spaMM)
library(here)

## Set a starting year. Filter out everything before this
start_yr <- 1997 # We ignore 1996 because majority are left censored individuals
end_yr   <- 2023 # Technically we can use data from early 2022, but not enough samples for year RE

## GENERATED IN STEP0_prepare_data/01_fit_VR_models.Rmd
modlist <- readRDS(here::here("./data/model_list.RDS"))

# Take the start pop 1996 and increase it to different sizes
## GENERATED IN STEP0_prepare_data/starting_population.R
start_pop <- readRDS(here::here("./data/starting_data.RDS"))

#Run 10 iterations for each year with same starting values
system.time({db_20_04_2024_K <- simulation_iterate(start_pops = start_pop,
                                                   return = FALSE,
                                                   sim_years = start_yr:end_yr, i = 10,
                                                   predictors = list(start_clan = \(ID) ID$clan_name,
                                                                     post_dispersal_status = \(ID) if (ID$birth_date == ID$first_date) "philo" else "disp",
                                                                     ## If we want a more refined estimate of rank
                                                                     rank_category2 = \(ID){
                                                                       natals <- ID$clan_ID$inhabitants_tbl$ID[ID$clan_ID$inhabitants_tbl$natal]
                                                                       ## Will extract for all individuals (even disperser males) so can be NA
                                                                       if (!ID$ID %in% natals) return(NA_character_)
                                                                       std_rank <- seq(1, -1, length.out = length(natals))[which(natals == ID$ID)]
                                                                       if (std_rank >= 1/3) "top" else if (std_rank <= -1/3) "bottom" else "middle"}
                                                   ),
                                                   number_steps = 1200,
                                                   step_size = 1, models = modlist,
                                                   save_dir = "./STEP1_estimateK",
                                                   save_size = 60,
                                                   iterator_seed = 123,
                                                   parallel = TRUE, CPUcores = 48, .parallel.min = 1)})

### MARGINAL EFFECTS
###
### METHOD 1: Using re.form = NA
### This is incorrect because re.form = NA is not the same as the mean value of RE (due to non-linear link function)

#Run 10 iterations ignoring year (i.e. marginal effect)
system.time({simulation_iterate(start_pops = start_pop,
                                return = FALSE,
                                sim_years = 1900, include_ranef = FALSE,
                                i = 10,
                                predictors = list(start_clan = \(ID) ID$clan_name,
                                                  post_dispersal_status = \(ID) if (ID$birth_date == ID$first_date) "philo" else "disp",
                                                  ## If we want a more refined estimate of rank
                                                  rank_category2 = \(ID){
                                                    natals <- ID$clan_ID$inhabitants_tbl$ID[ID$clan_ID$inhabitants_tbl$natal]
                                                    ## Will extract for all individuals (even disperser males) so can be NA
                                                    if (!ID$ID %in% natals) return(NA_character_)
                                                    std_rank <- seq(1, -1, length.out = length(natals))[which(natals == ID$ID)]
                                                    if (std_rank >= 1/3) "top" else if (std_rank <= -1/3) "bottom" else "middle"}
                                ),
                                number_steps = 1200,
                                step_size = 1, models = modlist,
                                save_dir = "./STEP1_estimateK/marginal",
                                save_size = 60,
                                iterator_seed = 123,
                                parallel = TRUE, CPUcores = 10, .parallel.min = 1)})
