### IF WE WANT TO USE GELMAN-RUBIN STAT AS OUR ASSESSMENT OF CONVERGENCE
### THEN WE SHOULD RUN SIMULATIONS STARTING IN MULTIPLE LOCATIONS
### CURRENTLY WE CAN ONLY PROVIDE A STATIC VALUE AS START POP
### WE NEED TO WRITE CUSTOM CODE FOR THIS TO WORK. IF IT'S WORTHWHILE
### WE CAN FORMALIZE THIS

#PREPARE PACKAGES AND LOAD DATA ####
options(future.rng.onMisuse="ignore", scipen = 200)

## Currently allows for more flexible rank fn (e.g. identify natals to take just natal rank)
# remotes::install_github(repo = "hyenaproject/SHIM", force = TRUE, ref = "v0.5.21")

#Load required libraries
library(SHIM)
library(dplyr)
library(hyenaR)
library(ggplot2)
library(spaMM)
library(here)

## GENERATED IN STEP0_prepare_data/model_fit.Rmd
model_data <- readRDS(here::here("./data/model_data.RDS"))

## Set a starting year. Filter out everything before this
start_yr <- 1997 # We ignore 1996 because majority are left censored individuals
end_yr   <- 2022 # Technically we can use data from early 2022, but not enough samples for year RE

##FIT MODELS ####
## Fem survival
F_surv_data <- model_data$F_surv_data |>
  dplyr::filter(year >= start_yr & year <= end_yr)

system.time({F_surv_mixmod <- spaMM::fitme(surv ~ poly(age, 2)*rank_category2 +
                                             clan_size +
                                             clan_size:rank_category2 + start_clan + (1|start_clan:year),
                                           data = F_surv_data,
                                           family = binomial(link = "logit"), method = "PQL/L")})

### M predisp survival ####
PreM_surv_data <- model_data$PreM_surv_data |>
  dplyr::filter(year >= start_yr & year <= end_yr)
system.time({preMsurv_mixmod <- spaMM::fitme(surv ~ poly(age, 2)*rank_category2 +
                                               clan_size +
                                               clan_size:rank_category2 + start_clan + (1|start_clan:year),
                                             data = PreM_surv_data,
                                             family = binomial(link = "logit"), method = "PQL/L")})

### M postdisp survival ####
system.time({postMsurv_mixmod <- spaMM::fitme(surv ~ poly(age, 2)*post_dispersal_status +
                                                clan_size +
                                                start_clan + (1|start_clan:year),
                                              data = model_data$PostM_surv_data |> dplyr::filter(year >= start_yr & year <= end_yr),
                                              family = binomial(link = "logit"), method = "PQL/L")})

### Twin ####
F_twin_data <- model_data$F_twin_data |>
  dplyr::filter(year >= start_yr & year <= end_yr)
system.time({twin_mixmod <- spaMM::fitme(twin ~ poly(age, 2)*rank_category2 +
                                           after1y_effort_all +
                                           clan_size*rank_category2 +
                                           start_clan + (1|start_clan:year),
                                         data = F_twin_data,
                                         family = binomial(link = "logit"), method = "PQL/L")})

### All repro ####
F_repro_primi <- model_data$F_repro_primi |>
  dplyr::filter(year >= start_yr & year <= end_yr)
system.time({primirepro_mixmod <- spaMM::fitme(repro ~ log(months+0.01)*rank_category2 +
                                                 rank_category2*clan_size +
                                                 after1y_effort_all + start_clan + (1|start_clan:year),
                                               data = F_repro_primi,
                                               family = binomial(link = "logit"), method = "PQL/L")})

F_repro_nonprimi <- model_data$F_repro_nonprimi |>
  dplyr::filter(year >= start_yr & year <= end_yr)
system.time({nonprimirepro_mixmod <- spaMM::fitme(repro ~ log(months+0.01)*rank_category2 + log(months+0.01)*age +
                                                    clan_size*rank_category2 + after1y_effort_all + start_clan + (1|start_clan:year),
                                                  data = F_repro_nonprimi,
                                                  family = binomial(link = "logit"), method = "PQL/L")})

system.time({disp_mixmod <- spaMM::fitme(second_disp ~ age + is_philo,
                                         data = model_data$M_second_disp_data |> dplyr::filter(year >= start_yr & year <= end_yr),
                                         family = binomial(link = "logit"), method = "PQL/L")})

modlist <- list(allF          = F_surv_mixmod,
                #Postdisp males don't consider rank
                postdispM     = postMsurv_mixmod,
                predispM      = preMsurv_mixmod,
                twin          = twin_mixmod,
                #Male dispersal is the same in all cases
                disp          = disp_mixmod,
                primirepro    = primirepro_mixmod,
                nonprimirepro = nonprimirepro_mixmod)

## Save model list so it can be recalled in other places
saveRDS(modlist, here::here("./data/model_list.RDS"))

# Take the start pop 1996 and increase it to different sizes
## GENERATED IN STEP0_prepare_data/starting_population.R
start_pop <- read.csv(here::here("./data/starting_data.csv"))

#Run 10 iterations for each year with same starting values
system.time({v050_K <- simulation_iterate(start_pops = start_pop,
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
                                          save_dir = "./STEP1_estimate_K",
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
                                save_dir = "./STEP1_estimate_K/marginal",
                                save_size = 60,
                                iterator_seed = 123,
                                parallel = TRUE, CPUcores = 10, .parallel.min = 1)})
