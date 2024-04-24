### IF WE WANT TO USE GELMAN-RUBIN STAT AS OUR ASSESSMENT OF CONVERGENCE
### THEN WE SHOULD RUN SIMULATIONS STARTING IN MULTIPLE LOCATIONS
### CURRENTLY WE CAN ONLY PROVIDE A STATIC VALUE AS START POP
### WE NEED TO WRITE CUSTOM CODE FOR THIS TO WORK. IF IT'S WORTHWHILE
### WE CAN FORMALIZE THIS

#PREPARE PACKAGES AND LOAD DATA ####
options(future.rng.onMisuse="ignore", scipen = 200)

#Load required libraries
library(SHIM)
library(dplyr)
library(hyenaR)
library(ggplot2)
library(spaMM)
library(here)

start_yr <- 1997
end_yr <- 2022

## GENERATED IN STEP0_prepare_data/model_fit.Rmd
model_data <- readRDS(here::here("./data/model_data.RDS"))

## GENERATED IN STEP1_estimate_K/estimateK.R
modlist <- readRDS(here::here("./data/model_list.RDS"))

## Update all models here to avoid env issues
modlist$allF <- update(modlist$allF, data = model_data$F_surv_data)
modlist$postdispM <- update(modlist$postdispM, data = model_data$PostM_surv_data)
modlist$predispM <- update(modlist$predispM, data = model_data$PreM_surv_data)
modlist$twin <- update(modlist$twin, data = model_data$F_twin_data)
modlist$disp <- update(modlist$disp, data = model_data$M_second_disp_data)
modlist$primirepro <- update(modlist$primirepro, data = model_data$F_repro_primi)
modlist$nonprimirepro <- update(modlist$nonprimirepro, data = model_data$F_repro_nonprimi)

## Choose individuals born in 2010
## Cohort of individuals with lots of information

### CREATE FN TO REFIT MODEL WITH CHANGED INTERCEPT ####
refit_mod <- function(mod, x){

  oldfixef <- spaMM::fixef(mod)
  oldfixef["(Intercept)"] <- oldfixef["(Intercept)"] + x
  stats::update(mod, etaFix = list(beta = oldfixef),
                fixed = list(lambda = mod$lambda))

}

### CREATE FN TO RETURN ODDS RATIO AFTER CHANGING INTERCEPT ####

find_odds_ratio <- function(mod, newdata, x){

  new_mod <- refit_mod(mod, x = x)

  new_prob <- mean(predict(new_mod, newdata = newdata, type = "response", verbose = c("showpbar" = FALSE)))
  old_prob <- mean(predict(mod, newdata = newdata, type = "response", verbose = c("showpbar" = FALSE)))

  new_odds <- new_prob/(1-new_prob)
  old_odds <- old_prob/(1-old_prob)

  ## ALWAYS DO ODDS RATIO AS LARGEST/SMALLEST
  if (x > 0) {
    odds_ratio <- new_odds/old_odds
  } else {
    odds_ratio <- old_odds/new_odds
  }

  return(odds_ratio)

}

### FUNCTION TO FEED INTO OPTIM THAT FINDS DIFFERENCE B/W ODDS RATIO AND GOAL
### direction: SHOULD BE 1 OR -1 TO DETERMINE WHETHER FUNCTION WILL ATTEMPT TO REDUCE OR INCREASE INTERCEPT
### goal: IS THE ODDS RATIO WE ARE TRYING TO REACH
optim_odds_ratio <- function(par, direction = 1, goal = 1.25, mod, newdata) {

  ## We optimise larger/smaller odds ratio, so goal must be >1
  if (goal <= 1) {
    stop("goal must be > 1")
  }

  x <- par * direction

  distance <- abs(goal - find_odds_ratio(mod, newdata, x))

  return(distance)

}

### OPTIMISE UPPER AND LOWER INTERCEPT
find_new_intercepts <- function(mod, newdata, goal = 1.25, ...) {

  message("Optimise upper intercept")
  upper_x <- optim(par = c(0.25), fn = optim_odds_ratio, gr = NULL,
                   mod = mod, newdata = newdata, direction = 1,
                   method = "Brent", lower = 0, upper = 2, ...)
  upper_x_mod <- refit_mod(mod, x = upper_x$par)

  message("Optimise lower intercept")
  lower_x <- optim(par = c(0.25), fn = optim_odds_ratio, gr = NULL,
                   mod = mod, newdata = newdata, direction = -1,
                   method = "Brent", lower = 0, upper = 2, ...)
  lower_x_mod <- refit_mod(mod, x = -1*lower_x$par)

  tibble(mod_name = c("Decrease", "Increased", "Original"),
         mod = list(lower_x_mod, upper_x_mod, mod),
         x = c(-1*lower_x$par, upper_x$par, 0),
         goal = goal)

}

### FEMALE SURVIVAL ####

## Shift intercept of model and see how it affects predicted outcome
F_changes <- find_new_intercepts(mod = modlist$allF,
                                 newdata = model_data$F_surv_data |>
                                   dplyr::filter(year >= start_yr & year <= end_yr), goal = 1.25) |>
  mutate(VR = "femsurv")

#### REDUCE SURV
elasticity_modlist <- list(allF          = F_changes$mod[[1]],
                           #Postdisp males don't consider rank
                           postdispM     = modlist$postdispM,
                           predispM      = modlist$predispM,
                           twin          = modlist$twin,
                           #Male dispersal is the same in all cases
                           disp          = modlist$disp,
                           primirepro    = modlist$primirepro,
                           nonprimirepro = modlist$nonprimirepro)

simulation_iterate(start_pops = start_pop_example,
                   return = FALSE,
                   sim_years = start_yr:end_yr, i = 3,
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
                   step_size = 1, models = elasticity_modlist,
                   save_dir = "./STEP1_estimate_K/elasticity_oddsratio/femsurv/lower",
                   save_size = 60,
                   iterator_seed = 123,
                   parallel = TRUE, CPUcores = 48, .parallel.min = 1)

#### INCREASE SURV
elasticity_modlist <- list(allF          = F_changes$mod[[2]],
                           #Postdisp males don't consider rank
                           postdispM     = modlist$postdispM,
                           predispM      = modlist$predispM,
                           twin          = modlist$twin,
                           #Male dispersal is the same in all cases
                           disp          = modlist$disp,
                           primirepro    = modlist$primirepro,
                           nonprimirepro = modlist$nonprimirepro)

simulation_iterate(start_pops = start_pop_example,
                   return = FALSE,
                   sim_years = start_yr:end_yr, i = 3,
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
                   step_size = 1, models = elasticity_modlist,
                   save_dir = "./STEP1_estimate_K/elasticity_oddsratio/femsurv/higher",
                   save_size = 60,
                   iterator_seed = 123,
                   parallel = TRUE, CPUcores = 48, .parallel.min = 1)

### M Predisp SURVIVAL ####

## Shift intercept of model and see how it affects predicted outcome
preM_changes <- find_new_intercepts(mod = modlist$predispM,
                                    newdata = model_data$PreM_surv_data |>
                                      dplyr::filter(year >= start_yr & year <= end_yr),
                                    goal = 1.25) |>
  mutate(VR = "preM")

#### REDUCE SURV
elasticity_modlist <- list(allF          = modlist$allF,
                           #Postdisp males don't consider rank
                           postdispM     = modlist$postdispM,
                           predispM      = preM_changes$mod[[1]],
                           twin          = modlist$twin,
                           #Male dispersal is the same in all cases
                           disp          = modlist$disp,
                           primirepro    = modlist$primirepro,
                           nonprimirepro = modlist$nonprimirepro)

simulation_iterate(start_pops = start_pop_example,
                   return = FALSE,
                   sim_years = start_yr:end_yr, i = 3,
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
                   step_size = 1, models = elasticity_modlist,
                   save_dir = "./STEP1_estimate_K/elasticity_oddsratio/preM/lower",
                   save_size = 60,
                   iterator_seed = 123,
                   parallel = TRUE, CPUcores = 48, .parallel.min = 1)

#### INCREASE SURV
elasticity_modlist <- list(allF          = modlist$allF,
                           #Postdisp males don't consider rank
                           postdispM     = modlist$postdispM,
                           predispM      = preM_changes$mod[[2]],
                           twin          = modlist$twin,
                           #Male dispersal is the same in all cases
                           disp          = modlist$disp,
                           primirepro    = modlist$primirepro,
                           nonprimirepro = modlist$nonprimirepro)

simulation_iterate(start_pops = start_pop_example,
                   return = FALSE,
                   sim_years = start_yr:end_yr, i = 3,
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
                   step_size = 1, models = elasticity_modlist,
                   save_dir = "./STEP1_estimate_K/elasticity_oddsratio/preM/higher",
                   save_size = 60,
                   iterator_seed = 123,
                   parallel = TRUE, CPUcores = 48, .parallel.min = 1)

### M Postdisp SURVIVAL ####

## Shift intercept of model and see how it affects predicted outcome
postM_changes <- find_new_intercepts(mod = modlist$postdispM,
                                     newdata = model_data$PostM_surv_data |>
                                       dplyr::filter(year >= start_yr & year <= end_yr & !is.na(post_dispersal_status)),
                                     goal = 1.25) |>
  mutate(VR = "postM")

#### REDUCE SURV
elasticity_modlist <- list(allF          = modlist$allF,
                           #Postdisp males don't consider rank
                           postdispM     = postM_changes$mod[[1]],
                           predispM      = modlist$predispM,
                           twin          = modlist$twin,
                           #Male dispersal is the same in all cases
                           disp          = modlist$disp,
                           primirepro    = modlist$primirepro,
                           nonprimirepro = modlist$nonprimirepro)

simulation_iterate(start_pops = start_pop_example,
                   return = FALSE,
                   sim_years = start_yr:end_yr, i = 3,
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
                   step_size = 1, models = elasticity_modlist,
                   save_dir = "./STEP1_estimate_K/elasticity_oddsratio/postM/lower",
                   save_size = 60,
                   iterator_seed = 123,
                   parallel = TRUE, CPUcores = 48, .parallel.min = 1)

#### INCREASE SURV
elasticity_modlist <- list(allF          = modlist$allF,
                           #Postdisp males don't consider rank
                           postdispM     = postM_changes$mod[[2]],
                           predispM      = modlist$predispM,
                           twin          = modlist$twin,
                           #Male dispersal is the same in all cases
                           disp          = modlist$disp,
                           primirepro    = modlist$primirepro,
                           nonprimirepro = modlist$nonprimirepro)

simulation_iterate(start_pops = start_pop_example,
                   return = FALSE,
                   sim_years = start_yr:end_yr, i = 3,
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
                   step_size = 1, models = elasticity_modlist,
                   save_dir = "./STEP1_estimate_K/elasticity_oddsratio/postM/higher",
                   save_size = 60,
                   iterator_seed = 123,
                   parallel = TRUE, CPUcores = 48, .parallel.min = 1)



### Twin ####

## Shift intercept of model and see how it affects predicted outcome
twin_changes <- find_new_intercepts(mod = modlist$twin,
                                    newdata = model_data$F_twin_data |>
                                      dplyr::filter(year >= start_yr & year <= end_yr),
                                    goal = 1.25) |>
  mutate(VR = "twin")

#### REDUCE SURV
elasticity_modlist <- list(allF          = modlist$allF,
                           #Postdisp males don't consider rank
                           postdispM     = modlist$postdispM,
                           predispM      = modlist$predispM,
                           twin          = twin_changes$mod[[1]],
                           #Male dispersal is the same in all cases
                           disp          = modlist$disp,
                           primirepro    = modlist$primirepro,
                           nonprimirepro = modlist$nonprimirepro)

simulation_iterate(start_pops = start_pop_example,
                   return = FALSE,
                   sim_years = start_yr:end_yr, i = 3,
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
                   step_size = 1, models = elasticity_modlist,
                   save_dir = "./STEP1_estimate_K/elasticity_oddsratio/twin/lower",
                   save_size = 60,
                   iterator_seed = 123,
                   parallel = TRUE, CPUcores = 48, .parallel.min = 1)

#### INCREASE SURV
elasticity_modlist <- list(allF          = modlist$allF,
                           #Postdisp males don't consider rank
                           postdispM     = modlist$postdispM,
                           predispM      = modlist$predispM,
                           twin          = twin_changes$mod[[2]],
                           #Male dispersal is the same in all cases
                           disp          = modlist$disp,
                           primirepro    = modlist$primirepro,
                           nonprimirepro = modlist$nonprimirepro)

simulation_iterate(start_pops = start_pop_example,
                   return = FALSE,
                   sim_years = start_yr:end_yr, i = 3,
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
                   step_size = 1, models = elasticity_modlist,
                   save_dir = "./STEP1_estimate_K/elasticity_oddsratio/twin/higher",
                   save_size = 60,
                   iterator_seed = 123,
                   parallel = TRUE, CPUcores = 48, .parallel.min = 1)


### Primi ####

## Shift intercept of model and see how it affects predicted outcome
primi_changes <- find_new_intercepts(mod = modlist$primirepro,
                                     newdata = model_data$F_repro_primi  |>
                                       dplyr::filter(year >= start_yr & year <= end_yr),
                                     goal = 1.25) |>
  mutate(VR = "primi")

#### REDUCE SURV
elasticity_modlist <- list(allF          = modlist$allF,
                           #Postdisp males don't consider rank
                           postdispM     = modlist$postdispM,
                           predispM      = modlist$predispM,
                           twin          = modlist$twin,
                           #Male dispersal is the same in all cases
                           disp          = modlist$disp,
                           primirepro    = primi_changes$mod[[1]],
                           nonprimirepro = modlist$nonprimirepro)

simulation_iterate(start_pops = start_pop_example,
                   return = FALSE,
                   sim_years = start_yr:end_yr, i = 3,
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
                   step_size = 1, models = elasticity_modlist,
                   save_dir = "./STEP1_estimate_K/elasticity_oddsratio/primi/lower",
                   save_size = 60,
                   iterator_seed = 123,
                   parallel = TRUE, CPUcores = 48, .parallel.min = 1)

#### INCREASE SURV
elasticity_modlist <- list(allF          = modlist$allF,
                           #Postdisp males don't consider rank
                           postdispM     = modlist$postdispM,
                           predispM      = modlist$predispM,
                           twin          = modlist$twin,
                           #Male dispersal is the same in all cases
                           disp          = modlist$disp,
                           primirepro    = primi_changes$mod[[2]],
                           nonprimirepro = modlist$nonprimirepro)

simulation_iterate(start_pops = start_pop_example,
                   return = FALSE,
                   sim_years = start_yr:end_yr, i = 3,
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
                   step_size = 1, models = elasticity_modlist,
                   save_dir = "./STEP1_estimate_K/elasticity_oddsratio/primi/higher",
                   save_size = 60,
                   iterator_seed = 123,
                   parallel = TRUE, CPUcores = 48, .parallel.min = 1)

### Non-Primi ####

## Shift intercept of model and see how it affects predicted outcome
nonprimi_changes <- find_new_intercepts(mod = modlist$nonprimirepro,
                                        newdata = model_data$F_repro_nonprimi |>
                                          dplyr::filter(year >= start_yr & year <= end_yr),
                                        goal = 1.25) |>
  mutate(VR = "nonprimi")

#### REDUCE SURV
elasticity_modlist <- list(allF          = modlist$allF,
                           #Postdisp males don't consider rank
                           postdispM     = modlist$postdispM,
                           predispM      = modlist$predispM,
                           twin          = modlist$twin,
                           #Male dispersal is the same in all cases
                           disp          = modlist$disp,
                           primirepro    = modlist$primirepro,
                           nonprimirepro = nonprimi_changes$mod[[1]])

simulation_iterate(start_pops = start_pop_example,
                   return = FALSE,
                   sim_years = start_yr:end_yr, i = 3,
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
                   step_size = 1, models = elasticity_modlist,
                   save_dir = "./STEP1_estimate_K/elasticity_oddsratio/nonprimi/lower",
                   save_size = 60,
                   iterator_seed = 123,
                   parallel = TRUE, CPUcores = 48, .parallel.min = 1)

#### INCREASE SURV
elasticity_modlist <- list(allF          = modlist$allF,
                           #Postdisp males don't consider rank
                           postdispM     = modlist$postdispM,
                           predispM      = modlist$predispM,
                           twin          = modlist$twin,
                           #Male dispersal is the same in all cases
                           disp          = modlist$disp,
                           primirepro    = modlist$primirepro,
                           nonprimirepro = nonprimi_changes$mod[[2]])

simulation_iterate(start_pops = start_pop_example,
                   return = FALSE,
                   sim_years = start_yr:end_yr, i = 3,
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
                   step_size = 1, models = elasticity_modlist,
                   save_dir = "./STEP1_estimate_K/elasticity_oddsratio/nonprimi/higher",
                   save_size = 60,
                   iterator_seed = 123,
                   parallel = TRUE, CPUcores = 48, .parallel.min = 1)
