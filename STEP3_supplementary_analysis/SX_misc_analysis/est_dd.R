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

## GENERATED IN STEP0_prepare_data/model_fit.Rmd
model_data <- readRDS(here::here("./data/model_data.RDS"))

## GENERATED IN STEP1_estimate_K/estimateK.R
modlist <- readRDS(here::here("./data/model_list.RDS"))

## GENERATED IN STEP0_prepare_data/starting_population.R
start_pop_example <- readRDS(here::here("./data/starting_data.RDS"))

# Take subset of startpop in 1996 to start at smaller N
start_pop <- start_pop_example |>
  mutate(juv = age < 24) |>
  group_by(current_clan, sex, juv) |>
  slice(sample(1:n(), size = n()/4))

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
                                number_steps = 600,
                                step_size = 1, models = modlist,
                                save_dir = "./STEP3_supplementary_analysis/SX_misc_analysis/plot_simulated_dd",
                                save_size = 60,
                                iterator_seed = 123,
                                parallel = TRUE, CPUcores = 10, .parallel.min = 1)})


### EXTRACT MARGINAL N WHERE ENV IS FIXED AND WE CAN SEE DD RELATIONSHIP EXPLICITLY

## Marginal K (i.e. not time-varying)
folder <- here::here("./STEP3_supplementary_analysis/SX_misc_analysis/plot_simulated_dd")
all_files <- list.files(folder, pattern = ".txt", full.names = TRUE)

pb_yr <- progress::progress_bar$new(total = length(all_files))

allsims <- purrr::map_df(.x = all_files,
                         .f = function(filepath){

                           pb_yr$tick()

                           basename <- base::basename(filepath)

                           readr::read_delim(filepath, show_col_types = FALSE, lazy = FALSE) %>%
                             dplyr::mutate(sim = stringr::str_extract(basename, "(?<=_)[0-9]+(?=.txt)"))

                         })

estN <- allsims %>%
  dplyr::group_by(.data$sim) %>%
  dplyr::slice(1:ceiling(dplyr::n()/1)) %>%
  ## Thin out to be every 3mo
  dplyr::slice(seq(1, dplyr::n(), by = 1)) |>
  dplyr::select(date, pop_size, sim) |>
  dplyr::group_by(sim) |>
  dplyr::mutate(Nt = pop_size,
                Nt1 = lead(pop_size),
                deltaN = Nt1-Nt,
                lambdaN = Nt1/Nt) |>
  dplyr::filter(!is.na(deltaN))

library(ggplot2)
ggplot(data = estN) +
  geom_point(aes(x = Nt, y = lambdaN), alpha = 0.25) +
  geom_hline(yintercept = 1, lty = 2, linewidth = 0.75) +
    geom_smooth(aes(x = Nt, y = lambdaN)) +
    facet_wrap(facets = ~sim) +
      theme_classic()

ggplot(data = estN) +
  geom_point(aes(x = Nt, y = lambdaN), alpha = 0.055) +
  geom_hline(yintercept = 1, lty = 2, linewidth = 0.75) +
  geom_smooth(aes(x = Nt, y = lambdaN), se = TRUE) +
  scale_x_continuous(limits = c(0, NA),
                     breaks = seq(0, 600, 50)) +
  theme_classic()

ggsave(filename = "./plots/supp_Nt_lambdaN.png")

ggplot(data = estN) +
  geom_point(aes(x = Nt, y = deltaN), alpha = 0.25) +
  geom_hline(yintercept = 0, lty = 2, linewidth = 0.75) +
  geom_smooth(aes(x = Nt, y = deltaN)) +
  facet_wrap(facets = ~sim) +
  theme_classic()

ggplot(data = estN) +
  geom_point(aes(x = Nt, y = deltaN), alpha = 0.05) +
  geom_hline(yintercept = 0, lty = 2, linewidth = 0.75) +
  geom_smooth(aes(x = Nt, y = deltaN), se = TRUE) +
  scale_x_continuous(limits = c(0, NA),
                     breaks = seq(0, 600, 50)) +
  theme_classic()

ggsave(filename = "./plots/supp_Nt_deltaN.png")
