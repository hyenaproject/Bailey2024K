## EXTRACT DEMOGRAPHIC DATA:

## Prepare workspace
library(hyenaR)
library(tidyr)
library(dplyr)
library(SHIM)
library(here)

#This data is up until 2024
## (so that means that individual deaths should be detectable in 2022)
db.path     <- "../hyena_data/Fisidata_20_04_2024.sqlite"
start_year  <- 1997
end_year    <- 2022

## Number of juveniles and adults (male and female) at both a monthly and annual time scale.
## This is used to plot population growth and sex/age ratios
##
## Calculate monthly output
if (file.exists(here::here("./data/Nplot_data_separate_1month.RDS"))) {
  real_pop_separate <- readRDS(here::here("./data/Nplot_data_separate_1month.RDS"))
} else {
  ## EXTRACT REAL POP DATA OVER TIME
  hyenaR::load_package_database.full(db.path)

  ## We include all data with at least 1y observations afterwards. Exclude Jan/Feb 2022 just to make it easier to describe!
  real_pop_separate <- dplyr::tibble(date = seq(as.Date("1996-05-01"), as.Date("2023-01-01"), by = "1 month")) %>%
    dplyr::mutate(young = hyenaR::fetch_pop_number(main.clans = TRUE, from = .data$date, to = lead(.data$date),
                                                   CPUcores = 20, .parallel.min = 50,
                                                   lifestage = "!adult"),
                  ad_male = hyenaR::fetch_pop_number(main.clans = TRUE, from = .data$date, to = lead(.data$date),
                                                     CPUcores = 20, .parallel.min = 50,
                                                     lifestage = "adult", sex = "male"),
                  ad_fem = hyenaR::fetch_pop_number(main.clans = TRUE, from = .data$date, to = lead(.data$date),
                                                    CPUcores = 20, .parallel.min = 50,
                                                    lifestage = "adult", sex = "female")
    )

  saveRDS(real_pop_separate, file = here::here("./data/Nplot_data_separate_1month.RDS"))
}

## Calculate annual
if (file.exists(here::here("./data/Nplot_data_separate_year.RDS"))) {
  real_pop_separate <- readRDS(here::here("./data/Nplot_data_separate_year.RDS"))
} else {
  ## EXTRACT REAL POP DATA OVER TIME
  hyenaR::load_package_database.full(db.path)

  ## We include all data with at least 1y observations afterwards. Exclude Jan/Feb 2022 just to make it easier to describe!
  real_pop_separate <- dplyr::tibble(date = seq(as.Date("1996-01-01"), as.Date("2023-01-01"), by = "1 year")) %>%
    dplyr::mutate(young = hyenaR::fetch_pop_number(main.clans = TRUE, from = .data$date, to = lead(.data$date),
                                                   CPUcores = 20, .parallel.min = 50,
                                                   lifestage = "!adult"),
                  ad_male = hyenaR::fetch_pop_number(main.clans = TRUE, from = .data$date, to = lead(.data$date),
                                                     CPUcores = 20, .parallel.min = 50,
                                                     lifestage = "adult", sex = "male"),
                  ad_fem = hyenaR::fetch_pop_number(main.clans = TRUE, from = .data$date, to = lead(.data$date),
                                                    CPUcores = 20, .parallel.min = 50,
                                                    lifestage = "adult", sex = "female")
    )

  saveRDS(real_pop_separate, file = here::here("./data/Nplot_data_separate_year.RDS"))
}

## Population abundance over time (Nt)
if (!file.exists(here::here("./data/Nplot_data_month.RDS"))) {
  ## EXTRACT REAL POP DATA OVER TIME
  hyenaR::load_package_database.full(db.path)

  real_pop <- dplyr::tibble(date = seq(as.Date("1996-05-01"), as.Date("2023-01-01"), by = "1 month")) %>%
    dplyr::mutate(pop_size = hyenaR::fetch_pop_number(main.clans = TRUE, at = .data$date,
                                                      CPUcores = 75, .parallel.min = 285))
  saveRDS(real_pop, here::here("./data/Nplot_data_month.RDS"))
} else {
  real_pop <- readRDS(here::here("./data/Nplot_data_month.RDS"))
}

if (!file.exists(here::here("./data/Nplot_data_year.RDS"))) {
  ## EXTRACT REAL POP DATA OVER TIME
  hyenaR::load_package_database.full(db.path)

  real_pop <- dplyr::tibble(date = seq(as.Date("1996-01-01"), as.Date("2023-01-01"), by = "1 year")) %>%
    dplyr::mutate(from = date, to = lead(date)) |>
    dplyr::filter(!is.na(to))

  ## In first year, just consider the
  real_pop$from[1] <- "1996-05-01"

  real_pop <- real_pop |>
    dplyr::mutate(pop_size = hyenaR::fetch_pop_number(main.clans = TRUE, from = .data$from, to = .data$to,
                                                      CPUcores = 75, .parallel.min = 285),
                  adult_pop_size = hyenaR::fetch_pop_number(main.clans = TRUE, from = .data$from, to = .data$to, lifestage = "adult",
                                                            CPUcores = 75, .parallel.min = 285)) |>
    ## Create new col that can be used for plotting (which is 1st July in the middle of the year)
    dplyr::mutate(date = as.Date(paste0(lubridate::year(date), "-07-01")))
  saveRDS(real_pop, here::here("./data/Nplot_data_year.RDS"))
} else {
  real_pop <- readRDS(here::here("./data/Nplot_data_year.RDS"))
}
