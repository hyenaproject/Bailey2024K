# Setup
## Load required packages

library(ggplot2)
library(hyenaR)
library(dplyr)
library(here)
library(patchwork)
library(ggtext)

## Define file/folder paths
start_year  <- 1997
end_year    <- 2022

scale <- "year"

# Extract data
## Time-varying carrying capacity (Kt)
if (!file.exists(here::here("./data/Kplot_data.RDS"))) {
  ## EXTRACT ESTIMATED K
  years <- start_year:end_year

  pb_yr <- progress::progress_bar$new(total = length(years))

  Kplot_data <- purrr::map_df(.x = years,
                              .f = function(year, folder_name){

                                pb_yr$tick()

                                txt_files <- list.files(here(paste0("./STEP1_estimate_K/", year)),
                                                        pattern = ".txt", full.names = TRUE)

                                allsims <- purrr::map_df(.x = txt_files,
                                                         .f = function(filepath){

                                                           readr::read_delim(filepath, show_col_types = FALSE, lazy = FALSE) %>%
                                                             dplyr::mutate(sim = stringr::str_extract(filepath, "(?<=_)[0-9]+(?=.txt)"))

                                                         })

                                print(year)

                                estK <- allsims %>%
                                  dplyr::group_by(.data$sim) %>%
                                  dplyr::mutate(start_pop = first(pop_size)) %>%
                                  dplyr::slice(ceiling(dplyr::n()/2):dplyr::n()) %>%
                                  dplyr::summarise(K = median(.data$pop_size),
                                                   start_size = first(start_pop))

                                plot_data <- estK %>%
                                  dplyr::summarise(year = year,
                                                   globalK = median(.data$K),
                                                   lower = min(.data$K),
                                                   upper = max(.data$K),
                                                   points = list(data.frame(pop_size = .data$K,
                                                                            start_pop = .data$start_size)))

                                return(plot_data)

                              })

  saveRDS(Kplot_data, here::here("./data/Kplot_data.RDS"))
} else {
  Kplot_data <- readRDS(here::here("./data/Kplot_data.RDS"))
}

## GENERATE SUMMARY STATS
range(Kplot_data$globalK)
sd(Kplot_data$globalK)
median(Kplot_data$globalK)
mean(Kplot_data$globalK)
nrow(Kplot_data)/sum(1/Kplot_data$globalK) ## Harmonic mean

### GENERATED IN STEP0_prepare_data/demographic_data.R
if (scale == "month") {
  real_pop <- readRDS(here::here("./data/Nplot_data_month.RDS"))
} else if (scale == "year") {
  real_pop <- readRDS(here::here("./data/Nplot_data_year.RDS"))
}

## Marginal K (i.e. not time-varying)
folder <- here::here("./STEP1_estimate_K/marginal")
all_files <- list.files(folder, pattern = ".txt", full.names = TRUE)

pb_yr <- progress::progress_bar$new(total = length(all_files))

allsims <- purrr::map_df(.x = all_files,
                         .f = function(filepath){

                           pb_yr$tick()

                           basename <- base::basename(filepath)

                           readr::read_delim(filepath, show_col_types = FALSE, lazy = FALSE) %>%
                             dplyr::mutate(sim = stringr::str_extract(basename, "(?<=_)[0-9]+(?=.txt)"))

                         })

estK <- allsims %>%
  dplyr::group_by(.data$sim) %>%
  dplyr::mutate(start_pop = first(pop_size)) %>%
  dplyr::slice(ceiling(dplyr::n()/2):dplyr::n()) %>%
  dplyr::summarise(K = median(.data$pop_size),
                   start_size = first(start_pop))

plot_data <- estK %>%
  dplyr::summarise(globalK = median(.data$K),
                   lower = min(.data$K),
                   upper = max(.data$K))

plot_data$globalK

## Median and harmonic mean of Kt
(medK      <- median(Kplot_data$globalK))
(harmonicK <- (sum(Kplot_data$globalK^-1)/nrow(Kplot_data))^-1)

## Harmonic of harmonic mean...
harm_fn <- function(x){
  (sum(x^-1)/length(x))^-1
}

Kplot_data %>%
  rowwise() %>%
  mutate(harm_mean = harm_fn(points$pop_size)) %>%
  ungroup() %>%
  summarise(harm_harm = harm_fn(harm_mean))

# Plot
## K DATA NEEDS DATE COLS SO IT CAN BE PLOTTED
## WE NEED TO SHOW THIS IT IS FLAT DURING THE YEAR (i.e. WE ASSUME IT IS FIXED)
Kplot_data_dates <- Kplot_data %>%
  dplyr::mutate(start = lubridate::ymd(paste(.data$year, "01", "01", sep = "-")),
                end = lubridate::ymd(paste(.data$year + 1, "01", "01", sep = "-"))) %>% tidyr::pivot_longer(cols = c(start, end), values_to = "date")

plotred <- "#EE4B2B"
plotblue <- "#000080"
ylim <- c(0, 1000)

# Boxplot of median K
K_boxplot <- ggplot() +
  geom_boxplot(data = Kplot_data,
               aes(x = 1, y = globalK),
               fill = plotred, alpha = 0.35) +
  geom_text(aes(x = 1, y = median(Kplot_data$globalK) * 1.05,
                label = round(median(Kplot_data$globalK)))) +
  scale_y_continuous(limits = ylim) +
  coord_cartesian(expand = FALSE) +
  theme_void() +
  theme(plot.margin = margin(r = 0, l = 0))

K_plot <- ggplot() +
  geom_ribbon(aes(x = c(as.Date("1995-01-01"), as.Date("2023-01-01")),
                  ymin = rep(plot_data$lower, 2),
                  ymax = rep(plot_data$upper, 2)),
              colour = NA, fill = "grey85", alpha = 0.5) +
  geom_hline(yintercept = plot_data$globalK,
             lty = 2, colour = "black") +
  geom_segment(aes(x = as.Date("1996-01-01"),
                   xend = as.Date("1996-01-01"),
                   y = plot_data$globalK,
                   yend = 725), linewidth = 0.25) +
  geom_segment(aes(x = as.Date("1996-01-01"),
                   xend = as.Date("1999-07-01"),
                   y = 725,
                   yend = 725), linewidth = 0.25) +
  geom_richtext(aes(x = as.Date("1995-11-15"),
                    y = 757),
                label = "Fixed<br>carrying capacity",
                hjust = 0, colour = "black",
                lineheight = 0.9, fill = NA, label.colour = NA,
                size = 2.75) +
  geom_line(data = real_pop, aes(x = date, y = pop_size),
            colour = plotblue, linewidth = 0.5, lty = 1) +
  geom_ribbon(data = Kplot_data_dates, aes(x = date, ymin = lower, ymax = upper),
              fill = plotred, alpha = 0.35) +
  geom_line(data = Kplot_data_dates, aes(x = date, y = globalK),
            colour = plotred, linewidth = 0.75) +
  geom_richtext(aes(x = as.Date("1996-06-01"),
                    y = 135),
                label = "Observed<br>population size",
                hjust = 0, colour = plotblue,
                lineheight = 0.9, fill = NA, label.colour = NA,
                size = 2.75) +
  geom_richtext(aes(x = as.Date("1996-11-01"),
                    y = 430),
                label = "Time-varying<br>carrying capacity",
                hjust = 0, colour = plotred,
                lineheight = 0.9, fill = NA, label.colour = NA,
                size = 2.75) +
  scale_x_date(breaks = seq.Date(as.Date("1995-01-01"), as.Date("2020-01-01"), by = "5 year"),
               date_labels = "%Y") +
  scale_y_continuous(breaks = seq(0, 1500, 250),
                     name = "Number of individuals",
                     expand = c(0, 0)) +
  coord_cartesian(clip = "off",
                  expand = FALSE,
                  ylim = ylim,
                  xlim = c(as.Date("1995-01-01"), as.Date("2023-01-01"))) +
  annotate(geom = "segment",
           y = 0, yend = -30,
           x = seq.Date(as.Date("1995-01-01"), as.Date("2020-01-01"), by = "5 year"),
           xend = seq.Date(as.Date("1995-01-01"), as.Date("2020-01-01"), by = "5 year"),
           lineend = "round", linejoin = "round") +
  annotate(geom = "segment",
           y = 0, yend = -10,
           x = seq.Date(as.Date("1995-01-01"), as.Date("2023-01-01"), by = "1 year"),
           xend = seq.Date(as.Date("1995-01-01"), as.Date("2023-01-01"), by = "1 year"),
           lineend = "round", linejoin = "round") +
  # labs(title = "Change in N and K over time") +
  theme_classic() +
  theme(axis.text = element_text(colour = "black"),
        axis.text.x = element_text(margin = margin(t = 10)),
        axis.title = element_blank(),
        axis.title.y = element_text(margin = margin(r = 5), size = 12),
        axis.ticks.y = element_line(lineend = "round"),
        plot.margin = margin(t = 20, r = 10, l = 10),
        panel.grid.major = element_line(colour = "grey75", linewidth = 0.25))

combo_plot <- K_plot + K_boxplot + patchwork::plot_layout(widths = c(17, 1))

ggsave(plot = combo_plot, filename = here::here("./plots/abs_KN_v_time_marginal.png"), dpi = 600,
       width = 8, height = 5)

## Same plot but with N year lines
real_pop_dates <- real_pop |>
  tidyr::pivot_longer(cols = c(from, to))

K_plot <- ggplot() +
  geom_ribbon(aes(x = c(as.Date("1995-01-01"), as.Date("2023-01-01")),
                  ymin = rep(plot_data$lower, 2),
                  ymax = rep(plot_data$upper, 2)),
              colour = NA, fill = "grey85", alpha = 0.5) +
  geom_hline(yintercept = plot_data$globalK,
             lty = 2, colour = "black") +
  geom_segment(aes(x = as.Date("1996-01-01"),
                   xend = as.Date("1996-01-01"),
                   y = plot_data$globalK,
                   yend = 725), linewidth = 0.25) +
  geom_segment(aes(x = as.Date("1996-01-01"),
                   xend = as.Date("1999-07-01"),
                   y = 725,
                   yend = 725), linewidth = 0.25) +
  geom_richtext(aes(x = as.Date("1995-11-15"),
                    y = 757),
                label = "Fixed<br>carrying capacity",
                hjust = 0, colour = "black",
                lineheight = 0.9, fill = NA, label.colour = NA,
                size = 2.75) +
  geom_line(data = real_pop_dates, aes(x = value, y = pop_size),
            colour = plotblue, linewidth = 0.5, lty = 1) +
  geom_ribbon(data = Kplot_data_dates, aes(x = date, ymin = lower, ymax = upper),
              fill = plotred, alpha = 0.35) +
  geom_line(data = Kplot_data_dates, aes(x = date, y = globalK),
            colour = plotred, linewidth = 0.75) +
  geom_richtext(aes(x = as.Date("1996-06-01"),
                    y = 135),
                label = "Observed<br>population size",
                hjust = 0, colour = plotblue,
                lineheight = 0.9, fill = NA, label.colour = NA,
                size = 2.75) +
  geom_richtext(aes(x = as.Date("1996-11-01"),
                    y = 430),
                label = "Time-varying<br>carrying capacity",
                hjust = 0, colour = plotred,
                lineheight = 0.9, fill = NA, label.colour = NA,
                size = 2.75) +
  scale_x_date(breaks = seq.Date(as.Date("1995-01-01"), as.Date("2020-01-01"), by = "5 year"),
               date_labels = "%Y") +
  scale_y_continuous(breaks = seq(0, 1500, 250),
                     name = "Number of individuals",
                     expand = c(0, 0)) +
  coord_cartesian(clip = "off",
                  expand = FALSE,
                  ylim = ylim,
                  xlim = c(as.Date("1995-01-01"), as.Date("2023-01-01"))) +
  annotate(geom = "segment",
           y = 0, yend = -30,
           x = seq.Date(as.Date("1995-01-01"), as.Date("2020-01-01"), by = "5 year"),
           xend = seq.Date(as.Date("1995-01-01"), as.Date("2020-01-01"), by = "5 year"),
           lineend = "round", linejoin = "round") +
  annotate(geom = "segment",
           y = 0, yend = -10,
           x = seq.Date(as.Date("1995-01-01"), as.Date("2023-01-01"), by = "1 year"),
           xend = seq.Date(as.Date("1995-01-01"), as.Date("2023-01-01"), by = "1 year"),
           lineend = "round", linejoin = "round") +
  # labs(title = "Change in N and K over time") +
  theme_classic() +
  theme(axis.text = element_text(colour = "black"),
        axis.text.x = element_text(margin = margin(t = 10)),
        axis.title = element_blank(),
        axis.title.y = element_text(margin = margin(r = 5), size = 12),
        axis.ticks.y = element_line(lineend = "round"),
        plot.margin = margin(t = 20, r = 10, l = 10),
        panel.grid.major = element_line(colour = "grey75", linewidth = 0.25))

combo_plot <- K_plot + K_boxplot + patchwork::plot_layout(widths = c(17, 1))

ggsave(plot = combo_plot, filename = here::here("./plots/abs_KN_v_time_marginal_year.png"), dpi = 600,
       width = 8, height = 5)
