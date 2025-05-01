## Prepare workspace
library(ggplot2)
library(hyenaR)
library(tidyr)
library(patchwork)
library(dplyr)

start_year  <- 1997
end_year    <- 2022
scale <- "year"

## Plot 1: Population size
## This is the MAX MONTHLY population size observed within each year
real_pop <- readRDS(here::here("./data/Nplot_data_year.RDS"))

plot_data <- real_pop %>%
  select(-max_adult_pop_size) |> ## Focus on whole population
  tidyr::pivot_longer(cols = max_pop_size) %>%
  filter(year >= start_year & year <= end_year)

(N_plot <- ggplot() +
    geom_col(data = plot_data, aes(x = year, y = value),
             colour = "black", linewidth = 0.25) +
    ## KRUUK ESTIMATE
    geom_text(aes(x = 1999,
                  y = (385 + 30)),
              label = "Adult population\nestimate (1960s)",
              size = 3,
              hjust = 1) +
    annotate(geom = "segment",
             x = 1996.5,
             xend = 2023,
             y = 385, yend = 385,
             lineend = "round", linejoin = "round", lty = 2,
             linewidth = 0.5) +
    ## X TICKS
    annotate(geom = "segment",
             x = seq(1997, 2023, by = 1),
             xend = seq(1997, 2023, by = 1),
             y = -10, yend = 0,
             lineend = "round", linejoin = "round") +
    annotate(geom = "segment",
             x = seq(2000, 2020, by = 5),
             xend = seq(2000, 2020, by = 5),
             y = -15, yend = 0,
             lineend = "round", linejoin = "round") +
    ## Y TICKS
    annotate(geom = "segment",
             x = 1996,
             xend = 1996 - 0.2,
             y = seq(0, 800, 100), yend = seq(0, 800, 100),
             lineend = "round", linejoin = "round") +
    scale_fill_manual(values = rev(c("grey90", "grey55", "grey20"))) +
    scale_x_continuous(breaks = seq(2000, 2020, by = 5)) +
    scale_y_continuous(breaks = seq(0, 800, 100),
                       name = "Number of individuals") +
    coord_cartesian(expand = FALSE, clip = "off",
                    ylim = c(0, 800),
                    xlim = c(1996, 2023)) +
    theme_classic() +
    theme(legend.position = "none",
          axis.title.x = element_blank(),
          axis.text.x = element_text(colour = "black",
                                     margin = margin(t = 10), size = 12),
          axis.ticks = element_blank(),
          axis.text.y = element_text(colour = "black", size = 12, margin = margin(r = 5)),
          axis.title.y = element_text(colour = "black", size = 17, margin = margin(r = 12)),
          plot.margin = margin(r = 30, t = 10, b = 10)))

ggsave(plot = N_plot, filename = here::here("./STEP3_supplementary_analysis/plots/N_v_time.png"), dpi = 600,
       width = 9, height = 5)

## Plot 1: Population size
## This is the MAX MONTHLY population size observed within each year
real_clan <- readRDS(here::here("./data/NClanplot_data_year.RDS"))

plot_data_clan <- real_clan %>%
  select(-max_adult_clan_size) |> ## Focus on whole population
  tidyr::pivot_longer(cols = max_clan_size) %>%
  filter(year >= start_year & year <= end_year)

(NClan_plot <- ggplot() +
    geom_col(data = plot_data_clan, aes(x = year, y = value, fill = clan),
             colour = "black", linewidth = 0.25) +
    ## X TICKS
    annotate(geom = "segment",
             x = seq(1997, 2023, by = 1),
             xend = seq(1997, 2023, by = 1),
             y = -3, yend = 0,
             lineend = "round", linejoin = "round") +
    annotate(geom = "segment",
             x = seq(2000, 2020, by = 5),
             xend = seq(2000, 2020, by = 5),
             y = -6, yend = 0,
             lineend = "round", linejoin = "round") +
    # ## Y TICKS
    annotate(geom = "segment",
             x = 1996,
             xend = 1996 - 0.2,
             y = seq(0, 150, 50), yend = seq(0, 150, 50),
             lineend = "round", linejoin = "round") +
    facet_wrap(facets = ~clan) +
    scale_x_continuous(breaks = seq(2000, 2020, by = 5)) +
    scale_y_continuous(
      breaks = seq(0, 150, 50),
      name = "Number of individuals"
    ) +
    coord_cartesian(expand = FALSE, clip = "off",
                    ylim = c(0, 150),
                    xlim = c(1996, 2023)) +
    theme_classic() +
    theme(legend.position = "none",
          axis.title.x = element_blank(),
          axis.text.x = element_text(colour = "black",
                                     margin = margin(t = 10),
                                     size = 12),
          axis.ticks = element_blank(),
          axis.text.y = element_text(colour = "black", size = 12, margin = margin(r = 5)),
          axis.title.y = element_text(colour = "black", size = 17, margin = margin(r = 12)),
          plot.margin = margin(r = 30, t = 10, b = 10)))

ggsave(plot = NClan_plot, filename = here::here("./STEP3_supplementary_analysis/plots/NClan_v_time.png"), dpi = 600,
       width = 9, height = 5)

## Extract data
### Population size separated by age and sex
### GENERATED IN STEP0_prepare_data/04_demographic_data.R
real_pop_separate <- readRDS(here::here("./data/Nplot_data_separate_1month.RDS"))

### Calculate sex ratio (male/all females)
ratios <- real_pop_separate %>%
  filter(young != 0) |> ## Last year has no data
  dplyr::mutate(sex_ratio = ad_male/(ad_male + ad_fem),
                age_ratio = young/(young + ad_male + ad_fem)
                # to = date + lubridate::years(1)
  ) |>
  dplyr::filter(lubridate::year(date) >= start_year & lubridate::year(date) <= end_year) |>
  tidyr::pivot_longer(cols = sex_ratio:age_ratio, names_to = "ratio_type", values_to = "ratio_val") |>
  group_by(ratio_type) |>
  tidyr::pivot_longer(cols = c(date)) |>
  ungroup()

# PLOT
## Sex and age ratio
end_vals <- ratios %>%
  group_by(ratio_type) %>%
  slice(n())

(ratios_plot <- ggplot() +
    geom_line(data = ratios, aes(x = value, y = ratio_val, colour = ratio_type, group = ratio_type)) +
    annotate(geom = "segment",
             x = as.Date("1995-01-01"),
             xend = as.Date("2025-01-01"),
             y = 0.5, yend = 0.5,
             lineend = "round", linejoin = "round", lty = 2,
             linewidth = 0.5) +
    ## X TICKS
    annotate(geom = "segment",
             x = seq(as.Date("1995-01-01"), as.Date("2025-01-01"), by = "1 year"),
             xend = seq(as.Date("1995-01-01"), as.Date("2025-01-01"), by = "1 year"),
             y = -0.01, yend = 0,
             lineend = "round", linejoin = "round") +
    annotate(geom = "segment",
             x = seq(as.Date("1995-01-01"), as.Date("2025-01-01"), by = "5 year"),
             xend = seq(as.Date("1995-01-01"), as.Date("2025-01-01"), by = "5 year"),
             y = -0.0175, yend = 0,
             lineend = "round", linejoin = "round") +
    ## Y TICKS
    annotate(geom = "segment",
             x = as.Date("1995-01-01"),
             xend = as.Date("1995-01-01") - 75,
             y = seq(0, 1, 0.25), yend = seq(0, 1, 0.25),
             lineend = "round", linejoin = "round") +
    ## LABELS
    geom_text(aes(x = as.Date("2023-06-01"),
                  y = end_vals$ratio_val + c(-0.05, 0.06), colour = end_vals$ratio_type),
              label = c("Proportion of\njuveniles", "Proportion of\nadult males"), size = 3,
              hjust = 0.5, lineheight = 0.75) +
    scale_colour_manual(values = c("red", "grey10")) +
    scale_x_date(breaks = seq(as.Date("1995-01-01"), as.Date("2025-01-01"), by = "5 years"),
                 date_labels = "%Y") +
    scale_y_continuous(breaks = seq(0, 1, 0.25),
                       name = "Proportion") +
    coord_cartesian(expand = FALSE, clip = "off",
                    ylim = c(0, 1),
                    xlim = c(as.Date("1995-01-01"), as.Date("2025-01-01"))) +
    theme_classic() +
    theme(legend.position = "none",
          axis.title.x = element_blank(),
          axis.text.x = element_text(colour = "black",
                                     margin = margin(t = 10), size = 12),
          axis.ticks = element_blank(),
          axis.text.y = element_text(colour = "black", size = 12, margin = margin(r = 5)),
          axis.title.y = element_text(colour = "black", size = 17, margin = margin(r = 12)),
          plot.margin = margin(r = 30, t = 10, b = 10)))

ggsave(plot = ratios_plot, filename = here::here("./STEP3_supplementary_analysis/plots/popratio_v_time.png"), dpi = 600,
       width = 9, height = 5)
