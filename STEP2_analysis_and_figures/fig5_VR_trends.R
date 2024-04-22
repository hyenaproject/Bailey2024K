
#Load required libraries
library(SHIM)
library(dplyr)
library(hyenaR)
library(ggplot2)
library(spaMM)


#This data is up until 2024
## (so that means that individual deaths should be detectable in 2022)
start_yr  <- 1997
end_yr    <- 2022

## GENERATED IN STEP0_prepare_data/model_fit.Rmd
model_data <- readRDS(here::here("./data/model_data.RDS"))

## Fem survival
F_surv_data <- model_data$F_surv_data |>
  dplyr::filter(year >= start_yr & year <= end_yr) |>
  mutate(globalmean = sum(surv)/n()) |>
  group_by(globalmean, year) |>
  summarise(binom::binom.wilson(n = n(), x = sum(surv)),
            VR = "Female survival")

### M predisp survival ####
PreM_surv_data <- model_data$PreM_surv_data |>
  dplyr::filter(year >= start_yr & year <= end_yr) |>
  mutate(globalmean = sum(surv)/n()) |>
  group_by(globalmean, year) |>
  summarise(binom::binom.wilson(n = n(), x = sum(surv)),
            VR = "Male survival\n (juvenile)")

### M postdisp survival ####
postM_surv_data <- model_data$PostM_surv_data %>%
  dplyr::filter(year >= start_yr & year <= end_yr) |>
  mutate(globalmean = sum(surv)/n()) |>
  group_by(globalmean, year) |>
  summarise(binom::binom.wilson(n = n(), x = sum(surv)),
            VR = "Male survival\n (adult)")

### Twin ####
F_twin_data <- model_data$F_twin_data %>%
  dplyr::filter(year >= start_yr & year <= end_yr) |>
  mutate(globalmean = sum(twin)/n()) |>
  group_by(globalmean, year) |>
  summarise(binom::binom.wilson(n = n(), x = sum(twin)),
            VR = "Twinning\n")

### All repro ####
F_repro_primi <- model_data$F_repro_primi %>%
  dplyr::filter(year >= start_yr & year <= end_yr) |>
  mutate(globalmean = sum(repro)/n()) |>
  group_by(globalmean, year) |>
  summarise(binom::binom.wilson(n = n(), x = sum(repro)),
            VR = "Reproduction\n(primiparous)")

F_repro_nonprimi <- model_data$F_repro_nonprimi %>%
  dplyr::filter(year >= start_yr & year <= end_yr) |>
  mutate(globalmean = sum(repro)/n()) |>
  group_by(globalmean, year) |>
  summarise(binom::binom.wilson(n = n(), x = sum(repro)),
            VR = "Reproduction\n(non-primiparous)")

plot_data <- do.call(bind_rows,
                     list(F_surv_data,
                          PreM_surv_data,
                          postM_surv_data,
                          F_repro_primi,
                          F_repro_nonprimi,
                          F_twin_data)) |>
  mutate(VR = factor(VR,
                     levels = c("Female survival", "Male survival\n (juvenile)", "Male survival\n (adult)",
                                "Reproduction\n(primiparous)", "Reproduction\n(non-primiparous)", "Twinning\n")))

bigtick_scale <- 0.0375
smalltick_scale <- 0.02

surv_ymax <- 1
surv_ymin <- 0.92

repro_ymax <- 0.1
repro_ymin <- 0

twin_ymax <- 0.8
twin_ymin <- 0.1

surv_panel <- ggplot(data = filter(plot_data, grepl("survival", x = VR))) +
  geom_ribbon(aes(x = year, ymin = lower, ymax = upper),
              fill = "grey85") +
  geom_line(aes(x = year, y = mean)) +
  geom_hline(aes(yintercept = globalmean), lty = 2) +
  coord_cartesian(clip = "off",
                  expand = FALSE,
                  ylim = c(surv_ymin, surv_ymax),
                  xlim = c(1995, 2022)) +
  annotate(geom = "segment",
           y = surv_ymin, yend = surv_ymin-(surv_ymax-surv_ymin)*bigtick_scale,
           x = seq(1995, 2020, by = 5),
           xend = seq(1995, 2020, by = 5),
           lineend = "round", linejoin = "round") +
  annotate(geom = "segment",
           y = surv_ymin, yend = surv_ymin-(surv_ymax-surv_ymin)*smalltick_scale,
           x = seq(1995, 2022, by = 1),
           xend = seq(1995, 2022, by = 1),
           lineend = "round", linejoin = "round") +
  facet_wrap(facets = ~VR, scales = "free_y") +
  labs(y = "Proportion") +
  scale_colour_discrete(guide = NULL) +
  theme_classic() +
  theme(axis.text = element_text(colour = "black"),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        strip.text = element_text(colour = "black"),
        strip.background = element_rect(linewidth = 0.5),
        plot.margin = margin(b = 5, t = 5))

repro_panel <- ggplot(data = filter(plot_data, grepl("Repro", x = VR))) +
  geom_ribbon(aes(x = year, ymin = lower, ymax = upper),
              fill = "grey85") +
  geom_line(aes(x = year, y = mean)) +
  geom_hline(aes(yintercept = globalmean), lty = 2) +
  coord_cartesian(clip = "off",
                  expand = FALSE,
                  ylim = c(repro_ymin, repro_ymax),
                  xlim = c(1995, 2022)) +
  annotate(geom = "segment",
           y = repro_ymin, yend = repro_ymin-(repro_ymax-repro_ymin)*bigtick_scale,
           x = seq(1995, 2020, by = 5),
           xend = seq(1995, 2020, by = 5),
           lineend = "round", linejoin = "round") +
  annotate(geom = "segment",
           y = repro_ymin, yend = repro_ymin-(repro_ymax-repro_ymin)*smalltick_scale,
           x = seq(1995, 2022, by = 1),
           xend = seq(1995, 2022, by = 1),
           lineend = "round", linejoin = "round") +
  labs(y = "Proportion") +
  scale_y_continuous(breaks = seq(0, 0.1, 0.02)) +
  facet_wrap(facets = ~VR, scales = "free_y") +
  scale_colour_discrete(guide = NULL) +
  theme_classic() +
  theme(axis.text = element_text(colour = "black"),
        axis.text.x = element_text(margin = margin(t = 4)),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        strip.text = element_text(colour = "black"),
        strip.background = element_rect(linewidth = 0.5))

twin_panel <- ggplot(data = filter(plot_data, grepl("Twin", x = VR))) +
  geom_ribbon(aes(x = year, ymin = lower, ymax = upper),
              fill = "grey85") +
  geom_line(aes(x = year, y = mean)) +
  geom_hline(aes(yintercept = globalmean), lty = 2) +
  coord_cartesian(clip = "off",
                  expand = FALSE,
                  ylim = c(twin_ymin, twin_ymax),
                  xlim = c(1995, 2022)) +
  annotate(geom = "segment",
           y = twin_ymin, yend = twin_ymin-(twin_ymax-twin_ymin)*bigtick_scale,
           x = seq(1995, 2020, by = 5),
           xend = seq(1995, 2020, by = 5),
           lineend = "round", linejoin = "round") +
  annotate(geom = "segment",
           y = twin_ymin, yend = twin_ymin-(twin_ymax-twin_ymin)*smalltick_scale,
           x = seq(1995, 2022, by = 1),
           xend = seq(1995, 2022, by = 1),
           lineend = "round", linejoin = "round") +
  scale_y_continuous(breaks = seq(0.1, 0.8, 0.1),
                     labels = format(seq(0.1, 0.8, 0.1), nsmall = 2)) +
  facet_wrap(facets = ~VR, scales = "free_y") +
  scale_colour_discrete(guide = NULL) +
  theme_classic() +
  theme(axis.text = element_text(colour = "black"),
        axis.text.x = element_text(margin = margin(t = 4)),
        axis.title = element_blank(),
        axis.ticks.x = element_blank(),
        strip.text = element_text(colour = "black"),
        strip.background = element_rect(linewidth = 0.5),
        plot.margin = margin(b = 5, t = 5))

library(patchwork)

surv_panel/((repro_panel|twin_panel) + plot_layout(widths = c(2.2, 1)))

ggsave(filename = here::here("./plots/VR_time.png"), dpi = 600,
       width = 8, height = 5)
