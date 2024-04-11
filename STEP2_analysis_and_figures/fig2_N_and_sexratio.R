## Prepare workspace
library(ggplot2)
library(hyenaR)
library(tidyr)
library(patchwork)
library(dplyr)

start_year  <- 1997
end_year    <- 2021
scale <- "year"

## Extract data
### Population size separated by age and sex 
if (scale == "month"){
  
  ### GENERATED IN STEP0_prepare_data/demographic_data.R
  real_pop_separate <- readRDS(here::here("./analysis/data/Nplot_data_separate_1month.RDS"))
  
  ### Calculate sex ratio (male/all females)
  ratios <- real_pop_separate %>% 
    dplyr::mutate(sex_ratio = ad_male/(ad_male + ad_fem),
                  age_ratio = young/(young + ad_male + ad_fem)) %>% 
    tidyr::pivot_longer(cols = sex_ratio:age_ratio)
  
  # PLOT
  
  ## Change in N over time
  
  plot_data <- real_pop_separate %>% 
    tidyr::pivot_longer(cols = young:ad_fem) %>% 
    #Remove unknown sex (small and is not detectable on plot)
    # filter(name != "ad_unk") %>% 
    dplyr:::mutate(name = factor(name, levels = c("young", "ad_male", "ad_fem")),
                   adult_edge = name != "ad_fem")
  
  end_vals <- plot_data %>% 
    group_by(name) %>% 
    slice(n()) %>% 
    mutate(grp = grepl(name, pattern = "ad_")) %>% 
    group_by(grp) %>% 
    summarise(date = first(date),
              value = sum(value))
  
  sex_vals <- plot_data %>% 
    group_by(name) %>% 
    slice(n() - 10) %>% 
    filter(name != "young")
  
  N_plot <- ggplot(data = plot_data) +
    geom_area(aes(x = date, y = value, fill = name,
                  linewidth = adult_edge), colour = "black") +
    ## KRUUK ESTIMATE
    geom_text(aes(x = as.Date("1999-01-01"),
                  y = (385 + 40)),
              label = "Adult population\nestimate (1960s)", size = 3,
              hjust = 1) +
    annotate(geom = "segment",
             x = as.Date("1995-01-01"),
             xend = end_vals$date[1] - 100,
             y = 385, yend = 385,
             lineend = "round", linejoin = "round", lty = 2,
             linewidth = 0.5) +
    ## X TICKS
    annotate(geom = "segment",
             x = seq(as.Date("1995-01-01"), as.Date("2025-01-01"), by = "1 year"),
             xend = seq(as.Date("1995-01-01"), as.Date("2025-01-01"), by = "1 year"),
             y = -10, yend = 0,
             lineend = "round", linejoin = "round") +
    annotate(geom = "segment",
             x = seq(as.Date("1995-01-01"), as.Date("2025-01-01"), by = "5 year"),
             xend = seq(as.Date("1995-01-01"), as.Date("2025-01-01"), by = "5 year"),
             y = -20, yend = 0,
             lineend = "round", linejoin = "round") +
    ## Y TICKS
    annotate(geom = "segment",
             x = as.Date("1995-01-01"),
             xend = as.Date("1995-01-01") - 75,
             y = seq(0, 800, 100), yend = seq(0, 800, 100),
             lineend = "round", linejoin = "round") +
    ## LABEL TEXT
    annotate(geom = "segment",
             x = end_vals$date[1] + 100,
             xend = end_vals$date[1] + 100,
             y = 5, yend = end_vals$value[2] - 5,
             lineend = "round", linejoin = "round", linewidth = 0.75) +
    annotate(geom = "segment",
             x = end_vals$date[1] + 100,
             xend = end_vals$date[1] + 100,
             y = end_vals$value[2] + 5, yend = end_vals$value[2] + end_vals$value[1] - 5,
             lineend = "round", linejoin = "round", linewidth = 0.75) +
    ## ADD TEXT FOR AD/YOUNG
    annotate(geom = "text",
             x = end_vals$date[1] + 175,
             y = mean(c(5, end_vals$value[2] - 5)),
             label = "ADULT", hjust = 0, size = 4) +
    annotate(geom = "text",
             x = end_vals$date[1] + 175,
             y = mean(c(end_vals$value[2] + 5, end_vals$value[2] + end_vals$value[1] - 5)),
             label = "JUVENILE", hjust = 0, size = 4) +
    ## ADD TEXT FOR MALE/FEMALE
    annotate(geom = "text",
             x = sex_vals$date[1] + 100,
             y = mean(c(sex_vals$value[2], sex_vals$value[2] + sex_vals$value[1])),
             label = "MALE", hjust = 1, size = 3.5,
             colour = "grey95") +
    annotate(geom = "text",
             x = sex_vals$date[1] + 100,
             y = mean(c(0, sex_vals$value[2])),
             label = "FEMALE", hjust = 1, size = 3.5,
             colour = "grey95") +
    scale_fill_manual(values = c("grey90", "grey55", "grey20")) +
    scale_linewidth_discrete(range = c(0, 0.75)) +
    scale_x_date(breaks = seq(as.Date("1995-01-01"), as.Date("2025-01-01"), by = "5 years"),
                 date_labels = "%Y") +
    scale_y_continuous(breaks = seq(0, 800, 100),
                       name = "Number of individuals") +
    coord_cartesian(expand = FALSE, clip = "off",
                    ylim = c(0, 800),
                    xlim = c(as.Date("1995-01-01"), as.Date("2025-01-01"))) +
    theme_classic() +
    theme(legend.position = "none",
          axis.title.x = element_blank(),
          axis.text.x = element_text(colour = "black",
                                     margin = margin(t = 10), size = 10),
          axis.ticks = element_blank(),
          axis.text.y = element_text(colour = "black", size = 10, margin = margin(r = 5)),
          axis.title.y = element_text(colour = "black", size = 15, margin = margin(r = 12)),
          plot.margin = margin(r = 30, t = 10, b = 10))
  
  ggsave(here::here("./analysis/plots/N_v_time_new.png"), dpi = 600,
         width = 9, height = 5)
  
  ## Sex and age ratio
  
  end_vals <- ratios %>% 
    group_by(name) %>% 
    slice(n())
  
  (ratios_plot <- ggplot() +
      geom_line(data = ratios, aes(x = date, y = value, colour = name, group = name)) +
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
               y = -0.0125, yend = 0,
               lineend = "round", linejoin = "round") +
      annotate(geom = "segment",
               x = seq(as.Date("1995-01-01"), as.Date("2025-01-01"), by = "5 year"),
               xend = seq(as.Date("1995-01-01"), as.Date("2025-01-01"), by = "5 year"),
               y = -0.025, yend = 0,
               lineend = "round", linejoin = "round") +
      ## Y TICKS
      annotate(geom = "segment",
               x = as.Date("1995-01-01"),
               xend = as.Date("1995-01-01") - 75,
               y = seq(0, 1, 0.25), yend = seq(0, 1, 0.25),
               lineend = "round", linejoin = "round") +
      ## LABELS
      geom_text(aes(x = as.Date("2023-10-01"),
                    y = end_vals$value + c(0, -0.025), colour = end_vals$name),
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
                                       margin = margin(t = 10), size = 10),
            axis.ticks = element_blank(),
            axis.text.y = element_text(colour = "black", size = 10, margin = margin(r = 5)),
            axis.title.y = element_text(colour = "black", size = 15, margin = margin(r = 12)),
            plot.margin = margin(r = 30, t = 10, b = 10)))
  
  ggsave(here::here("./analysis/plots/popratio_v_time_new.png"), dpi = 600,
         width = 9, height = 5)
  
  N_plot + ratios_plot + patchwork::plot_layout(nrow = 2)
  
  ggsave(here::here("./analysis/plots/N_popratio_v_time_new.png"), dpi = 600,
         width = 9, height = 8)
  
} else if (scale == "year"){
  
  ### GENERATED IN STEP0_prepare_data/demographic_data.R
  real_pop_separate <- readRDS(here::here("./analysis/data/Nplot_data_separate_year.RDS"))
  
  ### Calculate sex ratio (male/all females)
  ratios <- real_pop_separate %>% 
    dplyr::mutate(sex_ratio = ad_male/(ad_male + ad_fem),
                  age_ratio = young/(young + ad_male + ad_fem)) %>% 
    tidyr::pivot_longer(cols = sex_ratio:age_ratio) |> 
    filter(lubridate::year(date) != 2023)
  
  # PLOT
  
  ## Change in N over time
  
  plot_data <- real_pop_separate %>% 
    tidyr::pivot_longer(cols = young:ad_fem) %>% 
    #Remove unknown sex (small and is not detectable on plot)
    # filter(name != "ad_unk") %>% 
    dplyr:::mutate(name = factor(name, levels = c("young", "ad_male", "ad_fem")),
                   adult_edge = name != "ad_fem") |> 
    filter(lubridate::year(date) != 2023)
  
  end_vals <- plot_data %>% 
    group_by(name) %>% 
    slice(n()) %>% 
    mutate(grp = grepl(name, pattern = "ad_")) %>% 
    group_by(grp) %>% 
    summarise(date = first(date),
              value = sum(value))
  
  sex_vals <- plot_data %>% 
    group_by(name) %>% 
    slice(n() - 10) %>% 
    filter(name != "young")
  
  N_plot <- ggplot(data = plot_data) +
    geom_area(aes(x = date, y = value, fill = name,
                  linewidth = adult_edge), colour = "black") +
    ## KRUUK ESTIMATE
    geom_text(aes(x = as.Date("1999-01-01"),
                  y = (385 + 40)),
              label = "Adult population\nestimate (1960s)", size = 3,
              hjust = 1) +
    annotate(geom = "segment",
             x = as.Date("1995-01-01"),
             xend = end_vals$date[1] - 100,
             y = 385, yend = 385,
             lineend = "round", linejoin = "round", lty = 2,
             linewidth = 0.5) +
    ## X TICKS
    annotate(geom = "segment",
             x = seq(as.Date("1995-01-01"), as.Date("2025-01-01"), by = "1 year"),
             xend = seq(as.Date("1995-01-01"), as.Date("2025-01-01"), by = "1 year"),
             y = -10, yend = 0,
             lineend = "round", linejoin = "round") +
    annotate(geom = "segment",
             x = seq(as.Date("1995-01-01"), as.Date("2025-01-01"), by = "5 year"),
             xend = seq(as.Date("1995-01-01"), as.Date("2025-01-01"), by = "5 year"),
             y = -20, yend = 0,
             lineend = "round", linejoin = "round") +
    ## Y TICKS
    annotate(geom = "segment",
             x = as.Date("1995-01-01"),
             xend = as.Date("1995-01-01") - 75,
             y = seq(0, 800, 100), yend = seq(0, 800, 100),
             lineend = "round", linejoin = "round") +
    ## LABEL TEXT
    annotate(geom = "segment",
             x = end_vals$date[1] + 100,
             xend = end_vals$date[1] + 100,
             y = 5, yend = end_vals$value[2] - 5,
             lineend = "round", linejoin = "round", linewidth = 0.75) +
    annotate(geom = "segment",
             x = end_vals$date[1] + 100,
             xend = end_vals$date[1] + 100,
             y = end_vals$value[2] + 5, yend = end_vals$value[2] + end_vals$value[1] - 5,
             lineend = "round", linejoin = "round", linewidth = 0.75) +
    ## ADD TEXT FOR AD/YOUNG
    annotate(geom = "text",
             x = end_vals$date[1] + 175,
             y = mean(c(5, end_vals$value[2] - 5)),
             label = "ADULT", hjust = 0, size = 4) +
    annotate(geom = "text",
             x = end_vals$date[1] + 175,
             y = mean(c(end_vals$value[2] + 5, end_vals$value[2] + end_vals$value[1] - 5)),
             label = "JUVENILE", hjust = 0, size = 4) +
    ## ADD TEXT FOR MALE/FEMALE
    annotate(geom = "text",
             x = sex_vals$date[1] + 100,
             y = mean(c(sex_vals$value[2], sex_vals$value[2] + sex_vals$value[1])),
             label = "MALE", hjust = 1, size = 3.5,
             colour = "grey95") +
    annotate(geom = "text",
             x = sex_vals$date[1] + 100,
             y = mean(c(0, sex_vals$value[2])),
             label = "FEMALE", hjust = 1, size = 3.5,
             colour = "grey95") +
    scale_fill_manual(values = c("grey90", "grey55", "grey20")) +
    scale_linewidth_discrete(range = c(0, 0.75)) +
    scale_x_date(breaks = seq(as.Date("1995-01-01"), as.Date("2025-01-01"), by = "5 years"),
                 date_labels = "%Y") +
    scale_y_continuous(breaks = seq(0, 800, 100),
                       name = "Number of individuals") +
    coord_cartesian(expand = FALSE, clip = "off",
                    ylim = c(0, 800),
                    xlim = c(as.Date("1995-01-01"), as.Date("2025-01-01"))) +
    theme_classic() +
    theme(legend.position = "none",
          axis.title.x = element_blank(),
          axis.text.x = element_text(colour = "black",
                                     margin = margin(t = 10), size = 10),
          axis.ticks = element_blank(),
          axis.text.y = element_text(colour = "black", size = 10, margin = margin(r = 5)),
          axis.title.y = element_text(colour = "black", size = 15, margin = margin(r = 12)),
          plot.margin = margin(r = 30, t = 10, b = 10))
  
  ggsave(here::here("./analysis/plots/N_v_time_new.png"), dpi = 600,
         width = 9, height = 5)
  
  ## Sex and age ratio
  
  end_vals <- ratios %>% 
    group_by(name) %>% 
    slice(n())
  
  (ratios_plot <- ggplot() +
      geom_line(data = ratios, aes(x = date, y = value, colour = name, group = name)) +
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
               y = -0.0125, yend = 0,
               lineend = "round", linejoin = "round") +
      annotate(geom = "segment",
               x = seq(as.Date("1995-01-01"), as.Date("2025-01-01"), by = "5 year"),
               xend = seq(as.Date("1995-01-01"), as.Date("2025-01-01"), by = "5 year"),
               y = -0.025, yend = 0,
               lineend = "round", linejoin = "round") +
      ## Y TICKS
      annotate(geom = "segment",
               x = as.Date("1995-01-01"),
               xend = as.Date("1995-01-01") - 75,
               y = seq(0, 1, 0.25), yend = seq(0, 1, 0.25),
               lineend = "round", linejoin = "round") +
      ## LABELS
      geom_text(aes(x = as.Date("2023-10-01"),
                    y = end_vals$value + c(0, -0.025), colour = end_vals$name),
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
                                       margin = margin(t = 10), size = 10),
            axis.ticks = element_blank(),
            axis.text.y = element_text(colour = "black", size = 10, margin = margin(r = 5)),
            axis.title.y = element_text(colour = "black", size = 15, margin = margin(r = 12)),
            plot.margin = margin(r = 30, t = 10, b = 10)))
  
  ggsave(here::here("./analysis/plots/popratio_v_time_new.png"), dpi = 600,
         width = 9, height = 5)
  
  N_plot + ratios_plot + patchwork::plot_layout(nrow = 2)
  
  ggsave(here::here("./analysis/plots/N_popratio_v_time_new.png"), dpi = 600,
         width = 9, height = 8)
  
}
