## Use all simulated outcomes from 2005
library(ggplot2)
library(ggtext)
library(here)

dir <- "~/data/simulation_results/analysis/STEP1_estimate_K/2001"

files <- list.files(dir, pattern = ".txt", full.names = TRUE)

outcome <- purrr::map_df(.x = files, .f = function(file){
  
  readr::read_delim(file, show_col_types = FALSE, lazy = FALSE) %>%
    dplyr::mutate(sim = stringr::str_extract(file, "(?<=_)[0-9]+(?=.txt)")) |> 
    select(sim, date, Nt = pop_size) |> 
    mutate(Nt1 = lead(Nt),
           lambdaN = Nt1/Nt) |> 
    filter(!is.na(lambdaN))
  
})

(output <- ggplot() +
  geom_point(data = outcome |> filter(date <= as.Date("2025-01-01")), aes(x = Nt, y = lambdaN), alpha = 0.15) +
  geom_hline(yintercept = 1, lty = 2) +
  geom_smooth(data = outcome |> filter(date <= as.Date("2025-01-01")), aes(x = Nt, y = lambdaN), method = "lm", se = FALSE) +
  theme_classic() +
  labs(x = "N<sub>t</sub>",
       y = "\U03BB") +
  theme_classic() +
  theme(axis.text = element_text(colour = "black", size = 12),
        axis.title.y = element_markdown(colour = "black", size = 15),
        axis.title.x = element_markdown(colour = "black", size = 15),
        legend.position = "none",
        plot.margin = margin(b = 15, l = 15, t = 15)))

ggsave(plot = output, filename = here("./analysis/plots/supp_emergent_dd_new.png"), dpi = 600, height = 5, width = 7)
