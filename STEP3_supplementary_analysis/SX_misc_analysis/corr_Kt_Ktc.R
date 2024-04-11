library(dplyr)

Kplot_data <- readRDS(here::here("./analysis/data/Kplot_data.RDS"))

Kplot_data_byclan <- readRDS(here::here("./analysis/data/Kplot_clan_data.RDS"))

plot_data <- Kplot_data_byclan %>% 
  select(sim, A:T, year) %>% 
  tidyr::pivot_longer(A:T) %>% 
  group_by(name, year) %>% 
  summarise(median = as.integer(median(value)),
            min = min(value),
            max = max(value)) %>% 
  arrange(name, year) %>% 
  mutate(n = 1:n()) %>% 
  group_by(year) %>% 
  summarise(Kt_est = sum(median))

corr_data <- left_join(Kplot_data, plot_data, by = "year")

cor(corr_data$Kt_est, corr_data$globalK)
