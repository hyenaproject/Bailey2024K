library(dplyr)
library(ggplot2)

## Growth of N
Nt_old <- function(t, N0, K, r){
  (K*N0*exp(r*t))/(K+N0*(exp(r*t) - 1))
}

# ggplot() +
#   geom_line(aes(x = 0:500, y = Nt(t = 0:500, K = 200, N0 = 10, r = 0.025)))

############

## Change in K with time

Kt <- function(t, K1 = 200, K2 = 400, Kr = 0.1, t_inflection = 50){
  K1 + (K2/(1 + exp(-Kr*(t - t_inflection))))
}

############

## Change in Nt with change in Kt

Nt <- function(t, N0, r, K1, K2 = 0, Kr = 0, t_inflection = 0,
               verbose = FALSE){

  Kt <- Kt(t = t, K1 = K1,
           K2 = K2, Kr = Kr,
           t_inflection = t_inflection)
  Nt <- Kt/(1 + ((Kt - N0)/N0)*exp(-r*t))

  if (verbose) {
    print(paste("t: ", t, "K: ", Kt, "N: ", Nt))
  }

  return(Nt)

}

#########

## Create our figure

### Panel 1: Rapid growth in N matches K

plot_data_fig1a <- data.frame(t = 0:500, K1 = 100, K2 = 400, Kr = 0.02, t_inflection = 200,
                              N0 = 100, r = 0.025) |>
  mutate(Kt = Kt(t = t, K1 = K1, K2 = K2, Kr = Kr, t_inflection = t_inflection),
         Nt = NA,
         panel = c("A) Improving environment"))

for (i in 2:nrow(plot_data_fig1a)) {

  N0 <- if (i == 2) plot_data_fig1a$N0[1] else plot_data_fig1a$Nt[i - 1]

  plot_data_fig1a$Nt[i] <- Nt_old(t = 1, N0 = N0, r = plot_data_fig1a$r[1], K = plot_data_fig1a$Kt[i])

}

ggplot(data = plot_data_fig1a) +
  geom_line(aes(x = t, y = Kt), lty = 2) +
  geom_line(aes(x = t, y = Nt), lty = 1)

### Panel 2: Rapid growth in N stable K

plot_data_fig1b <- data.frame(t = 0:500, K1 = 500, K2 = 0, Kr = 0, t_inflection = 0,
                              N0 = 100,
                              r = 0.00725) |>
  mutate(Kt = Kt(t = t, K1 = K1, K2 = K2, Kr = Kr, t_inflection = t_inflection),
         Nt = NA,
         panel = c("B) Stable environment"))

for (i in 2:nrow(plot_data_fig1b)) {

  N0 <- if (i == 2) plot_data_fig1b$N0[1] else plot_data_fig1b$Nt[i - 1]

  plot_data_fig1b$Nt[i] <- Nt_old(t = 1, N0 = N0, r = plot_data_fig1b$r[1], K = plot_data_fig1b$Kt[i])

}

ggplot(data = plot_data_fig1b) +
  geom_line(aes(x = t, y = Kt), lty = 2) +
  geom_line(aes(x = t, y = Nt), lty = 1)

### Panel 3: Slow growth in N declining K

plot_data_fig1c <- data.frame(t = 0:500, K1 = 500, K2 = -400, Kr = 0.009, t_inflection = 350,
                              N0 = 100, r = 0.008) |>
  mutate(Kt = Kt(t = t, K1 = K1, K2 = K2, Kr = Kr, t_inflection = t_inflection),
         Nt = NA,
         panel = c("C) Deteriorating environment"))

for (i in 2:nrow(plot_data_fig1c)) {

  N0 <- if (i == 2) plot_data_fig1c$N0[1] else plot_data_fig1c$Nt[i - 1]

  plot_data_fig1c$Nt[i] <- Nt_old(t = 1, N0 = N0, r = plot_data_fig1c$r[1], K = plot_data_fig1c$Kt[i])

}

ggplot(data = plot_data_fig1c) +
  geom_line(aes(x = t, y = Kt), lty = 2) +
  geom_line(aes(x = t, y = Nt), lty = 1)

############

### Combine:
library(patchwork)
library(ggtext)

panel1 <- ggplot() +
  geom_line(data = plot_data_fig1a,
            aes(x = t, y = Kt), lty = 2, linewidth = 1.2) +
  geom_line(data = plot_data_fig1a,
            aes(x = t, y = Nt), lty = 1, linewidth = 1.2) +
  geom_ribbon(aes(x = c(250, Inf),
                  ymin = c(-Inf, -Inf),
                  ymax = c(Inf, Inf)), alpha = 0.25) +
  # geom_richtext(aes(x = 15, y = 540, label = "a)"),
  #           size = 8,
  #           fill = NA, label.colour = NA,) +
  geom_richtext(aes(x = 200, y = 390,
                    label = "K<sub>t</sub>"),
                fill = NA, label.colour = NA,
                size = 17) +
  geom_richtext(aes(x = 200, y = 150,
                    label = "N<sub>t</sub>"),
                fill = NA, label.colour = NA,
                size = 17) +
  geom_richtext(aes(x = c(125, 375),
                    y = c(550, 550),
                    label = c("Monitoring<br>period", "Future")),
                fill = NA, label.colour = NA,
                size = 12, hjust = 0.5, vjust = 1) +
  coord_cartesian(ylim = c(NA, 550)) +
  labs(y = "Number of individuals") +
  facet_wrap(facets = ~panel) +
  theme_classic() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 35, margin = margin(r = 10, l = 10)),
        axis.line = element_blank(),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(size = 35, colour = "black",
                                  margin = margin(t = 10, b = 10)),
        panel.background = element_rect(colour = "black", linewidth = 1.5))

panel2 <- ggplot() +
  geom_line(data = plot_data_fig1b,
            aes(x = t, y = Kt), lty = 2, linewidth = 1.2) +
  geom_line(data = plot_data_fig1b,
            aes(x = t, y = Nt), lty = 1, linewidth = 1.2) +
  geom_ribbon(aes(x = c(250, Inf),
                  ymin = c(-Inf, -Inf),
                  ymax = c(Inf, Inf)), alpha = 0.25) +
  # geom_text(aes(x = 15, y = 540, label = "b)"),
  #           size = 8) +
  coord_cartesian(ylim = c(NA, 550)) +
  facet_wrap(facets = ~panel) +
  labs(x = "Time") +
  theme_classic() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 35, margin = margin(t = 10, b = 10)),
        axis.line = element_blank(),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(size = 35, colour = "black",
                                  margin = margin(t = 10, b = 10)),
        panel.background = element_rect(colour = "black", linewidth = 1.5))

panel3 <- ggplot() +
  geom_line(data = plot_data_fig1c,
            aes(x = t, y = Kt), lty = 2, linewidth = 1.2) +
  geom_line(data = plot_data_fig1c,
            aes(x = t, y = Nt), lty = 1, linewidth = 1.2) +
  geom_ribbon(aes(x = c(250, Inf),
                  ymin = c(-Inf, -Inf),
                  ymax = c(Inf, Inf)), alpha = 0.25) +
  # geom_text(aes(x = 15, y = 540, label = "c)"),
  #           size = 8) +
  coord_cartesian(ylim = c(NA, 550)) +
  facet_wrap(facets = ~panel) +
  theme_classic() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.line = element_blank(),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(size = 35, colour = "black",
                                  margin = margin(t = 10, b = 10)),
        panel.background = element_rect(colour = "black", linewidth = 1.5))

panel1 + panel2 + panel3 + plot_layout(nrow = 1)

ggsave(filename = here::here("./plots/K_theory_plot.png"), dpi = 600,
       height = 10, width = 30)
