library(dplyr)
library(ggplot2)
library(spaMM)
library(forcats)

# Helper functions --------------------------------------------------------

pretty <- function(x, digits = 3) {
  format <- paste0("%#.", digits, "g")
  sprintf(format, signif(x, digits = digits))
}

format_estimates <- function(fit, rounded = FALSE){
  fit_summ <- spaMM::summary.HLfit(fit, verbose = FALSE)
  beta <- cbind(Parameter = rownames(fit_summ$beta_table), as.data.frame(fit_summ$beta_table))
  rownames(beta) <- NULL
  if (!is.null(fit_summ$lambda_table)) {
    lambda <- data.frame(Parameter = "clan:year", Estimate = as.numeric(fit_summ$lambda_table["Var."]),
                         CondSE = NA, t = NA)
  } else {
    lambda <- data.frame(Parameter = "clan:year", Estimate = NA,
                         CondSE = NA, t = NA)
  }
  colnames(lambda) <- colnames(beta)
  estimates <- rbind(beta, lambda)
  if (rounded) {
    estimates$Estimate   <- pretty(estimates$Estimate)
    estimates$`Cond. SE` <- pretty(estimates$`Cond. SE`)
    estimates$`t-value`  <- pretty(estimates$`t-value`)
  }
  estimates
}

extract_table <- function(model_list, rounded = FALSE) {
  full_table <- rbind(cbind(Model = "primirepro", format_estimates(model_list$primirepro, rounded = rounded)),
                      cbind(Model = "nonprimirepro", format_estimates(model_list$nonprimirepro, rounded = rounded)),
                      cbind(Model = "twin", format_estimates(model_list$twin, rounded = rounded)),
                      cbind(Model = "allF", format_estimates(model_list$allF, rounded = rounded)),
                      cbind(Model = "predispM", format_estimates(model_list$predispM, rounded = rounded)),
                      cbind(Model = "postdispM", format_estimates(model_list$postdispM, rounded = rounded)),
                      cbind(Model = "disp", format_estimates(model_list$disp, rounded = rounded)))
}

pdep_compute <- function(fit) {
  d <- fit$data

  d$rank_category2 <- "top"
  pdep_top <- pdep_effects(fit, "clan_size", newdata = d, focal_values = seq(20, 120, 10))
  pdep_top$rank_category2 <- "top"

  d$rank_category2 <- "middle"
  pdep_middle <- pdep_effects(fit, "clan_size", newdata = d, focal_values = seq(20, 120, 10))
  pdep_middle$rank_category2 <- "middle"

  d$rank_category2 <- "bottom"
  pdep_low <- pdep_effects(fit, "clan_size", newdata = d, focal_values = seq(20, 120, 10))
  pdep_low$rank_category2 <- "bottom"

  pdep_all <- rbind(pdep_top, pdep_middle, pdep_low)
  pdep_all$rank_category2 <- factor(pdep_all$rank_category2, levels = c("top", "middle", "bottom"))
  pdep_all
}


# Build table of estimates ------------------------------------------------

model_list <- readRDS(here::here("./data/model_list.RDS"))
full_table <- extract_table(model_list, rounded = TRUE)
full_table
#write.csv(full_table, file = "full_table_estimates.csv", row.names = FALSE)


# Extract sample sizes ----------------------------------------------------

nrow(model_list$primirepro$data)
nrow(model_list$nonprimirepro$data)
nrow(model_list$twin$data)
nrow(model_list$allF$data)
nrow(model_list$predispM$data)
nrow(model_list$postdispM$data)
nrow(model_list$disp$data)


# Plot density dependence -------------------------------------------------

primirepro_pdep    <- pdep_compute(model_list$primirepro)
nonprimirepro_pdep <- pdep_compute(model_list$nonprimirepro)
twin_pdep          <- pdep_compute(model_list$twin)
allF_pdep          <- pdep_compute(model_list$allF)
predispM_pdep      <- pdep_compute(model_list$predispM)
postdispM_pdep     <- pdep_compute(model_list$postdispM)

all_pdep <-  rbind(cbind(Model = "a) Female survival", allF_pdep),
                   cbind(Model = "b) Male survival\n(juvenile)", predispM_pdep),
                   cbind(Model = "c) Male survival\n(adult)", postdispM_pdep),
                   cbind(Model = "d) Reproduction\n(primiparous)", primirepro_pdep),
                   cbind(Model = "e) Reproduction\n(non-primiparous)", nonprimirepro_pdep),
                   cbind(Model = "f) Twinning\n ", twin_pdep))

all_pdep$Model <- forcats::fct_inorder(all_pdep$Model)

## FULL PLOT
##
ggplot(all_pdep) +
  aes(y = pointp , x = focal_var, col = rank_category2, fill = rank_category2) +
  geom_line() +
  geom_ribbon(aes(ymin = low, ymax = up), alpha = 0.3, linetype = "dotted") +
  facet_wrap(~ Model, scales = "free_y") +
  labs(x = "Number of individuals in the clan", y = "Predicted probability of event",
       colour = "Social rank", fill = "Social rank") +
  scale_y_continuous(minor_breaks = NULL) +
  scale_x_continuous(minor_breaks = NULL, breaks = seq(20, 120, 20)) +
  theme_classic() +
  theme()


## PLOT WITH JUST TOP AND BOTTOM RANK

bigtick_scale <- 0.0375
smalltick_scale <- 0.02

surv_ymax <- 1
surv_ymin <- 0.82

repro_ymax <- 0.125
repro_ymin <- -0.005

twin_ymax <- 0.8
twin_ymin <- 0

xmax <- 125
xmin <- 20

plot_data <- all_pdep |>
  filter(rank_category2 != "middle" & !(Model == "c) Male survival\n(adult)" & rank_category2 == "top")) |>
  mutate(rank_category2 = case_when(Model == "c) Male survival\n(adult)" ~ "all adults",
                                    TRUE ~ rank_category2))

surv_data <- plot_data |>
  filter(grepl(pattern = "survival", x = Model))

surv_panel <- ggplot() +
  geom_ribbon(data = surv_data,
              aes(y = pointp , x = focal_var, col = rank_category2,
                  ymin = low, ymax = up), alpha = 0.3, linetype = "dotted", fill = NA, linewidth = 0.5) +
  geom_line(data = surv_data,
            aes(y = pointp , x = focal_var, col = rank_category2),
            linewidth = 1) +
  geom_text(data = surv_data |>
              group_by(Model, rank_category2) |>
              slice(n() - 1) |>
              ungroup(),
            aes(x = focal_var, y = pointp + c(-0.007, 0.00575,
                                              -0.0065, 0.0055,
                                              -0.0055), label = rank_category2,
                colour = rank_category2),
            angle = c(-22,-7.5,
                      -27,-5,
                      -3), hjust = 0.5, size = 2) +
  facet_wrap(~ Model, scales = "free_y") +
  labs(x = "Number of individuals in the clan", y = "Predicted probability of event",
       colour = "Social rank", fill = "Social rank") +
  coord_cartesian(clip = "off",
                  expand = FALSE,
                  ylim = c(surv_ymin, surv_ymax),
                  xlim = c(xmin, xmax)) +
  annotate(geom = "segment",
           y = surv_ymin, yend = surv_ymin-(surv_ymax-surv_ymin)*bigtick_scale,
           x = seq(xmin, xmax, by = 20),
           xend = seq(xmin, xmax, by = 20),
           lineend = "round", linejoin = "round") +
  annotate(geom = "segment",
           y = surv_ymin, yend = surv_ymin-(surv_ymax-surv_ymin)*smalltick_scale,
           x = seq(xmin, xmax, by = 5),
           xend = seq(xmin, xmax, by = 5),
           lineend = "round", linejoin = "round") +
  scale_y_continuous(minor_breaks = NULL) +
  scale_x_continuous(minor_breaks = NULL, breaks = seq(xmin, xmax, 20)) +
  scale_fill_manual(values = c("grey50", "#0075C4", "#BA2D0B")) +
  scale_colour_manual(values = c("grey50", "#0075C4", "#BA2D0B")) +
  theme_classic() +
  theme(legend.position = "none",
        axis.text = element_text(colour = "black"),
        axis.text.x = element_blank(),
        axis.title = element_blank(),
        axis.ticks.x = element_blank(),
        strip.text = element_text(colour = "black"),
        strip.background = element_rect(linewidth = 0.5),
        panel.background = element_rect(linewidth = 0.5, colour = "black"),
        plot.margin = margin(b = 5, t = 5))

repro_data <- plot_data |>
  filter(grepl(pattern = "product", x = Model))

repro_panel <- ggplot() +
  geom_ribbon(data = repro_data,
              aes(y = pointp , x = focal_var, col = rank_category2,
                  ymin = low, ymax = up), alpha = 0.3, linetype = "dotted", fill = NA, linewidth = 0.5) +
  geom_line(data = repro_data,
            aes(y = pointp , x = focal_var, col = rank_category2),
            linewidth = 1) +
  geom_text(data = repro_data |>
              group_by(Model, rank_category2) |>
              slice(n() - 1) |>
              ungroup(),
            aes(x = focal_var, y = pointp + c(-0.0035, 0.004,
                                              -0.0035, 0.004), label = rank_category2,
                colour = rank_category2),
            angle = c(-3, 4.5,
                      -7, -7), hjust = 0.5, size = 2) +
  facet_wrap(~ Model, scales = "free_y") +
  labs(x = "Number of individuals in the clan", y = "Predicted probability of event",
       colour = "Social rank", fill = "Social rank") +
  coord_cartesian(clip = "off",
                  expand = FALSE,
                  ylim = c(repro_ymin, repro_ymax),
                  xlim = c(xmin, xmax)) +
  annotate(geom = "segment",
           y = repro_ymin, yend = repro_ymin-(repro_ymax-repro_ymin)*bigtick_scale,
           x = seq(xmin, xmax, by = 20),
           xend = seq(xmin, xmax, by = 20),
           lineend = "round", linejoin = "round") +
  annotate(geom = "segment",
           y = repro_ymin, yend = repro_ymin-(repro_ymax-repro_ymin)*smalltick_scale,
           x = seq(xmin, xmax, by = 5),
           xend = seq(xmin, xmax, by = 5),
           lineend = "round", linejoin = "round") +
  scale_y_continuous(minor_breaks = NULL, breaks = seq(0, 0.125, by = 0.02)) +
  scale_x_continuous(minor_breaks = NULL, breaks = seq(xmin, xmax, 20)) +
  scale_fill_manual(values = c("#0075C4", "#BA2D0B")) +
  scale_colour_manual(values = c("#0075C4", "#BA2D0B")) +
  theme_classic() +
  theme(legend.position = "none",
        axis.text = element_text(colour = "black"),
        axis.text.x = element_text(margin = margin(t = 4)),
        axis.title = element_blank(),
        axis.ticks.x = element_blank(),
        strip.text = element_text(colour = "black"),
        strip.background = element_rect(linewidth = 0.5),
        panel.background = element_rect(linewidth = 0.5, colour = "black"))

twin_data <- plot_data |>
  filter(grepl(pattern = "winning", x = Model))

twin_panel <- ggplot() +
  geom_ribbon(data = twin_data,
              aes(y = pointp , x = focal_var, col = rank_category2,
                  ymin = low, ymax = up), alpha = 0.3, linetype = "dotted", fill = NA, linewidth = 0.5) +
  geom_line(data = twin_data,
            aes(y = pointp , x = focal_var, col = rank_category2),
            linewidth = 1) +
  geom_text(data = twin_data |>
              group_by(Model, rank_category2) |>
              slice(n() - 1) |>
              ungroup(),
            aes(x = focal_var, y = pointp + c(-0.025, 0.025), label = rank_category2,
                colour = rank_category2),
            angle = c(-5, -6.5), hjust = 0.5, size = 2) +
  facet_wrap(~ Model, scales = "free_y") +
  labs(x = "Number of individuals in the clan", y = "Predicted probability of event",
       colour = "Social rank", fill = "Social rank") +
  coord_cartesian(clip = "off",
                  expand = FALSE,
                  ylim = c(twin_ymin, twin_ymax),
                  xlim = c(xmin, xmax)) +
  annotate(geom = "segment",
           y = twin_ymin, yend = twin_ymin-(twin_ymax-twin_ymin)*bigtick_scale,
           x = seq(xmin, xmax, by = 20),
           xend = seq(xmin, xmax, by = 20),
           lineend = "round", linejoin = "round") +
  annotate(geom = "segment",
           y = twin_ymin, yend = twin_ymin-(twin_ymax-twin_ymin)*smalltick_scale,
           x = seq(xmin, xmax, by = 5),
           xend = seq(xmin, xmax, by = 5),
           lineend = "round", linejoin = "round") +
  scale_y_continuous(breaks = seq(0, 0.8, 0.2),
                     labels = format(seq(0, 0.8, 0.2), nsmall = 2)) +
  scale_x_continuous(breaks = seq(xmin, xmax, 20)) +
  scale_fill_manual(values = c("#0075C4", "#BA2D0B")) +
  scale_colour_manual(values = c("#0075C4", "#BA2D0B")) +
  theme_classic() +
  theme(legend.position = "none",
        axis.text = element_text(colour = "black"),
        axis.text.x = element_text(margin = margin(t = 4)),
        axis.title = element_blank(),
        axis.ticks.x = element_blank(),
        strip.text = element_text(colour = "black"),
        strip.background = element_rect(linewidth = 0.5),
        panel.background = element_rect(linewidth = 0.5, colour = "black"),
        plot.margin = margin(b = 5, t = 5))

library(patchwork)

output <- surv_panel/((repro_panel|twin_panel) + plot_layout(widths = c(2.2, 1)))

ggsave(output, filename = here::here("./plots/model_DD_plot.png"), dpi = 600,
       width = 8, height = 5)

### FACETTED FOR ALL RANK GROUPS AND MODELS

plot_data <- all_pdep |>
  filter(!(Model == "c) Male survival\n(adult)" & rank_category2 %in% c("top", "bottom"))) |>
  mutate(rank_category2 = case_when(Model == "c) Male survival\n(adult)" ~ "all adults",
                                    TRUE ~ rank_category2),
         rank_category2 = forcats::fct_relevel(rank_category2, "top", "middle", "bottom"))

surv_data <- plot_data |>
  filter(grepl(pattern = "survival", x = Model)) |>
  mutate(Model = forcats::fct_relevel(Model, "a) Female survival", "c) Male survival\n(adult)", "b) Male survival\n(juvenile)"))

surv_panel_facets <- ggplot() +
  geom_ribbon(data = surv_data,
              aes(y = pointp , x = focal_var, col = rank_category2,
                  ymin = low, ymax = up), alpha = 0.3, linetype = "dotted", fill = NA, linewidth = 0.5) +
  geom_line(data = surv_data,
            aes(y = pointp , x = focal_var, col = rank_category2),
            linewidth = 1) +
  facet_wrap(~ Model + rank_category2, scales = "free_y", ncol = 4) +
  labs(x = "Number of individuals in the clan", y = "Predicted probability of event",
       colour = "Social rank", fill = "Social rank") +
  coord_cartesian(clip = "off",
                  expand = FALSE,
                  ylim = c(surv_ymin, surv_ymax),
                  xlim = c(xmin, xmax)) +
  annotate(geom = "segment",
           y = surv_ymin, yend = surv_ymin-(surv_ymax-surv_ymin)*bigtick_scale,
           x = seq(xmin, xmax, by = 20),
           xend = seq(xmin, xmax, by = 20),
           lineend = "round", linejoin = "round") +
  annotate(geom = "segment",
           y = surv_ymin, yend = surv_ymin-(surv_ymax-surv_ymin)*smalltick_scale,
           x = seq(xmin, xmax, by = 5),
           xend = seq(xmin, xmax, by = 5),
           lineend = "round", linejoin = "round") +
  scale_y_continuous(minor_breaks = NULL) +
  scale_x_continuous(minor_breaks = NULL, breaks = seq(xmin, xmax, 20)) +
  scale_fill_manual(values = c("#0075C4", "#00783C", "#BA2D0B", "grey50")) +
  scale_colour_manual(values = c("#0075C4", "#00783C", "#BA2D0B", "grey50")) +
  theme_classic() +
  theme(legend.position = "none",
        axis.text = element_text(colour = "black"),
        axis.text.x = element_text(margin = margin(t = 4)),
        axis.title = element_blank(),
        axis.ticks.x = element_blank(),
        strip.text = element_text(colour = "black"),
        strip.background = element_rect(linewidth = 0.5),
        panel.spacing = unit(0.5, "cm"),
        panel.background = element_rect(linewidth = 0.5, colour = "black"),
        plot.margin = margin(b = 5, t = 5, r = 5))

ggsave(surv_panel_facets, filename = here::here("./plots/model_DD_plot_survfacets.png"), dpi = 600,
       width = 8, height = 5)

repro_data <- plot_data |>
  filter(grepl(pattern = "product", x = Model)) |>
  mutate(rank_category2 = forcats::fct_relevel(rank_category2, "top", "middle", "bottom"))

repro_panel_facets <- ggplot() +
  geom_ribbon(data = repro_data,
              aes(y = pointp , x = focal_var, col = rank_category2,
                  ymin = low, ymax = up), alpha = 0.3, linetype = "dotted", fill = NA, linewidth = 0.5) +
  geom_line(data = repro_data,
            aes(y = pointp , x = focal_var, col = rank_category2),
            linewidth = 1) +
facet_wrap(~ Model + rank_category2, scales = "free_y", ncol = 3) +
  labs(x = "Number of individuals in the clan", y = "Predicted probability of event",
       colour = "Social rank", fill = "Social rank") +
  coord_cartesian(clip = "off",
                  expand = FALSE,
                  ylim = c(repro_ymin, repro_ymax),
                  xlim = c(xmin, xmax)) +
  annotate(geom = "segment",
           y = repro_ymin, yend = repro_ymin-(repro_ymax-repro_ymin)*bigtick_scale,
           x = seq(xmin, xmax, by = 20),
           xend = seq(xmin, xmax, by = 20),
           lineend = "round", linejoin = "round") +
  annotate(geom = "segment",
           y = repro_ymin, yend = repro_ymin-(repro_ymax-repro_ymin)*smalltick_scale,
           x = seq(xmin, xmax, by = 5),
           xend = seq(xmin, xmax, by = 5),
           lineend = "round", linejoin = "round") +
  scale_y_continuous(breaks = seq(0, 0.12, 0.02),
                     labels = format(seq(0, 0.12, 0.02), nsmall = 2)) +
  scale_x_continuous(minor_breaks = NULL, breaks = seq(xmin, xmax, 20)) +
  scale_fill_manual(values = c("#0075C4", "#00783C", "#BA2D0B", "grey50")) +
  scale_colour_manual(values = c("#0075C4", "#00783C", "#BA2D0B", "grey50")) +
  theme_classic() +
  theme(legend.position = "none",
        axis.text = element_text(colour = "black"),
        axis.text.x = element_blank(),
        axis.title = element_blank(),
        axis.ticks.x = element_blank(),
        strip.text = element_text(colour = "black"),
        strip.background = element_rect(linewidth = 0.5),
        panel.spacing = unit(0.5, "cm"),
        panel.background = element_rect(linewidth = 0.5, colour = "black"),
        plot.margin = margin(b = 5, t = 5, r = 5))

twin_data <- plot_data |>
  filter(grepl(pattern = "winn", x = Model)) |>
  mutate(rank_category2 = forcats::fct_relevel(rank_category2, "top", "middle", "bottom"))

twin_panel_facets <- ggplot() +
  geom_ribbon(data = twin_data,
              aes(y = pointp , x = focal_var, col = rank_category2,
                  ymin = low, ymax = up), alpha = 0.3, linetype = "dotted", fill = NA, linewidth = 0.5) +
  geom_line(data = twin_data,
            aes(y = pointp , x = focal_var, col = rank_category2),
            linewidth = 1) +
  facet_wrap(~ Model + rank_category2, scales = "free_y", ncol = 3) +
  labs(x = "Number of individuals in the clan", y = "Predicted probability of event",
       colour = "Social rank", fill = "Social rank") +
  coord_cartesian(clip = "off",
                  expand = FALSE,
                  ylim = c(twin_ymin, twin_ymax),
                  xlim = c(xmin, xmax)) +
  annotate(geom = "segment",
           y = twin_ymin, yend = twin_ymin-(twin_ymax-twin_ymin)*bigtick_scale,
           x = seq(xmin, xmax, by = 20),
           xend = seq(xmin, xmax, by = 20),
           lineend = "round", linejoin = "round") +
  annotate(geom = "segment",
           y = twin_ymin, yend = twin_ymin-(twin_ymax-twin_ymin)*smalltick_scale,
           x = seq(xmin, xmax, by = 5),
           xend = seq(xmin, xmax, by = 5),
           lineend = "round", linejoin = "round") +
  scale_y_continuous(breaks = seq(0, 0.8, 0.2),
                     labels = format(seq(0, 0.8, 0.2), nsmall = 2)) +
  scale_x_continuous(minor_breaks = NULL, breaks = seq(xmin, xmax, 20)) +
  scale_fill_manual(values = c("#0075C4", "#00783C", "#BA2D0B", "grey50")) +
  scale_colour_manual(values = c("#0075C4", "#00783C", "#BA2D0B", "grey50")) +
  theme_classic() +
  theme(legend.position = "none",
        axis.text = element_text(colour = "black"),
        axis.text.x = element_text(margin = margin(t = 4)),
        axis.title = element_blank(),
        axis.ticks.x = element_blank(),
        strip.text = element_text(colour = "black"),
        strip.background = element_rect(linewidth = 0.5),
        panel.spacing = unit(0.5, "cm"),
        panel.background = element_rect(linewidth = 0.5, colour = "black"),
        plot.margin = margin(b = 5, t = 5, r = 5))

output <- repro_panel_facets/twin_panel_facets + plot_layout(heights = c(1, 0.35))

ggsave(output, filename = here::here("./plots/model_DD_plot_reprotwinfacets.png"), dpi = 600,
       width = 8, height = 7.5)


# Plot Odds Ratios (not shown) --------------------------------------------

full_table_num <- extract_table(model_list, rounded = FALSE)

full_table_num[grepl("size", full_table_num$Parameter), ] |>
  mutate(Rank = case_when(Model != "postdispM" & Parameter == "clan_size" ~ "bottom",
                          Model != "postdispM" & grepl("middle", Parameter) ~ "middle",
                          Model != "postdispM" & grepl("top", Parameter) ~ "top",
                          Model == "postdispM" ~ "all")) |>
  mutate(Estimate_full = case_when(length(Estimate) == 3 & Rank ==  "bottom" ~ Estimate,
                                   length(Estimate) == 3 & Rank ==  "middle" ~ Estimate[1] + Estimate[2],
                                   length(Estimate) == 3 & Rank ==  "top" ~ Estimate[1] + Estimate[3],
                                   length(Estimate) == 1 ~ Estimate),
         .by = "Model") |>
  mutate(OR20 = exp(Estimate_full*20), `1-OR20` = 1 - OR20) |>
  mutate(Model = case_match(Model,
                            "primirepro" ~ "first repro",
                            "nonprimirepro" ~ "other repro",
                            "twin" ~ "twinning",
                            "allF" ~ "survival (females)",
                            "predispM" ~ "survival (pre-disp males)",
                            "postdispM" ~ "survival (post-disp males)",
                            "disp" ~ "additional dispersion")) |>
  mutate(Model = forcats::fct_inorder(Model)) |>
  select(Model, Rank, OR20, `1-OR20`) -> DDtable

rownames(DDtable) <- NULL

ggplot(DDtable) +
  aes(y = `1-OR20`, x = Model, fill = Rank) +
  geom_col(position = "dodge2") +
  geom_hline(yintercept = 0) +
  labs(y = "Decrease in odds of event when clan size increased by 20 individuals",
       x = "Event") +
  theme_bw()


