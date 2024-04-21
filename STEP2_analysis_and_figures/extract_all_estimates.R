library(dplyr)
library(ggplot2)
library(spaMM)

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

model_list <- readRDS("model_list.RDS")
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

all_pdep <-  rbind(cbind(Model = "a) Monthly reproduction for primiparous female", primirepro_pdep),
                   cbind(Model = "b) Monthly reproduction for non-primiparous", nonprimirepro_pdep),
                   cbind(Model = "c) Twinning outcome", twin_pdep),
                   cbind(Model = "d) Monthly survival for female", allF_pdep),
                   cbind(Model = "e) Monthly survival for young males", predispM_pdep),
                   cbind(Model = "f) Monthly survival for adult males", postdispM_pdep))

all_pdep$Model <- forcats::fct_inorder(all_pdep$Model)

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


