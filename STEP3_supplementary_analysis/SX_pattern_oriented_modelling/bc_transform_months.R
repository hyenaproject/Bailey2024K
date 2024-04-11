my_boxcox <- function(x, lambda, lambda2 = 0.01){
  case_when(lambda == 0 ~ log(x + lambda2),
            lambda != 0 ~ (x + lambda2)^lambda-1/lambda)
  }

loglik <- purrr::map(.x = seq(-2, 2, 0.25),
                     .f = function(i){
                       
                       moddata <- model_data$F_repro_nonprimi %>%
                         dplyr::filter(year >= start_yr & year <= end_yr) %>% 
                         mutate(monthsbc = my_boxcox(months, lambda = i))
                       
                       mod <- spaMM::fitme(repro ~ monthsbc*rank_category + monthsbc*age +
                                             clan_size*rank_category + after1y_effort_all + start_clan + (1|start_clan:year),
                                           data = moddata,
                                           family = binomial(link = "probit"), method = "PQL/L")
                       
                       data.frame(LL = logLik(mod),
                                  exp = i)
                       
                     }) %>% 
  purrr::list_rbind()

ggplot() +
  geom_line(data = loglik,
            aes(x = exp, y = LL))


modbc <- spaMM::fitme(repro ~ my_boxcox(months, 0)*rank_category + my_boxcox(months, 0)*age +
                        clan_size*rank_category + after1y_effort_all + start_clan + (1|start_clan:year),
                      data = model_data$F_repro_nonprimi %>%
                        dplyr::filter(year >= start_yr & year <= end_yr),
                      family = binomial(link = "probit"), method = "PQL/L")

mod1 <- spaMM::fitme(repro ~ months*rank_category + months*age +
                       clan_size*rank_category + after1y_effort_all + start_clan + (1|start_clan:year),
                     data = model_data$F_repro_nonprimi %>%
                       dplyr::filter(year >= start_yr & year <= end_yr),
                     family = binomial(link = "probit"), method = "PQL/L")

ggplot() +
  geom_line(aes(x = 0:240,
                y = as.numeric(predict(modbc, 
                                       newdata = data.frame(months = 0:240,
                                                            age = 120,
                                                            rank_category = "top5",
                                                            clan_size = 40,
                                                            after1y_effort_all = 1,
                                                            start_clan = "A"),
                                       re.form = NA))),
            colour = "red") +
  geom_line(aes(x = 0:240,
                y = as.numeric(predict(mod1, 
                                       newdata = data.frame(months = 0:240,
                                                            age = 120,
                                                            rank_category = "top5",
                                                            clan_size = 40,
                                                            after1y_effort_all = 1,
                                                            start_clan = "A"),
                                       re.form = NA))),
            colour = "blue")


