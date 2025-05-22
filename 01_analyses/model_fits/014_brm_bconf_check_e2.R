basket_conf_data <- osf_load_file(
  osf_datafolder$id[osf_datafolder$name == "basket_conf_data_exp2.csv"],
  "basket_conf_data_exp2.csv",
  stringsAsFactors = F
) # load file

basket_conf_data <- subset(basket_conf_data, prereg_exclusion == 0)



length(unique(basket_conf_data$subject))

basket_conf_data$basket_cat_f <- factor(basket_conf_data$basket_cat, levels = c("high", "low"))
contrasts(basket_conf_data$basket_cat_f) <- contr.sum(2)

basket_conf_data$basket_cat <- ifelse(basket_conf_data$basket_cat == "low", -1, 1)

for (i in 1:nrow(basket_conf_data)) {
  basket_conf_data$item_value_sd[i] <- sd(c(basket_conf_data$item1_value[i], basket_conf_data$item2_value[i], basket_conf_data$item3_value[i]))
  basket_conf_data$basket_value_ex[i] <- abs(basket_conf_data$basket_value[i] - 100)
}
basket_conf_data$confidence_01 <- scales::rescale(basket_conf_data$confidence, c(0, 1), c(0, 200))
basket_conf_data$evaluation_01 <- scales::rescale(basket_conf_data$evaluation, c(0, 1), c(0, 200))

basket_conf_data <- standardize_vars(basket_conf_data, c("basket_value", "item_value_sd", "basket_conf", "basket_value_ex"))



bf_bconf_check <- bf(basket_conf ~ basket_cat_f
  + (1 + basket_cat_f | subject))

brm_bconf_exp2 <- brm(bf_bconf_check,
  data = basket_conf_data,
  family = gaussian(),
  chains = 4, cores = 4, warmup = 1000, iter = 2000,
  backend = "cmdstanr", threads = threading(2),
  seed = 1,
  file = here::here("01_analyses/fitted_models/014_brm_bconf_check_e2.rds"),
  file_refit = "on_change"
)
