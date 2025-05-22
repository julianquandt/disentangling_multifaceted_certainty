
basket_conf_data <- osf_load_file(
  osf_datafolder$id[osf_datafolder$name == "basket_conf_data_exp3.csv"], 
  "basket_conf_data_exp3.csv", stringsAsFactors = F) # load file

basket_conf_data <- subset(basket_conf_data, prereg_exclusion == 0)

basket_conf_data$confidence_01 <- scales::rescale(basket_conf_data$confidence, c(0, 1), c(0, 200)) 
basket_conf_data$evaluation_01 <- scales::rescale(basket_conf_data$evaluation, c(0, 1), c(0, 7500)) 

basket_conf_data$basket_value_price <- scales::rescale(basket_conf_data$basket_value, c(0, 75), c(0, 50)) 
basket_conf_data$basket_value_price_ex <- scales::rescale(basket_conf_data$basket_value_ex, c(0, 37.5), c(0, 25)) 

basket_conf_data$basket_cat_f <- factor(basket_conf_data$basket_cat, levels = c("high", "low"))
basket_conf_data$basket_matching_var_f <- factor(basket_conf_data$basket_matching_var)
contrasts(basket_conf_data$basket_cat_f) <- contr.sum(2)

basket_conf_data <- standardize_vars(basket_conf_data, c("basket_value", "item_value_sd", "basket_conf", "basket_value_ex", "value_sd"))

basket_conf_data$evaluation_price <- basket_conf_data$evaluation/100
basket_conf_data$value_diff <- abs(basket_conf_data$basket_value_price - basket_conf_data$evaluation_price)

bf_vdiff_hln_full <- bf(value_diff ~ basket_cat_f
               + (1 + basket_cat_f | subject_nr) + (1 | stim_name),
               hu ~ basket_cat_f  + (1 + basket_cat_f | subject_nr)) 

brm_vdiff_exp3 <- brm(bf_vdiff_hln_full
           , data = basket_conf_data
           , family = hurdle_lognormal()
           , chains = 4, cores = 4, warmup = 2000, iter = 4000 
           , backend = "cmdstanr", threads = threading(3)
           , seed = 1, control = list(adapt_delta = .95)
           , file = here::here("01_analyses/fitted_models/020_brm_vdiff_hln_exp3_full.rds")
           , file_refit = "on_change"
           )