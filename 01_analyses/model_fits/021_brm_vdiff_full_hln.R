basket_conf_data_exp2 <- osf_load_file(
  osf_datafolder$id[osf_datafolder$name == "basket_conf_data_exp2.csv"], 
  "basket_conf_data_exp2.csv", stringsAsFactors = F) # load file

basket_conf_data_exp3 <- osf_load_file(
  osf_datafolder$id[osf_datafolder$name == "basket_conf_data_exp3.csv"], 
  "basket_conf_data_exp3.csv", stringsAsFactors = F) # load file



basket_conf_data_exp2 <- subset(basket_conf_data_exp2, prereg_exclusion == 0)
basket_conf_data_exp3 <- subset(basket_conf_data_exp3, prereg_exclusion == 0)


basket_conf_data_exp2$confidence_01 <- scales::rescale(basket_conf_data_exp2$confidence, c(0, 1), c(0, 200)) 
basket_conf_data_exp3$confidence_01 <- scales::rescale(basket_conf_data_exp3$confidence, c(0, 1), c(0, 200)) 
basket_conf_data_exp2$evaluation_01 <- scales::rescale(basket_conf_data_exp2$evaluation, c(0, 1), c(0, 200)) 
basket_conf_data_exp3$evaluation_01 <- scales::rescale(basket_conf_data_exp3$evaluation, c(0, 1), c(0, 7500)) 

basket_conf_data_exp2$basket_value_01 <- scales::rescale(basket_conf_data_exp2$basket_value, c(0, 1), c(0, 200))

basket_conf_data_exp3$item1_value_price <- scales::rescale(basket_conf_data_exp3$item1_value, c(0, 2500), c(0, 50))
basket_conf_data_exp3$item2_value_price <- scales::rescale(basket_conf_data_exp3$item2_value, c(0, 2500), c(0, 50))
basket_conf_data_exp3$item3_value_price <- scales::rescale(basket_conf_data_exp3$item3_value, c(0, 2500), c(0, 50))
for(i in 1:nrow(basket_conf_data_exp3)){
  basket_conf_data_exp3$basket_value_price[i] <- sum(c(basket_conf_data_exp3$item1_value_price[i], basket_conf_data_exp3$item2_value_price[i], basket_conf_data_exp3$item3_value_price[i]))
}

basket_conf_data_exp3$basket_value_01 <- scales::rescale(basket_conf_data_exp3$basket_value_price, c(0, 1), c(0, 7500))

basket_conf_data_exp2$experiment <- rep("exp2")
basket_conf_data_exp3$experiment <- rep("exp3")
basket_conf_data_exp3$subject_nr <- basket_conf_data_exp3$subject_nr + 1000

basket_conf_data_exp3$value_diff <- abs(basket_conf_data_exp3$basket_value_01 - basket_conf_data_exp3$evaluation_01)
basket_conf_data_exp3$basket_cat_f <- factor(basket_conf_data_exp3$basket_cat, levels = c("high", "low"))
contrasts(basket_conf_data_exp3$basket_cat_f) <- contr.sum(2)

names(basket_conf_data_exp2)[which(colnames(basket_conf_data_exp2) == "subject")] <- "subject_nr"

basket_conf_data <- rbind(basket_conf_data_exp2[, which(colnames(basket_conf_data_exp2) %in% c("subject_nr", "basket_cat", "confidence_01", "evaluation_01", "basket_value_01", "experiment", "stim_name"))],
basket_conf_data_exp3[, which(colnames(basket_conf_data_exp3) %in% c("subject_nr", "basket_cat", "confidence_01", "evaluation_01", "basket_value_01", "experiment", "stim_name"))])

basket_conf_data$basket_cat_f <- factor(basket_conf_data$basket_cat, levels = c("high", "low"))
# basket_conf_data$basket_matching_var_f <- factor(basket_conf_data$basket_matching_var)
contrasts(basket_conf_data$basket_cat_f) <- contr.sum(2)


basket_conf_data$value_diff <- abs(basket_conf_data$basket_value_01 - basket_conf_data$evaluation_01)
basket_conf_data$value_diff_01 <- abs(basket_conf_data$basket_value_01 - basket_conf_data$evaluation_01)

bf_vdiff_full_hln <- bf(value_diff ~ basket_cat_f*experiment
           + (1 + basket_cat_f | subject_nr)
           + (1 | stim_name),
           hu ~ basket_cat_f*experiment + (1 + basket_cat_f | subject_nr)
           )

brm_vdiff_comb <- brm(bf_vdiff_full_hln
           , data = basket_conf_data
           , family = hurdle_lognormal()
           , chains = 4, cores = 4, warmup = 2000, iter = 4000 
           , backend = "cmdstanr", threads = threading(3)
           , seed = 1, control = list(adapt_delta = .95)
           , file = here::here("01_analyses/fitted_models/021_brm_vdiff_full_hln.rds")
           , file_refit = "on_change"
           )
