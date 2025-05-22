osf_node <- osf_retrieve_node("9y38n") # retrieve OSF node
osf_datafolder <- osf_ls_files(osf_node, path = "processed_data", n_max = Inf) # get data folder

basket_conf_data <- osf_load_file(
  osf_datafolder$id[osf_datafolder$name == "basket_conf_data_exp4.csv"], 
  "basket_conf_data_exp4.csv", stringsAsFactors = F) # load file


basket_conf_data <- subset(basket_conf_data, prereg_exclusion == 0)


basket_conf_data$confidence_01 <- scales::rescale(basket_conf_data$confidence, c(0, 1), c(0, 200)) 
basket_conf_data$evaluation_01 <- scales::rescale(basket_conf_data$evaluation, c(0, 1), c(0, 7500)) 

basket_conf_data$basket_value_price <- scales::rescale(basket_conf_data$basket_value, c(0, 75), c(0, 50)) 
basket_conf_data$basket_value_price_ex <- scales::rescale(basket_conf_data$basket_value_ex, c(0, 37.5), c(0, 25)) 

basket_conf_data$basket_cat[which(basket_conf_data$basket_cat == "low" &basket_conf_data$basket_matching_var == "evaluation_ex" )] <- "high_conf"
basket_conf_data$basket_cat[which(basket_conf_data$basket_cat == "high" & basket_conf_data$basket_matching_var == "evaluation_ex" )] <- "low_conf"
basket_conf_data$basket_cat[which(basket_conf_data$basket_cat == "high_conf" & basket_conf_data$basket_matching_var == "evaluation_ex" )] <- "high"
basket_conf_data$basket_cat[which(basket_conf_data$basket_cat == "low_conf"& basket_conf_data$basket_matching_var == "evaluation_ex" )] <- "low"


basket_conf_data$basket_matching_var_f <- factor(basket_conf_data$basket_matching_var)
basket_conf_data$basket_cat_f <- factor(basket_conf_data$basket_cat)
contrasts(basket_conf_data$basket_cat_f) <- contr.sum(2)
contrasts(basket_conf_data$basket_matching_var_f) <- contr.sum(2)

basket_conf_data <- standardize_vars(basket_conf_data, c("basket_value", "item_value_sd", "basket_conf", "basket_value_ex", "value_sd"))

basket_conf_data_sd <- subset(basket_conf_data, basket_matching_var == "rating_sd")
basket_conf_data_sd$basket_value_s <- scale(basket_conf_data_sd$basket_value, scale = T)
basket_conf_data_sd$basket_value_ex_s <- scale(basket_conf_data_sd$basket_value_ex, scale = T)
basket_conf_data_sd$value_sd_s <- scale(basket_conf_data_sd$value_sd, scale = T)


bf_val_check_sdmatch <- bf(basket_value_price_ex ~ basket_cat_f 
           + (1 + basket_cat_f | subject_nr))

brm_val_exp4_cons <- brm(bf_val_check_sdmatch
           , data = basket_conf_data_sd
           , family = gaussian()
           , chains = 4, cores = 4, warmup = 1000, iter = 2000
           , backend = "cmdstanr", threads = threading(3)
           , seed = 1
           , file = here::here("01_analyses/fitted_models/022_brm_val_check_sdmatch_e4.rds")
           , file_refit = "on_change"
           )
