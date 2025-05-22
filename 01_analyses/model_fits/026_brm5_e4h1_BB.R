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


bf_brm5_e4h1_BB <- bf(confidence | trials(200) ~  +  basket_cat_f*basket_matching_var_f
                      + (1 +  basket_cat_f*basket_matching_var_f | subject_nr))


brm5_e4h1_BB <- brm(bf_brm5_e4h1_BB
                    , data = basket_conf_data
                    , family = beta_binomial()
                    , chains = 4, cores = 4, warmup = 2000, iter = 4000
                    , backend = "cmdstanr", threads = threading(3)
                    , seed = 1
                    , file = here::here("01_analyses/fitted_models/026_brm5_e4h1_BB.rds")
                    , file_refit = "on_change"
           )