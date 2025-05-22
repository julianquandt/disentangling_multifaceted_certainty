osf_node <- osf_retrieve_node("9y38n") # retrieve OSF node
osf_datafolder <- osf_ls_files(osf_node, path = "processed_data", n_max = Inf) # get data folder

basket_conf_data <- osf_load_file(
  osf_datafolder$id[osf_datafolder$name == "basket_conf_data_exp2.csv"], 
  "basket_conf_data_exp2.csv", stringsAsFactors = F) # load file

basket_conf_data <- subset(basket_conf_data, prereg_exclusion == 0)

basket_conf_data$basket_cat_f <- factor(basket_conf_data$basket_cat, levels = c("high", "low"))
contrasts(basket_conf_data$basket_cat_f) <- contr.sum(2)

basket_conf_data$basket_cat <- ifelse(basket_conf_data$basket_cat == "low", -1, 1)

for(i in 1:nrow(basket_conf_data)){
  basket_conf_data$item_value_sd[i] <- sd(c(basket_conf_data$item1_value[i], basket_conf_data$item2_value[i], basket_conf_data$item3_value[i]))
  basket_conf_data$basket_value_ex[i] <- abs(basket_conf_data$basket_value[i]-100)
}
basket_conf_data$confidence_01 <- scales::rescale(basket_conf_data$confidence, c(0, 1), c(0, 200)) 
basket_conf_data$evaluation_01 <- scales::rescale(basket_conf_data$evaluation, c(0, 1), c(0, 200)) 
  
basket_conf_data <- standardize_vars(basket_conf_data, c("basket_value", "item_value_sd", "basket_conf", "basket_value_ex"))

bf_brm5_e2h1_BB <- bf(confidence | trials(200) ~ basket_cat_f + basket_value_s + item_value_sd_s  + basket_value_ex_s
                      + (1 +  basket_cat_f + basket_value_s + item_value_sd_s  + basket_value_ex_s | subject) 
                      + (1 | stim_name))

brm5_e2h1_BB <- brm(bf_brm5_e2h1_BB
                    , data = basket_conf_data
                    , family = beta_binomial()
                    , chains = 4, cores = 4, warmup = 2000, iter = 4000
                    , backend = "cmdstanr", threads = threading(3)
                    , seed = 1
                    , file = here::here("01_analyses/fitted_models/003_brm5_e2h1_bb.rds")
                    , file_refit = "on_change"
           )
