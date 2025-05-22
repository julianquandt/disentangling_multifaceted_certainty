osf_node <- osf_retrieve_node("9y38n") # retrieve OSF node
osf_datafolder <- osf_ls_files(osf_node, path = "processed_data", n_max = Inf) # get data folder

basket_conf_data_exp2 <- osf_load_file(
  osf_datafolder$id[osf_datafolder$name == "basket_conf_data_exp2.csv"], 
  "basket_conf_data_exp2.csv", stringsAsFactors = F) # load file
basket_conf_data_exp2 <- subset(basket_conf_data_exp2, prereg_exclusion == 0) # exclude participants that were excluded in the preregistration

basket_conf_data_exp2$subject_nr <- basket_conf_data_exp2$subject
for(i in 1:nrow(basket_conf_data_exp2)){
  basket_conf_data_exp2$item_value_sd[i] <- sd(c(basket_conf_data_exp2$item1_value[i], basket_conf_data_exp2$item2_value[i], basket_conf_data_exp2$item3_value[i]))
  basket_conf_data_exp2$basket_value_ex[i] <- abs(mean(c(basket_conf_data_exp2$item1_value[i]-100, basket_conf_data_exp2$item2_value[i]-100, basket_conf_data_exp2$item3_value[i]-100)))
}

basket_conf_data_exp2_diff <- NULL
for (i in unique(basket_conf_data_exp2$subject_nr)){
  d_sub <- subset(basket_conf_data_exp2, subject_nr == i)
  d_sub_diff <- NULL
  for(k in unique(d_sub$basket_cluster)){
    d_cluster <- subset(d_sub, basket_cluster == k)
    if(nrow(d_cluster)< 2){
      next
    }
    # average the 3 rows per basket_nr and keep basket_cat in the new data frame
    d_cluster <- aggregate(d_cluster[,c("confidence", "evaluation", "basket_value", "value_sd", "basket_value_ex", "confidence_mean")], by = list(d_cluster$basket_cat), FUN = mean)
    names(d_cluster)[1] <- "basket_cat"
    d_cluster$subject_nr <- i

    if(nrow(d_cluster) > 0){
      outcome_confidence_diff <- d_cluster$confidence[which(d_cluster$basket_cat == "high")] - d_cluster$confidence[which(d_cluster$basket_cat == "low")]
      outcome_eval_diff <- d_cluster$evaluation[which(d_cluster$basket_cat == "high")] - d_cluster$evaluation[which(d_cluster$basket_cat == "low")]
      basket_conf_diff <- d_cluster$confidence_mean[which(d_cluster$basket_cat == "high")] - d_cluster$confidence_mean[which(d_cluster$basket_cat == "low")]
      basket_value_diff <- d_cluster$basket_value[which(d_cluster$basket_cat == "high")] - d_cluster$basket_value[which(d_cluster$basket_cat == "low")]
      basket_value_sd_diff <- d_cluster$value_sd[which(d_cluster$basket_cat == "high")] - d_cluster$value_sd[which(d_cluster$basket_cat == "low")]
      basket_value_ex_diff <- d_cluster$basket_value_ex[which(d_cluster$basket_cat == "high")] - d_cluster$basket_value_ex[which(d_cluster$basket_cat == "low")]
      d_sub_diff <- rbind(d_sub_diff, data.frame(subject_nr = i, basket_cluster = k, confidence_diff = outcome_confidence_diff, evaluation_diff = outcome_eval_diff, basket_value_diff = basket_value_diff, basket_conf_diff = basket_conf_diff, basket_value_ex_diff = basket_value_ex_diff, basket_value_sd_diff = basket_value_sd_diff, basket_matching_var = "evaluation"))
    }
  }
  basket_conf_data_exp2_diff <- rbind(basket_conf_data_exp2_diff, d_sub_diff)
}
basket_conf_data_exp2_diff <- standardize_vars(basket_conf_data_exp2_diff, vars = c("basket_value_diff", "basket_conf_diff", "basket_value_ex_diff", "basket_value_sd_diff"))

# fit model
bf_diff_exp2 <- bf(confidence_diff ~ 1 + basket_value_ex_diff_s + basket_conf_diff_s + basket_value_sd_diff_s + (1 +  basket_value_ex_diff_s + basket_conf_diff_s + basket_value_sd_diff_s | subject_nr))

brm_diff_exp2_student <- brm(
  bf_diff_exp2,
  data = basket_conf_data_exp2_diff,
  family = student(),
  backend = "cmdstanr",
  iter = 4000,
  warmup = 2000,
  chains = 4,
  cores = 4,
  threads = threading(3),
  file = here::here("01_analyses/fitted_models/010_brm_diff_exp2_student.rds"),
  file_refit = "on_change"
)
