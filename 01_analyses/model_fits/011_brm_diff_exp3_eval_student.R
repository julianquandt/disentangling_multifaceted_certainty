osf_node <- osf_retrieve_node("9y38n") # retrieve OSF node
osf_datafolder <- osf_ls_files(osf_node, path = "processed_data", n_max = Inf) # get data folder

basket_conf_data_exp3 <- osf_load_file(
  osf_datafolder$id[osf_datafolder$name == "basket_conf_data_exp3.csv"], 
  "basket_conf_data_exp3.csv", stringsAsFactors = F) # load file
basket_conf_data_exp3 <- subset(basket_conf_data_exp3, prereg_exclusion == 0) # exclude participants that were excluded in the preregistration
basket_conf_data_exp3_diff <- NULL
for (i in unique(basket_conf_data_exp3$subject_nr)){
  d_sub <- subset(basket_conf_data_exp3, subject_nr == i)
  d_sub_diff <- NULL
  for(k in unique(d_sub$basket_cluster)){
    d_cluster <- subset(d_sub, basket_cluster == k)
    if(nrow(d_cluster)< 2){
      next
    }
    d_cluster_ex <- subset(d_cluster, basket_matching_var == "evaluation_ex")
    d_cluster_eval <- subset(d_cluster, basket_matching_var == "evaluation")



    if(nrow(d_cluster_ex) > 0){
      basket_value_diff_ex <- d_cluster_ex$basket_value[which(d_cluster_ex$basket_cat == "high")] - d_cluster_ex$basket_value[which(d_cluster_ex$basket_cat == "low")]
      outcome_conf_diff_ex <- d_cluster_ex$confidence[which(d_cluster_ex$basket_cat == "high")] - d_cluster_ex$confidence[which(d_cluster_ex$basket_cat == "low")]
      outcome_eval_diff_ex <- d_cluster_ex$evaluation[which(d_cluster_ex$basket_cat == "high")] - d_cluster_ex$evaluation[which(d_cluster_ex$basket_cat == "low")]
      basket_value_sd_diff_ex <- d_cluster_ex$value_sd[which(d_cluster_ex$basket_cat == "high")] - d_cluster_ex$value_sd[which(d_cluster_ex$basket_cat == "low")]
      basket_value_ex_diff_ex <- d_cluster_ex$basket_value_ex[which(d_cluster_ex$basket_cat == "high")] - d_cluster_ex$basket_value_ex[which(d_cluster_ex$basket_cat == "low")]
      basket_conf_diff_ex <- d_cluster_ex$basket_conf[which(d_cluster_ex$basket_cat == "high")] - d_cluster_ex$basket_conf[which(d_cluster_ex$basket_cat == "low")]
      d_sub_diff <- rbind(d_sub_diff, data.frame(subject_nr = i, basket_cluster = k, confidence_diff = outcome_conf_diff_ex, evaluation_diff = outcome_eval_diff_ex, basket_value_diff = basket_value_diff_ex, basket_value_ex_diff = basket_value_ex_diff_ex, basket_value_sd_diff = basket_value_sd_diff_ex, basket_conf_diff = basket_conf_diff_ex, basket_matching_var = "evaluation_ex"))
    }

    if(nrow(d_cluster_eval) > 0){
      outcome_conf_diff_eval <- d_cluster_eval$confidence[which(d_cluster_eval$basket_cat == "high")] - d_cluster_eval$confidence[which(d_cluster_eval$basket_cat == "low")]
      outcome_eval_diff_eval <- d_cluster_eval$evaluation[which(d_cluster_eval$basket_cat == "high")] - d_cluster_eval$evaluation[which(d_cluster_eval$basket_cat == "low")]
      basket_value_diff_eval <- d_cluster_eval$basket_value[which(d_cluster_eval$basket_cat == "high")] - d_cluster_eval$basket_value[which(d_cluster_eval$basket_cat == "low")]
      basket_value_ex_diff_eval <- d_cluster_eval$basket_value_ex[which(d_cluster_eval$basket_cat == "high")] - d_cluster_eval$basket_value_ex[which(d_cluster_eval$basket_cat == "low")]
      basket_value_sd_diff_eval <- d_cluster_eval$value_sd[which(d_cluster_eval$basket_cat == "high")] - d_cluster_eval$value_sd[which(d_cluster_eval$basket_cat == "low")]
        basket_conf_diff_eval <- d_cluster_eval$basket_conf[which(d_cluster_eval$basket_cat == "high")] - d_cluster_eval$basket_conf[which(d_cluster_eval$basket_cat == "low")]
      d_sub_diff <- rbind(d_sub_diff, data.frame(subject_nr = i, basket_cluster = k, confidence_diff = outcome_conf_diff_eval, evaluation_diff = outcome_eval_diff_eval, basket_value_diff = basket_value_diff_eval, basket_value_ex_diff = basket_value_ex_diff_eval, basket_value_sd_diff = basket_value_sd_diff_eval, basket_conf_diff = basket_conf_diff_eval, basket_matching_var = "evaluation"))
    }
  }
  basket_conf_data_exp3_diff <- rbind(basket_conf_data_exp3_diff, d_sub_diff)
}

# subset based on matching condition
basket_conf_data_exp3_diff_eval <- subset(basket_conf_data_exp3_diff, basket_matching_var == "evaluation")
basket_conf_data_exp3_diff_eval <- standardize_vars(basket_conf_data_exp3_diff_eval, vars = c("basket_value_diff", "basket_conf_diff", "basket_value_ex_diff", "basket_value_sd_diff"))

# eval model
bf_diff_eval_exp3 <-bf(confidence_diff ~ 1 + basket_value_ex_diff_s + basket_conf_diff_s + basket_value_sd_diff_s + (1 +  basket_value_ex_diff_s + basket_conf_diff_s + basket_value_sd_diff_s | subject_nr))

brm_diff_exp3_eval_student <- brm(
  bf_diff_eval_exp3,
  data = basket_conf_data_exp3_diff_eval,
  family = student,
  backend = "cmdstan",
  iter = 4000,
  warmup = 2000,
  chains = 4,
  cores = 4,
  threads = threading(3),
  file = here::here("01_analyses/fitted_models/011_brm_diff_exp3_eval_student.rds"),
  file_refit = "on_change"
)
