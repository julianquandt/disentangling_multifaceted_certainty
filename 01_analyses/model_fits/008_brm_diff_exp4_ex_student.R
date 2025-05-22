osf_node <- osf_retrieve_node("9y38n") # retrieve OSF node
osf_datafolder <- osf_ls_files(osf_node, path = "processed_data", n_max = Inf) # get data folder

basket_conf_data_exp4 <- osf_load_file(
  osf_datafolder$id[osf_datafolder$name == "basket_conf_data_exp4.csv"], 
  "basket_conf_data_exp4.csv", stringsAsFactors = F) # load file
basket_conf_data_exp4 <- subset(basket_conf_data_exp4, prereg_exclusion == 0) # exclude participants that were excluded in the preregistration

basket_conf_data_exp4_diff <- NULL
for (i in unique(basket_conf_data_exp4$subject_nr)){
  d_sub <- subset(basket_conf_data_exp4, subject_nr == i)
  d_sub_diff <- NULL
  for(k in unique(d_sub$basket_cluster)){
    d_cluster <- subset(d_sub, basket_cluster == k)
    if(nrow(d_cluster)< 2){
      next
    }
    d_cluster_ex <- subset(d_cluster, basket_matching_var == "evaluation_ex")
    d_cluster_sd <- subset(d_cluster, basket_matching_var == "rating_sd")



    if(nrow(d_cluster_ex) > 0){
      outcome_conf_diff_ex <- d_cluster_ex$confidence[which(d_cluster_ex$basket_cat == "high")] - d_cluster_ex$confidence[which(d_cluster_ex$basket_cat == "low")]
      outcome_eval_diff_ex <- d_cluster_ex$evaluation[which(d_cluster_ex$basket_cat == "high")] - d_cluster_ex$evaluation[which(d_cluster_ex$basket_cat == "low")]
      basket_value_diff_ex <- d_cluster_ex$basket_value[which(d_cluster_ex$basket_cat == "high")] - d_cluster_ex$basket_value[which(d_cluster_ex$basket_cat == "low")]
      basket_value_sd_diff_ex <- d_cluster_ex$value_sd[which(d_cluster_ex$basket_cat == "high")] - d_cluster_ex$value_sd[which(d_cluster_ex$basket_cat == "low")]
      basket_value_ex_diff_ex <- d_cluster_ex$basket_value_ex[which(d_cluster_ex$basket_cat == "high")] - d_cluster_ex$basket_value_ex[which(d_cluster_ex$basket_cat == "low")]
      basket_rating_sd_diff_ex <- d_cluster_ex$basket_rating_sd[which(d_cluster_ex$basket_cat == "high")] - d_cluster_ex$basket_rating_sd[which(d_cluster_ex$basket_cat == "low")]
      d_sub_diff <- rbind(d_sub_diff, data.frame(subject_nr = i, basket_cluster = k, confidence_diff = outcome_conf_diff_ex, evaluation_diff = outcome_eval_diff_ex, basket_value_diff = basket_value_diff_ex, basket_value_ex_diff = basket_value_ex_diff_ex, basket_value_sd_diff = basket_value_sd_diff_ex, basket_rating_sd_diff = basket_rating_sd_diff_ex, basket_matching_var = "evaluation_ex"))
    }

    if(nrow(d_cluster_sd) > 0){
      outcome_conf_diff_sd <- d_cluster_sd$confidence[which(d_cluster_sd$basket_cat == "high")] - d_cluster_sd$confidence[which(d_cluster_sd$basket_cat == "low")]
      outcome_eval_diff_sd <- d_cluster_sd$evaluation[which(d_cluster_sd$basket_cat == "high")] - d_cluster_sd$evaluation[which(d_cluster_sd$basket_cat == "low")]
      basket_value_diff_sd <- d_cluster_sd$basket_value[which(d_cluster_sd$basket_cat == "high")] - d_cluster_sd$basket_value[which(d_cluster_sd$basket_cat == "low")]
      basket_value_ex_diff_sd <- d_cluster_sd$basket_value_ex[which(d_cluster_sd$basket_cat == "high")] - d_cluster_sd$basket_value_ex[which(d_cluster_sd$basket_cat == "low")]
      basket_value_sd_diff_sd <- d_cluster_sd$value_sd[which(d_cluster_sd$basket_cat == "high")] - d_cluster_sd$value_sd[which(d_cluster_sd$basket_cat == "low")]
      basket_rating_sd_diff_sd <- d_cluster_sd$basket_rating_sd[which(d_cluster_sd$basket_cat == "high")] - d_cluster_sd$basket_rating_sd[which(d_cluster_sd$basket_cat == "low")]
      d_sub_diff <- rbind(d_sub_diff, data.frame(subject_nr = i, basket_cluster = k, confidence_diff = outcome_conf_diff_sd, evaluation_diff = outcome_eval_diff_sd, basket_value_diff = basket_value_diff_sd, basket_value_ex_diff = basket_value_ex_diff_sd, basket_value_sd_diff = basket_value_sd_diff_sd, basket_rating_sd_diff = basket_rating_sd_diff_sd, basket_matching_var = "rating_sd"))
    }
  }
  basket_conf_data_exp4_diff <- rbind(basket_conf_data_exp4_diff, d_sub_diff)
}

basket_conf_data_exp4_diff_ex <- subset(basket_conf_data_exp4_diff, basket_matching_var == "evaluation_ex")
basket_conf_data_exp4_diff_ex <- standardize_vars(basket_conf_data_exp4_diff_ex, vars = c("basket_value_diff", "basket_rating_sd_diff", "basket_value_ex_diff", "basket_value_sd_diff"))


bf_diff_ex_exp4 <- bf(confidence_diff ~ 1 + basket_value_diff_s + basket_rating_sd_diff_s + basket_value_sd_diff_s + (1 +  basket_value_diff_s  + basket_value_diff_s + basket_value_sd_diff_s| subject_nr))

brm_diff_exp4_ex_student <- brm(
  bf_diff_ex_exp4,
  data = basket_conf_data_exp4_diff_ex,
  family = student(),
  backend = "cmdstan",
  iter = 4000,
  warmup = 2000,
  chains = 4,
  cores = 4,
  threads = threading(3),
  file = here::here("01_analyses/fitted_models/008_brm_diff_exp4_ex_student.rds"),
  file_refit = "on_change"
)
