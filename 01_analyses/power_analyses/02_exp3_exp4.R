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

bf5_e2h1_gauss <- bf(confidence_01 ~ basket_cat_f + basket_value_s + item_value_sd_s  + basket_value_ex_s
           + (1 + basket_cat_f + basket_value_s + item_value_sd_s  + basket_value_ex_s | subject)
           + (1 | stim_name))

brm5_e2h1_gauss <- brm(bf5_e2h1_gauss
           , data = basket_conf_data
           , family = gaussian()
           , chains = 4, cores = 4, warmup = 2000, iter = 4000
           , backend = "cmdstanr", threads = threading(3)
           , seed = 1
           , file = here::here("01_analyses/fitted_models/047_brm5_e2h1_gauss.rds")
           , file_refit = "on_change"
           )

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

bf5_e2h1_zoib <- bf(confidence_01 ~ basket_cat_f + basket_value_s + item_value_sd_s  + basket_value_s + basket_value_ex_s
                    + (1 + basket_cat_f + basket_value_s + item_value_sd_s  + basket_value_s + basket_value_ex_s | subject)
                    + (1 | stim_name),
                    phi ~ basket_cat_f + basket_value_s + item_value_sd_s  + basket_value_s + basket_value_ex_s  + (1 + basket_cat_f + basket_value_s + item_value_sd_s  + basket_value_s + basket_value_ex_s | subject),
                    zoi ~ basket_cat_f + basket_value_s + item_value_sd_s  + basket_value_s + basket_value_ex_s  + (1 + basket_cat_f + basket_value_s + item_value_sd_s  + basket_value_s + basket_value_ex_s | subject),
                    coi ~ basket_cat_f + basket_value_s + item_value_sd_s  + basket_value_s + basket_value_ex_s  + (1 + basket_cat_f + basket_value_s + item_value_sd_s  + basket_value_s + basket_value_ex_s | subject)
  )

brm5_e2h1_zoib <- brm(bf5_e2h1_zoib
                     , data = basket_conf_data
                     , family = zero_one_inflated_beta() 
                     , chains = 4, cores = 4, warmup = 2000, iter = 4000
                     , backend = "cmdstanr", threads = threading(3)
                     , seed = 1
                     , file = here::here("01_analyses/fitted_models/048_brm5_e2h1_zoib.rds")
                     , file_refit = "on_change"
           )

new_dat_p60 <- subset(basket_conf_data, subject %in% unique(basket_conf_data$subject)[1:60])
new_dat_p60$confidence_01 <- predict(brm5_e2h1_gauss, newdata = new_dat_p60, summary = F, nsamples = 1)[1,]

brm5_e2h1_gauss_60p <- update(brm5_e2h1_gauss, newdata = new_dat_p60, chains = 6, cores = 6, threads = threading(4), warmup = 1000, iter = 2000, control = list(adapt_delta = .80))
brm5_e2h1_BB_60p <- update(brm5_e2h1_BB, newdata = new_dat_p60, chains = 6, cores = 6, threads = threading(4), thin = 1, warmup = 1000, iter = 2000, control = list(adapt_delta = .80))

new_dat_p60$confidence_01 <- predict(brm5_e2h1_zoib, newdata = new_dat_p60, summary = F, nsamples = 1)[1,]
brm5_e2h1_zoib_60p <- update(brm5_e2h1_zoib, newdata = new_dat_p60, chains = 6, cores = 6, threads = threading(4), thin = 1, warmup = 1000, iter = 2000, control = list(adapt_delta = .80))

v_ci_width <- c()
v_ci_width_at_k <- list(c(999))
k_seq <- seq(50, 100, by = 10)
k <- 1

v_ci_width_brm5_e2h1_BB <- c()
v_ci_width_at_k_brm5_e2h1_BB <- list(c(999))

v_ci_width_brm5_e2h1_zoib <- c()
v_ci_width_at_k_brm5_e2h1_zoib <- list(c(999))

while((mean(v_ci_width_at_k[[k]] < 10) < .80 | mean(v_ci_width_at_k_brm5_e2h1_BB[[k]] < 10) < .80  | mean(v_ci_width_at_k_brm5_e2h1_zoib[[k]] < 10) < .80 ) && k < length(k_seq)){
  k <- k+1
  for(i in 1:100){
    sample_sub <- sample(unique(basket_conf_data$subject), k_seq[k-1])
    new_dat_p60 <- subset(basket_conf_data, subject %in% sample_sub)
    for(r in 1:nrow(new_dat_p60)){
      new_dat_p60$subject[r] <- toString(which(sample_sub == new_dat_p60$subject[r]))
    }
    
    # brm 1
    new_dat_p60$confidence_01 <- predict(brm5_e2h1_gauss, newdata = new_dat_p60, allow_new_levels =T, summary = F, nsamples = 1)[1,]
    brm5_e2h1_gauss_60p <- update(brm5_e2h1_gauss, newdata = new_dat_p60, chains = 6, cores = 6, threads = threading(4), warmup = 1000, iter = 2000, thin = 1, control = list(adapt_delta = .80))
    fe_brm5_e2h1_gauss_60p <- data.frame(fixef(brm5_e2h1_gauss_60p))
    brm5_e2h1_gauss_60p_ci_low <- abs((fe_brm5_e2h1_gauss_60p$Estimate[1]  + fe_brm5_e2h1_gauss_60p$Q2.5[2]) - (fe_brm5_e2h1_gauss_60p$Estimate[1]  - fe_brm5_e2h1_gauss_60p$Q2.5[2]))*200   # calculate lower CI
    brm5_e2h1_gauss_60p_ci_high <- abs((fe_brm5_e2h1_gauss_60p$Estimate[1]  + fe_brm5_e2h1_gauss_60p$Q97.5[2]) - (fe_brm5_e2h1_gauss_60p$Estimate[1]  - fe_brm5_e2h1_gauss_60p$Q97.5[2]))*200   # calculate upper CI
    v_ci_width[i] <- brm5_e2h1_gauss_60p_ci_high-brm5_e2h1_gauss_60p_ci_low
    
    # brm5_e2h1_BB
    new_dat_p60$confidence <- predict(brm5_e2h1_BB, newdata = new_dat_p60, allow_new_levels =T, summary = F, nsamples = 1)[1,]
    brm5_e2h1_BB_60p <- update(brm5_e2h1_BB, newdata = new_dat_p60, chains = 6, cores = 6, threads = threading(4), warmup = 1000, iter = 2000, thin = 1, control = list(adapt_delta = .80))
    fe_brm5_e2h1_BB_60p <- data.frame(fixef(brm5_e2h1_BB_60p))
    brm5_e2h1_BB_60p_ci_low <- abs(plogis(fe_brm5_e2h1_BB_60p$Estimate[1]  + fe_brm5_e2h1_BB_60p$Q2.5[2])*200 - plogis(fe_brm5_e2h1_BB_60p$Estimate[1]  - fe_brm5_e2h1_BB_60p$Q2.5[2])*200)  # calculate lower CI
    brm5_e2h1_BB_60p_ci_high <- abs(plogis(fe_brm5_e2h1_BB_60p$Estimate[1]  + fe_brm5_e2h1_BB_60p$Q97.5[2])*200 - plogis(fe_brm5_e2h1_BB_60p$Estimate[1]  - fe_brm5_e2h1_BB_60p$Q97.5[2])*200)   # calculate upper CI
    v_ci_width_brm5_e2h1_BB[i] <- brm5_e2h1_BB_60p_ci_high-brm5_e2h1_BB_60p_ci_low

    # brm5_e2h1_zoib
    new_dat_p60$confidence_01 <- predict(brm5_e2h1_zoib, newdata = new_dat_p60, allow_new_levels =T, summary = F, nsamples = 1)[1,]
    brm5_e2h1_zoib_60p <- update(brm5_e2h1_zoib, newdata = new_dat_p60, chains = 6, cores = 6, threads = threading(4), warmup = 1000, iter = 2000, thin = 1, control = list(adapt_delta = .80))
    fe_brm5_e2h1_zoib_60p <- data.frame(fixef(brm5_e2h1_zoib_60p))
    brm5_e2h1_zoib_60p_ci_low <- abs(plogis(fe_brm5_e2h1_zoib_60p$Estimate[1]  + fe_brm5_e2h1_zoib_60p$Q2.5[5])*200 - plogis(fe_brm5_e2h1_zoib_60p$Estimate[1]  - fe_brm5_e2h1_zoib_60p$Q2.5[5])*200)  # calculate lower CI
    brm5_e2h1_zoib_60p_ci_high <- abs(plogis(fe_brm5_e2h1_zoib_60p$Estimate[1]  + fe_brm5_e2h1_zoib_60p$Q97.5[5])*200 - plogis(fe_brm5_e2h1_zoib_60p$Estimate[1]  - fe_brm5_e2h1_zoib_60p$Q97.5[5])*200)   # calculate upper CI
    v_ci_width_brm5_e2h1_zoib[i] <- brm5_e2h1_zoib_60p_ci_high-brm5_e2h1_zoib_60p_ci_low
    
    print(paste0("################## ", (k-1), "---", i, " ##################"))
  }
  v_ci_width_at_k[[k]] <- v_ci_width
  v_ci_width_at_k_brm5_e2h1_BB[[k]] <- v_ci_width_brm5_e2h1_BB
  v_ci_width_at_k_brm5_e2h1_zoib[[k]] <- v_ci_width_brm5_e2h1_zoib
  saveRDS(v_ci_width_at_k, file = "backup_ci_width")
  saveRDS(v_ci_width_at_k_brm5_e2h1_BB, file = "backup_ci_width_brm5_e2h1_BB")
  saveRDS(v_ci_width_at_k_brm5_e2h1_zoib, file = "backup_ci_width_brm5_e2h1_zoib")
}

mean(v_ci_width_at_k[[k]] < 10) # can be checked for the Gaussian model with k = 1, 2, 3, 4, 5 to see power for CI < 10 at each sample size
mean(v_ci_width_at_k_brm5_e2h1_BB[[k]] < 10) # can be checked for the betabinomial model with k = 1, 2, 3, 4, 5 to see power for CI < 10  at each sample size
mean(v_ci_width_at_k_brm5_e2h1_zoib[[k]] < 10) # can be checked for the zoib model with k = 1, 2, 3, 4, 5 to see power for CI < 10 at each sample size
