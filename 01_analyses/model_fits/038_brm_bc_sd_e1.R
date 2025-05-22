osf_node <- osf_retrieve_node("9y38n") # retrieve OSF node
osf_datafolder <-
  osf_ls_files(osf_node, path = "processed_data", n_max = Inf) # get data folder
basket_conf_data <- osf_load_file(osf_datafolder$id[osf_datafolder$name == "basket_conf_data_exp1.csv"],
                                  "basket_conf_data_exp1.csv",
                                  stringsAsFactors = F) # load file

n_cores = 4
n_chains = 4
n_threading = 3
n_iter = 8000
n_warmup = 4000
reloo_max = 0
options(mc.cores = 5)
options(future.globals.maxSize = 1.0 * 1e9)  ## 1.0 GB


#Preregistered Exclusion criteria will be applied and relevant variables centered and standardized.

basket_conf_data_prereg <- droplevels(subset(basket_conf_data,
                                             prereg_exclusion == 0))

basket_conf_data_prereg <-
  standardize_vars(
    basket_conf_data_prereg,
    vars = c("confidence_mean",
             "value_sd", "rep_nr", "evaluation")
  )

#and subject
basket_conf_data_prereg$subject <-
  as.factor(basket_conf_data_prereg$subject)

#rescale confidence ratings to between 0 and 1 to make the model easier to estimate for e.g. zoib model
basket_conf_data_prereg$confidence_01 <-
  (
    basket_conf_data_prereg$confidence - min(basket_conf_data_prereg$confidence, na.rm = T)
  ) /
  (
    max(basket_conf_data_prereg$confidence, na.rm = T) - min(basket_conf_data_prereg$confidence, na.rm = T)
  )




for (i in 1:nrow(basket_conf_data_prereg)) {
  basket_conf_data_prereg$basket_value[i] <-
    (
      basket_conf_data_prereg$item1_value[i] +
        basket_conf_data_prereg$item2_value[i] +
        basket_conf_data_prereg$item3_value[i]
    ) / 3
  basket_conf_data_prereg$basket_value_ex[i] <-
    abs(basket_conf_data_prereg$basket_value[i] - 100)
  
}

basket_conf_data_prereg$basket_value_s <-
  scale(basket_conf_data_prereg$basket_value, scale = T)
basket_conf_data_prereg$basket_value_ex_s <-
  scale(basket_conf_data_prereg$basket_value_ex, scale = T)


basket_conf_data_prereg$fold_ex2basket <-
  rep(1:10,
      each = 6,
      length.out = nrow(basket_conf_data_prereg))
set.seed(123)
basket_conf_data_prereg$fold_pp <-
  kfold_split_grouped(K = 10, x = basket_conf_data_prereg$subject)

basket_conf_data_prereg_div_reverse <- basket_conf_data_prereg

basket_conf_data_prereg_div_reverse$basket_diversity_cat <- ifelse(basket_conf_data_prereg_div_reverse$basket_diversity_cat == "low", "Sim.High", "Sim.Low")

basket_conf_data_prereg_div_reverse$basket_confidence_cat <- ifelse(basket_conf_data_prereg_div_reverse$basket_confidence_cat == "low", "Conf.Low", "Conf.High")

basket_conf_data_prereg_div_reverse$basket_diversity_cat_f <- factor(basket_conf_data_prereg_div_reverse$basket_diversity_cat)
contrasts(basket_conf_data_prereg_div_reverse$basket_diversity_cat_f) <- contr.sum(2)

basket_conf_data_prereg_div_reverse$basket_confidence_cat_f <- factor(basket_conf_data_prereg_div_reverse$basket_confidence_cat)
contrasts(basket_conf_data_prereg_div_reverse$basket_confidence_cat_f) <- contr.sum(2)

basket_conf_data_prereg$basket_confidence_cat_f <- factor(basket_conf_data_prereg$basket_confidence_cat)
basket_conf_data_prereg$basket_diversity_cat_f <- factor(basket_conf_data_prereg$basket_diversity_cat)

bf_bc_sd <- bf(value_sd ~ basket_confidence_cat_f+basket_diversity_cat_f 
           + (1 + basket_confidence_cat_f+basket_diversity_cat_f   | subject))

brm_bc_sd <- brm(bf_bc_sd
           , data = basket_conf_data_prereg
           , family = gaussian()
           , chains = n_cores, cores = n_cores, warmup = 2000, iter = 4000
           , backend = "cmdstanr", threads = threading(2)
           , seed = 1
           , file = here::here("01_analyses/fitted_models/038_brm_bc_sd_e1.rds")
           , file_refit = "on_change"
           , control = list(adapt_delta = .99, max_treedepth = 15)
           )