

if(!exists("brms_helpers_executed")){
  
  brms_helpers_executed = TRUE
  
  # initiate custom beta-binomial family ------------------------------------
  
  beta_binomial2 <- custom_family(
    "beta_binomial2", dpars = c("mu", "phi"),
    links = c("logit", "log"), lb = c(NA, 0),
    type = "int", vars = "vint1[n]"
  )
  
  stan_funs <- "
  real beta_binomial2_lpmf(int y, real mu, real phi, int T) {
    return beta_binomial_lpmf(y | T, mu * phi, (1 - mu) * phi);
  }
  int beta_binomial2_rng(real mu, real phi, int T) {
    return beta_binomial_rng(T, mu * phi, (1 - mu) * phi);
  }
"
  
  stanvars <- stanvar(scode = stan_funs, block = "functions")
  
  log_lik_beta_binomial2 <- function(i, prep) {
    mu <- prep$dpars$mu[, i]
    phi <- prep$dpars$phi
    trials <- prep$data$vint1[i]
    y <- prep$data$Y[i]
    beta_binomial2_lpmf(y, mu, phi, trials)
  }
  
  posterior_predict_beta_binomial2 <- function(i, prep, ...) {
    mu <- prep$dpars$mu[, i]
    phi <- prep$dpars$phi
    trials <- prep$data$vint1[i]
    beta_binomial2_rng(mu, phi, trials)
  }
  
  posterior_epred_beta_binomial2 <- function(prep) {
    mu <- prep$dpars$mu
    trials <- prep$data$vint1
    trials <- matrix(trials, nrow = nrow(mu), ncol = ncol(mu), byrow = TRUE)
    mu * trials
  }
  
  # 
  # posterior_predict_beta_binomial2 <- function(i, draws, ...) {
  #   mu <- draws$dpars$mu[, i]
  #   phi <- draws$dpars$phi
  #   N <- draws$data$trials[i]
  #   beta_binomial2_rng(mu, phi, N)
  # }
  # 
  # log_lik_beta_binomial2 <- function(i, draws) {
  #   mu <- draws$dpars$mu[, i]
  #   phi <- draws$dpars$phi
  #   N <- draws$data$trials[i]
  #   y <- draws$data$Y[i]
  #   beta_binomial2_lpmf(y, mu, phi, N)
  # }
  # 
  # fitted_beta_binomial2 <- function(draws) {
  #   mu <- draws$dpars$mu
  #   trials <- draws$data$trials
  #   trials <- matrix(trials, nrow = nrow(mu), ncol = ncol(mu), byrow = TRUE)
  #   mu * trials
  # }
  # 
  # pp_expect_beta_binomial2 <- function(draws) {
  #   mu <- draws$dpars$mu
  #   trials <- draws$data$trials
  #   trials <- matrix(trials, nrow = nrow(mu), ncol = ncol(mu), byrow = TRUE)
  #   mu * trials
  # }
  # 
  # 


  
  # brms diagnostics --------------------------------------------------------
  
  print_sep <- function(width, header){
    cat(paste(c("\n", rep("=",width), "\n"), collapse = ""))
    cat(paste(c(rep(" ",floor(width/2 - nchar(header)/2)), header, rep(" ",floor(width/2 - nchar(header)/2)), "\n"), collapse = ""))
    cat(paste(c(rep("=",width), "\n"), collapse = ""))
  }
  
  brm_diagnose <- function(brm_fit, output = c("summary", "traceplot", "b_traceplot", "pairs_b")){
    
    brm_diag <- list()
    
    brm_diag$summary <- summary(brm_fit)
    if ("summary" %in% output) {
      options(width = 200)
      print_sep(80, "summary")
      print(brm_diag$summary)
    }
    
    brm_diag$traceplot <- plot(brm_fit, ask = F, plot = F)
    if ("traceplot" %in% output) {
      print_sep(80, "traceplot")
      plot(brm_diag$traceplot[[1]])
    }
    
    brm_diag$b_traceplot <- plot(brm_fit, pars = "^b", ask = F, plot = F)
    if ("b_traceplot" %in% output) {
      print_sep(80, "zoom-in traceplot of population-level parameters")
      plot(brm_diag$b_traceplot[[1]])
    }
    
    brm_diag$pairs_b <- pairs(brm_fit, pars = "^b")
    if ("pairs_b" %in% output) {
      print_sep(80, "sampling correlation of population-level parameters")
      plot(brm_diag$pairs_b)
    }
    
    return(brm_diag)
  }
  
  
  # brms model evaluation ---------------------------------------------------
  
  skew <- function(x) {
    xdev <- x - mean(x)
    n <- length(x)
    r <- sum(xdev^3) / sum(xdev^2)^1.5
    return(r * sqrt(n) * (1 - 1/n)^1.5)
  }
  
  # function to compute loo-object and plot different model-evaluation plots
  brm_evaluate <- function(brm_fit, create_loo = TRUE, reloo = FALSE, reloo_max = 10, ..., reloo_extra_args = NULL){
    
    brm_eval <- list()
    if(create_loo == TRUE){
      brm_eval$loo <- loo(brm_fit, reloo = FALSE, ...)
    }
    if (create_loo == TRUE & reloo == TRUE) {
      if (sum(brm_eval$loo$diagnostics$pareto_k >= .7) <= reloo_max) {
        if(!is.null(reloo_extra_args)){
          brm_eval$loo <- loo(brm_fit, reloo = reloo, reloo_args = reloo_extra_args, ...)
        } else  {
          brm_eval$loo <- loo(brm_fit, reloo = reloo, reloo_args = list(cores = n_cores, chains = n_cores), ...)
        }
      } else {
        warning(sum(brm_eval$loo$diagnostics$pareto_k >= .7), 
                " observations  have pareto-k of >= 0.7, exceeding reloo_max = ", 
                reloo_max, ". Reloo set to FALSE. Increase reloo_max if you still want to reloo.")
      }
    }
    
   
    brm_eval$pp_dist <- ggplotGrob(pp_check(brm_fit))
    brm_eval$pp_mean <- ggplotGrob(pp_check(brm_fit, type = "stat", stat = "mean", ...))
    brm_eval$pp_sd <- ggplotGrob(pp_check(brm_fit, type = "stat", stat = "sd", ...))
    brm_eval$pp_skew <- ggplotGrob(pp_check(brm_fit, type = "stat", stat = "skew", ...))
    brm_eval$pp_median <-ggplotGrob(pp_check(brm_fit, type = "stat", stat = "median", ...))
    brm_eval$pp_lp_overlay <- ggplotGrob(pp_check(brm_fit, type = "loo_pit_overlay", ...))
    brm_eval$pp_lp_qq <- ggplotGrob(pp_check(brm_fit, type = "loo_pit_qq", ...))
    brm_eval$pp_ecdf <- ggplotGrob(pp_check(brm_fit, type = "ecdf_overlay"))
    brm_eval$pp_scatter_avg <- ggplotGrob(pp_check(brm_fit, type = "scatter_avg", ...))
    
    return(brm_eval)
  }
  
  
brm_assumption_check <- function(brm_fit){
  
  # outliers
  resid_data <- data.frame(residuals(brm_fit, method ="posterior_predict"))
  z_resids <- scale(resid_data$Estimate, scale = TRUE)
  cat("############################## outliers ##############################\n")
  cat(paste("z_redids > 3.0 = ", sum(abs(z_resids) > 3.0) / length(z_resids), " %\n"))
  cat(paste("z_redids > 2.5 = ", sum(abs(z_resids) > 2.5) / length(z_resids), " %\n"))
  cat(paste("z_redids > 2.0 = ", sum(abs(z_resids) > 2.0) / length(z_resids), " %\n"))
  cat("######################################################################\n")
  
  # autocorrelation of residuals
  acf(z_resids)# see above for how to get these
  
  # homoscedasticity of residuals
  fitted_data <- data.frame(fitted(brm_fit))
  z_fitted <- scale(fitted_data$Estimate, scale = TRUE)
  pd <- data.frame(z_fitted = z_fitted, z_resids =z_resids)
  ggplot(pd, aes(z_fitted, z_resids)) + 
    geom_point() +
    geom_smooth(method = "loess", se = FALSE)
  
  # normality of residuals
  ggplot(pd, aes(sample = z_resids)) + geom_qq() + geom_qq_line()
  
}
  
  # compute posterior probability > or < 0 for parameter.
brm_postprop <- function(brm_fit, pars, direction = c("<", ">")){
    
    pars <- ifelse(substr(pars, 1, 2) != "b_", paste0("b_", pars), pars)  
  
    post_samples <- c(as_draws_array(brm_fit, variable = pars))
    if (direction == "<") {
      postprob <- mean(post_samples < 0)
    } else if (direction == ">") {
      postprob <- mean(post_samples > 0)
    } else {
      warning("direction must be either '<' or '>'")
    }
    return(postprob)
}
  

report_brm <- function(fit, pred_name, postprop_direction){
  fit_rep <- list()
  fit_eff <- data.frame(summary(fit)$fixed)
  fit_rep$es <- sprintf("%.2f", round(fit_eff[which(row.names(fit_eff) == pred_name),]$Estimate, 2))
  fit_rep$ciMIN <- sprintf("%.2f", round(fit_eff[which(row.names(fit_eff) == pred_name),]$l.95..CI, 2))
  fit_rep$ciMAX <- sprintf("%.2f",round(fit_eff[which(row.names(fit_eff) == pred_name),]$u.95..CI, 2))
  fit_rep$pp <- ifelse(round(brm_postprop(fit, pred_name, direction = postprop_direction), 4) == 0, "< .001", 
                       paste0("= ", sub('^(-)?0[.]', '\\1.', sprintf("%.3f", round(brm_postprop(fit, pred_name, direction = postprop_direction), 4)))))
  return(fit_rep)
}

report_brm_apa <- function(fit, pred_name, postprop_direction, log_scale = FALSE, hypothesis = FALSE, offset = 0, print_out = TRUE){
  fit_rep <- list()
  pp_sign <- ifelse(postprop_direction == "<", paste0("$","pp", "_{-}","$"),  paste0("$","pp", "_{+}","$"))

  if(hypothesis == FALSE){
    fit_eff <- data.frame(summary(fit)$fixed)
    if(log_scale == TRUE){
      h_tmp_txt <- c(pred_name = paste0("plogis(Intercept",
                                        "+",
                                        pred_name, 
                                        ")",
                                        "-",
                                        "plogis(Intercept",
                                        "-",
                                        pred_name, 
                                        ")",
                                        ifelse(postprop_direction == "<", ">", "<"),
                                        ifelse(postprop_direction == "<", offset, -offset)))
      h_tmp <- hypothesis(fit, h_tmp_txt, alpha = 0.025)
      fit_rep$es <- sprintf("%.2f", round(h_tmp$hypothesis[2], 2))
      fit_rep$ciMIN <- sprintf("%.2f", round(h_tmp$hypothesis[4], 2))
      fit_rep$ciMAX <- sprintf("%.2f",round(h_tmp$hypothesis[5], 2))
      fit_rep$pp <- ifelse(round(1-h_tmp$hypothesis$Post.Prob,3) > 0, 
                          paste0(" = ", report_value(1-h_tmp$hypothesis$Post.Prob, 3, fraction = TRUE)), "< .001")
    } else {
       h_tmp_txt <- c(pred_name = paste0(pred_name,
                                        ifelse(postprop_direction == "<", ">", "<"),
                                        ifelse(postprop_direction == "<", offset, -offset)))
      h_tmp <- hypothesis(fit, h_tmp_txt, alpha = 0.025)
      fit_rep$es <- sprintf("%.2f", round(h_tmp$hypothesis[2], 2))
      fit_rep$ciMIN <- sprintf("%.2f", round(h_tmp$hypothesis[4], 2))
      fit_rep$ciMAX <- sprintf("%.2f",round(h_tmp$hypothesis[5], 2))
      fit_rep$pp <- ifelse(round(1-h_tmp$hypothesis$Post.Prob,3) > 0, 
                          paste0(" = ", report_value(1-h_tmp$hypothesis$Post.Prob, 3, fraction = TRUE)), "< .001")
    }
  } else {
    h_tmp <- fit
    fit_rep$es <- sprintf("%.2f", round(h_tmp$hypothesis[2], 2))
    fit_rep$ciMIN <- sprintf("%.2f", round(h_tmp$hypothesis[4], 2))
    fit_rep$ciMAX <- sprintf("%.2f",round(h_tmp$hypothesis[5], 2))
    fit_rep$pp <- ifelse(round(1-h_tmp$hypothesis$Post.Prob,3) > 0, 
                        paste0(" = ", report_value(1-h_tmp$hypothesis$Post.Prob, 3, fraction = TRUE)), "< .001")
  }

  

  if(print_out == TRUE){
    rep <- paste0("Estimate = ", fit_rep$es, ", 95%CI = ", "[", fit_rep$ciMIN, ", ", fit_rep$ciMAX, "], ", pp_sign, " ",
                # ifelse(fit_rep$pp == "< .001" & hypothesis == FALSE, " ", " x "), 
                        fit_rep$pp)
  } else {
    fit_rep$pp <- ifelse(substr(fit_rep$pp, 0, 3) == " = ", substr(fit_rep$pp, 4, nchar(fit_rep$pp)), fit_rep$pp)
    rep <- list(es = fit_rep$es, ciMIN = fit_rep$ciMIN, ciMAX = fit_rep$ciMAX, pp = fit_rep$pp)
  }
  return(rep)
}

report_value <- function(value, digits, fraction = FALSE){
  temp_sprintf <- paste0("%.", toString(digits), "f")
  value <- ifelse(fraction == FALSE, sprintf(temp_sprintf, round(value, digits)), 
                  sub('^(-)?0[.]', '\\1.', sprintf("%.3f", round(value, digits))))
  return(value)
}
  
}

brm_eval_summary <- function(brm_eval, output = c("loo_table", "loo_plot", 
                                        "pp_dist", "pp_mean", "pp_sd", "pp_skew",
                                        "pp_median", "pp_loo_pit_overlay", 
                                        "pp_loo_pit_qq", "ecdf_overlay", 
                                        "scatter_avg"), ...){
  
  if ("loo_table" %in% output) {
    print_sep(80, "loo table")
    print(brm_eval$loo)
  }
  if ("loo_plot" %in% output) {
    print_sep(80, "pareto-k plot (after reloo)")
    plot(brm_eval$loo, label_points = TRUE)
  }
  if ("pp_dist" %in% output) {
    print_sep(80, "posterior prediction: distribution")
    grid.newpage()
    print(grid.draw(brm_eval$pp_dist)) # for some reason this needs to include print() for all the plots to show up in the rmd file
  }
  if ("pp_mean" %in% output) {
    print_sep(80, "posterior prediction: mean")
    grid.newpage()
    grid.draw(brm_eval$pp_mean)
  }
  if ("pp_sd" %in% output) {
    print_sep(80, "posterior prediction: sd")
    grid.newpage()
    grid.draw(brm_eval$pp_sd)
  }
  if ("pp_skew" %in% output) {
    print_sep(80, "posterior prediction: skew")
    grid.newpage()
    grid.draw(brm_eval$pp_skew)
  }
  if ("pp_median" %in% output) {
    print_sep(80, "posterior prediction: median")
    grid.newpage()
    grid.draw(brm_eval$pp_median)
  }
  if ("pp_loo_pit_overlay" %in% output) {
    print_sep(80, "posterior prediction: loo-pit overlay")
    grid.newpage()
    grid.draw(brm_eval$pp_lp_overlay)
  }
  if ("pp_loo_pit_qq" %in% output) {
    print_sep(80, "posterior prediction: loo-pit QQ")
    grid.newpage()
    grid.draw(brm_eval$pp_lp_qq)
  }
  if ("ecdf_overlay" %in% output) {
    print_sep(80, "posterior prediction: estimated cumulative distribution ")
    grid.newpage()
    grid.draw(brm_eval$pp_ecdf)
  }
  if ("scatter_avg" %in% output) {
    print_sep(80, "posterior prediction: scatter avg ")
    grid.newpage()
    grid.draw(brm_eval$pp_scatter_avg)
  }
}



plot_categorical_effect <- function(m, raw_data = NULL, pred = NULL, outcome = NULL, xname = NULL, yname = NULL, title, add_pred = TRUE, ylim_adj = 0, point_color = "#4891ac", ylim = "auto", ylim_min = NA, ylim_max = NA, pred_data = NULL, viol_width = 0.1, add_zoom = TRUE, emm_precomp = NULL, cluster_name = "subject", trunc_pred = FALSE, trunc_pred_min = NA, trunc_pred_max = NA, line_preds = c(), int_var_cond = NULL, plot_line_preds = TRUE, plot_equal_lines = FALSE, ...){
  

  add_args <- list(...)
  f_env <- environment()
  list2env(add_args, f_env)

  if(is.null(raw_data)){
    raw_data <- m$data
  }
  if(!(exists("x_angle"))){
    x_angle <- 0
  }


  

  if(!is.null(int_var_cond)){
    conditions <- make_conditions(m, eval(int_var_cond))
    tmp_cedat <- plot(conditional_effects(m, conditions = conditions, robust = TRUE), plot = F)[[pred]]$data
    tmp_cedat$group <- with(tmp_cedat, paste(tmp_cedat[,1], tmp_cedat[,2], sep="_")) 
    average_tmp_cedat <- aggregate(tmp_cedat[, which(names(tmp_cedat) %in% c("estimate__", "se__", "lower__" , "upper__"))], by=list(tmp_cedat$group), FUN=mean, na.rm = TRUE)
    tmp_cedat <- tmp_cedat[, -which(names(tmp_cedat) %in% c("estimate__", "se__", "lower__" , "upper__", eval(cluster_name)))]
    tmp_cedat_unique <- tmp_cedat[!duplicated(tmp_cedat$group), ]
    tmp_cedat_final <- merge(tmp_cedat_unique, average_tmp_cedat, by.x = "group", by.y = "Group.1", all.x = TRUE)
    tmp_cedat_final <- tmp_cedat_final[, -which(names(tmp_cedat_final) %in% c("group"))]
    tmp_cedat <- tmp_cedat_final[,-4]

  } else {
    tmp_cedat <- plot(conditional_effects(m, robust = TRUE), plot = F)[[pred]]$data
  }




  raw_data$plot_outcome <- unlist(raw_data[which(names(raw_data) == outcome)])
  if(grepl(":", pred, fixed = TRUE)){
    pred_l <- c(strsplit(pred, ":")[[1]][1], strsplit(pred, ":")[[1]][2])
    raw_data$combined_pred <- rep(NA)
    for(i in 1:nrow(raw_data)){
      raw_data$combined_pred[i] <- paste(as.character(raw_data[i, which(names(raw_data) == pred_l[1])]), as.character(raw_data[i, which(names(raw_data) == pred_l[2])]), sep = ":")
    }
    tmp_cedat$combined_pred <- rep(NA)
    for(i in 1:nrow(tmp_cedat)){
      tmp_cedat$combined_pred[i] <- paste(as.character(tmp_cedat[i, which(names(tmp_cedat) == pred_l[1])]), as.character(tmp_cedat[i, which(names(tmp_cedat) == pred_l[2])]), sep = ":")
    }
    names(tmp_cedat)[which(names(tmp_cedat) == "combined_pred")] <- pred
    names(raw_data)[which(names(raw_data) == "combined_pred")] <- pred
  }
  raw_data$plot_pred <- as.numeric(mapvalues(unlist(raw_data[which(names(raw_data) == pred)]), 
                                             from = levels(factor(unlist(raw_data[which(names(raw_data) == pred)]))), 
                                             to =1:nrow(tmp_cedat)))
  
  if(is.null(emm_precomp)){
    if(grepl(":", pred, fixed = TRUE)){
      emm_tmp <- emmeans::emmeans(m, pred_l, robust = TRUE)
    } else {
      emm_tmp <- emmeans::emmeans(m, pred, robust = TRUE)
    }
  } else {
    emm_tmp <- emm_precomp
  }
  emm_sub <-  emm_tmp@post.beta
  hpdi_dat <- data.frame(emm_tmp)
  if(grepl(":", pred, fixed = TRUE)){
    hpdi_dat$combined_pred <- rep(NA)
    for(i in 1:nrow(hpdi_dat)){
      hpdi_dat$combined_pred[i] <- paste(as.character(hpdi_dat[i, which(names(hpdi_dat) == pred_l[1])]), as.character(hpdi_dat[i, which(names(hpdi_dat) == pred_l[2])]), sep = ":")
    }
      names(hpdi_dat)[which(names(hpdi_dat) == "combined_pred")] <- pred
  }

  

  raw_data_summary <- aggregate(raw_data$plot_outcome, by = list(raw_data[,which(names(raw_data) == cluster_name)], raw_data[,which(names(raw_data) == pred)]), FUN = mean) 
  names(raw_data_summary) <- c("subject", paste(pred), "plot_outcome")
  if(grepl(":", pred, fixed = TRUE)){
    raw_data_summary$pred1 <- sapply(1:nrow(raw_data_summary), function(x) strsplit(raw_data_summary[x,pred], ":")[[1]][1])
    raw_data_summary$pred2 <- sapply(1:nrow(raw_data_summary), function(x) strsplit(raw_data_summary[x,pred], ":")[[1]][2])
  }
  raw_data_summary$line_col <- rep(NA)
  if(length(line_preds) == nrow(tmp_cedat)-1){
      for(k in unique(raw_data_summary$subject)){
        tmp_sum_dat <- raw_data_summary[which(raw_data_summary$subject == k),]

    # check if line_pred argument has tmp_cedat -1 values
   
        for(i in 1:length(line_preds)){
          # if(!grepl(":", pred, fixed = TRUE)){
          cond_left <- as.character(tmp_cedat[i,pred])
          cond_right <- as.character(tmp_cedat[i+1,pred])
          if(!(line_preds[i] %in% c(">", "<"))){
            if(plot_line_preds == TRUE & plot_equal_lines == TRUE){
              raw_data_summary$line_col <- rep("#b8dbe8")
            }
            else{
              next
            }
          } else if(line_preds[i] == ">"){
            if(tmp_sum_dat$plot_outcome[which(tmp_sum_dat[,pred] == cond_left)] > tmp_sum_dat$plot_outcome[which(tmp_sum_dat[,pred] == cond_right)]){
              raw_data_summary$line_col[which(raw_data_summary$subject == k & (raw_data_summary[,pred] == cond_left | raw_data_summary[,pred] == cond_right))] <- rep("#78cc78") 
            } else {
              raw_data_summary$line_col[which(raw_data_summary$subject == k & (raw_data_summary[,pred] == cond_left | raw_data_summary[,pred] == cond_right))] <- rep("#bb7676")
            }
          } else {
            if(tmp_sum_dat$plot_outcome[which(tmp_sum_dat[,pred] == cond_left)] > tmp_sum_dat$plot_outcome[which(tmp_sum_dat[,pred] == cond_right)]){
              raw_data_summary$line_col[which(raw_data_summary$subject == k & (raw_data_summary[,pred] == cond_left | raw_data_summary[,pred] == cond_right))] <- rep("#bb7676")
            } else {
              raw_data_summary$line_col[which(raw_data_summary$subject == k & (raw_data_summary[,pred] == cond_left | raw_data_summary[,pred] == cond_right))] <- rep("#78cc78") 
            }
          }
        }
        # } else {
        #   cond_left <- as.character(tmp_cedat[i,pred])
        #   cond_right <- as.character(tmp_cedat[i+1,pred])
        #   if(line_preds[i] == " "){
        #     next
        #   } else if(line_preds[i] == ">"){

        #   if(tmp_sum_dat$plot_outcome[which(tmp_sum_dat[,2] == paste0(as.character(tmp_cedat[,1])[1], ":go"))] > tmp_sum_dat$plot_outcome[which(tmp_sum_dat[,2] == paste0(as.character(tmp_cedat[,1])[1], ":nogo"))]){
        #           raw_data_summary$line_col[which(raw_data_summary$subject == k & grepl(as.character(tmp_cedat[,1])[1], raw_data_summary[,2], fixed = TRUE))] <- rep( "#bb7676")
        #         } else{
        #           raw_data_summary$line_col[which(raw_data_summary$subject == k & grepl(as.character(tmp_cedat[,1])[1], raw_data_summary[,2], fixed = TRUE))]   <- rep("#78cc78")
        #         }
        #         if(tmp_sum_dat$plot_outcome[which(tmp_sum_dat[,2] == paste0(as.character(tmp_cedat[,1])[3], ":go"))] > tmp_sum_dat$plot_outcome[which(tmp_sum_dat[,2] == paste0(as.character(tmp_cedat[,1])[3], ":nogo"))]){
        #           raw_data_summary$line_col[which(raw_data_summary$subject == k & grepl(as.character(tmp_cedat[,1])[3], raw_data_summary[,2], fixed = TRUE))] <- rep( "#bb7676")
        #         } else{
        #           raw_data_summary$line_col[which(raw_data_summary$subject == k & grepl(as.character(tmp_cedat[,1])[3], raw_data_summary[,2], fixed = TRUE))]   <- rep("#78cc78")
        #         }
        #       } else {
        #         if(tmp_sum_dat$plot_outcome[which(tmp_sum_dat[,2] == "go")] > tmp_sum_dat$plot_outcome[which(tmp_sum_dat[,2] == "nogo")]){
        #           raw_data_summary$line_col[which(raw_data_summary$subject == k)] <- rep( "#bb7676")
        #         } else{
        #           raw_data_summary$line_col[which(raw_data_summary$subject == k)]  <- rep("#78cc78")
        #         }
        #       }
        #           }
          
        # }
      }
      } else {
    warning("line_preds argument must have length of tmp_cedat - 1")
  }




  emm_sub_list <- lapply(1:dim(emm_sub)[2], function(x) emm_sub[,x])
  names(emm_sub_list) <- as.character(unique(raw_data[,pred]))
  emm_sub_df <- data.frame()
  for (i in 1:length(emm_sub_list)) {
    # if(length(emm_sub_list) > nrow(tmp_cedat)){
      i_s <- rep(match(tmp_cedat[,eval(pred)], hpdi_dat[,eval(pred)]), each = length(emm_sub_list)/nrow(tmp_cedat))
      temp_df <- data.frame(value = emm_sub_list[[i]], plot_pred = i_s[i])
    # } else {
    # temp_df <- data.frame(value = emm_sub_list[[i]], plot_pred = i)
    # }
    emm_sub_df <- rbind(emm_sub_df, temp_df)
}


# if(length(emm_sub_list) > nrow(tmp_cedat)){
#   # combine columns that have the same value for emm_sub_list into a single column with the average of each row
#   for(i in 1:nrow(emm_sub_df)){

#   } 
#   names(emm_sub_df) <- paste(pred)
# } else {

# }

names(emm_sub_df)[2] <- paste(pred)
# names(emm_sub_df)[which(names(emm_sub_df) == "tmp_predname")] <- eval(pred)
  # match the lower and upper bounds of the HPDI to the respective tmp_cedat row
  tmp_cedat$lower_HPD <- hpdi_dat$lower.HPD[match(tmp_cedat[,eval(pred)], hpdi_dat[,eval(pred)])] 
  tmp_cedat$upper_HPD <- hpdi_dat$upper.HPD[match(tmp_cedat[,eval(pred)], hpdi_dat[,eval(pred)])] 

  if(is.null(pred_data)){
    
    if(!is.null(int_var_cond)){
      conditions <- make_conditions(m, eval(int_var_cond))
      tmp_preddat <- plot(conditional_effects(m, method = "predict", conditions = conditions, robust = TRUE), plot = F)[[pred]]$data
      tmp_preddat$group <- with(tmp_preddat, paste(tmp_preddat[,1], tmp_preddat[,2], sep="_")) 
      average_tmp_preddat <- aggregate(tmp_preddat[, which(names(tmp_preddat) %in% c("estimate__", "se__", "lower__" , "upper__"))], by=list(tmp_preddat$group), FUN=mean, na.rm = TRUE)
      tmp_preddat <- tmp_preddat[, -which(names(tmp_preddat) %in% c("estimate__", "se__", "lower__" , "upper__", eval(cluster_name)))]
      tmp_preddat_unique <- tmp_preddat[!duplicated(tmp_preddat$group), ]
      tmp_preddat_final <- merge(tmp_preddat_unique, average_tmp_preddat, by.x = "group", by.y = "Group.1", all.x = TRUE)
      tmp_preddat_final <- tmp_preddat_final[, -which(names(tmp_preddat_final) %in% c("group"))]
      tmp_preddat <- tmp_preddat_final[,-4]

    } else {
      tmp_preddat <- plot(conditional_effects(m, method = "predict", robust = TRUE), plot = F)[[pred]]$data
    }
    tmp_preddat$q25 <- unlist(ddply(raw_data, .(get(pred)), function(x) quantile(x$plot_outcome, 0.25))[2])
    tmp_preddat$q75 <- unlist(ddply(raw_data, .(get(pred)), function(x) quantile(x$plot_outcome, 0.75))[2])
  } else {
    tmp_preddat <- pred_data
    tmp_preddat$q25 <- tmp_preddat[,3]
    tmp_preddat$q75 <- tmp_preddat[,4]
  }
  tmp_preddat[,pred] <- as.character(tmp_cedat[,pred])
  if(trunc_pred){
    tmp_preddat$upper__ <- sapply(1:nrow(tmp_preddat), function(x) ifelse(tmp_preddat$upper_[x] > trunc_pred_max, trunc_pred_max, tmp_preddat$upper_[x]))
    tmp_preddat$lower__ <- sapply(1:nrow(tmp_preddat), function(x) ifelse(tmp_preddat$lower_[x] < trunc_pred_min, trunc_pred_min, tmp_preddat$lower_[x]))
  }


  

  x_labels <- as.character(unique(tmp_cedat[,eval(pred)]))
  if(!(exists("aspect_ratio"))){
    aspect_ratio <- ifelse(nrow(tmp_cedat) > 2, 1/1, 2/1)
  }
  p <- ggplot(aes(x = .data[[pred]], y = estimate__, group = 1), data = droplevels(tmp_cedat)) + 
    geom_beeswarm(data = droplevels(raw_data_summary), aes(x = .data[[pred]], y = plot_outcome, group = .data[[pred]]), color = "#4891ac", alpha = .8, size = 1.2)

    if(grepl(":", pred, fixed = TRUE)){
      p <- p + geom_line(data = droplevels(raw_data_summary[which(raw_data_summary$pred1 == unique(raw_data_summary$pred1)[1]),]), aes(y = plot_outcome, x = .data[[pred]], group=subject, color = line_col), alpha = 0.3) +
      geom_line(data = droplevels(raw_data_summary[which(raw_data_summary$pred1 == unique(raw_data_summary$pred1)[2]),]), aes(y = plot_outcome, x = .data[[pred]], group=subject, color = line_col), alpha = 0.3)
    } else {
      p <- p + geom_line(data = droplevels(raw_data_summary), aes(y = plot_outcome, x = .data[[pred]], group=subject, color = line_col), alpha = 0.3)
    }
  p <- p + scale_color_identity() +
  # stat_summary(data = droplevels(raw_data_summary), aes(x = .data[[pred]], y = plot_outcome), 
  #                fun.y = mean,fun.ymin = mean, fun.ymax =mean, geom = "crossbar", width = 0.05, size = 0.5, color = "#4891ac")+
  #   stat_summary(data = droplevels(raw_data_summary), aes(.data[[pred]], y = plot_outcome),fun.y = function(z) { quantile(z,0.25) },fun.ymin = function(z) { quantile(z,0.25) }, fun.ymax = function(z) { quantile(z,0.25) }, 
  #                geom = "crossbar", width = 0.05, size = 0.3, color = "#4891ac", linetype = 1) +
  #   stat_summary(data = droplevels(raw_data_summary), aes(.data[[pred]], y = plot_outcome),fun.y = function(z) { quantile(z,0.75) },fun.ymin =function(z) { quantile(z,0.75) }, fun.ymax = function(z) { quantile(z,0.75) }, 
  #                geom = "crossbar", width = 0.05, size = 0.3, color = "#4891ac", linetype = 1) +
    geom_violinhalf(data = droplevels(emm_sub_df), aes(y = value, x = .data[[pred]], group = .data[[pred]]), fill="black", alpha=0.3, scale = "area", scale_factor = viol_width) +
    geom_errorbar(aes(ymin = lower_HPD, ymax = upper_HPD), width = 0.2, size = 1) +
    labs(x = xname, y = yname, title = title) + 
     geom_point(size = 2) + 
    #  geom_line(size = 1.2) +
         theme_bw() + coord_cartesian(xlim = c(1.4, nrow(tmp_cedat)-0.4)) + theme(plot.margin = margin(0, 0, 0, 0), aspect.ratio = aspect_ratio, panel.border = element_rect(colour = "black", fill=NA, size=1))
  if (add_pred) {
    p <- p +  geom_line(size = 0.8, data = droplevels(tmp_preddat), aes(x = .data[[pred]], y = lower__), linetype = 2) +  
      geom_line(size = 0.8, data = droplevels(tmp_preddat), aes(x = .data[[pred]], y = upper__), linetype = 2)
      # if (!(ylim == "manual")) {
        ylim_min_pred = min(tmp_preddat$lower__)-ylim_adj
        ylim_max_pred = max(tmp_preddat$upper__)+ylim_adj
      #   p <- p + ylim(min(tmp_preddat$lower__)-ylim_adj, max(tmp_preddat$upper__)+ylim_adj) 
      # }
  } else {
    ylim_min_pred = NA
    ylim_max_pred = NA
  }
  if (ylim == "manual"){
    p <- p + ylim(get("ylim_min"), get("ylim_max")) 
  } else {
    if(!is.na(ylim_min_pred)){
      ylim_min = min(raw_data_summary$plot_outcome)
      ylim_max = max(raw_data_summary$plot_outcome)
      ylim_min <- ifelse(ylim_min_pred < ylim_min, ylim_min_pred, ylim_min)
      ylim_max <- ifelse(ylim_max_pred > ylim_max, ylim_max_pred, ylim_max)
    } else {
      ylim_min = min(raw_data_summary$plot_outcome)
      ylim_max = max(raw_data_summary$plot_outcome)
    }

  }
  if (exists("x_ticks")){
    p <- p + scale_x_discrete(labels = x_ticks, guide = guide_axis(angle = x_angle))
  }
  if(add_zoom == FALSE){
      return(p)
  } else {
    ymin_zoom = min(emm_sub_df$value)
    ymax_zoom = max(emm_sub_df$value)
    p <- p + 
    # geom_rect(data = tmp_cedat, aes(xmin = 0.8,
    #                                     xmax = 2.2,
    #                                     ymin = ymin_zoom, ymax = ymax_zoom), fill = alpha("grey",0), color = alpha("#ada4a4", 0.5), size = 1.2, linetype = 2)+ coord_cartesian(xlim = c(1.4, 1.6)) + theme(plot.margin = margin(0, 0, 0, 0), aspect.ratio = 2/1)
    geom_hline(yintercept = ymin_zoom, linetype = 2, color = alpha("#ada4a4", 0.5), size = 1.2) +
    geom_hline(yintercept = ymax_zoom, linetype = 2, color = alpha("#ada4a4", 0.5), size = 1.2) 

    pz <- ggplot(aes(x = .data[[pred]], y = estimate__, group = 1), data = droplevels(tmp_cedat))+ 
    geom_violinhalf(data = droplevels(emm_sub_df), aes(y = value, x = .data[[pred]], group = .data[[pred]]), fill="black", alpha=0.3, scale = "area", scale_factor = 1) + 
    scale_x_discrete(guide = guide_axis(n.dodge=2)) +
    geom_errorbar(aes(ymin = lower_HPD, ymax = upper_HPD), width = 0.5, size = 1) +
     geom_point(size = 2) +
    #  geom_line(size = 1.2) + 
     scale_y_continuous(limits = c(min(emm_sub_df$value), max(emm_sub_df$value)), breaks = as.integer(seq(round(min(emm_sub_df$value),0), round(max(emm_sub_df$value),0), length.out = 10)))+
         theme_bw() + theme(plot.margin = margin(0, 0, 0, 10), axis.title.x=element_blank(), 
        axis.title.y=element_blank(), aspect.ratio = ifelse(nrow(tmp_cedat) > 2, 2/1, 4/1),panel.border = element_rect(alpha("#ada4a4", 0.5), fill=NA, size=1.2, linetype = 2), plot.background = element_rect(fill =alpha("#ada4a4", 0), color = alpha("#ada4a4", 0)))

  # adjust the distance between plots
  # print(ylim_min)
  # print(ylim_max)
  zoom_funnel <- scales::rescale(c(ymin_zoom, ymax_zoom), from = c(ylim_min, ylim_max), to = c(0, 1))
  zoom_funnel_lower <- zoom_funnel[1]
  zoom_funnel_upper <- zoom_funnel[2]

  adj_x <- 0.15 * abs(2-max(c(nchar(toString(round(ymin_zoom, 0))), nchar(toString(round(ymax_zoom, 0))))))
  zoom_up <- ggplot(data = data.frame(x = c(1, 2),
                                     y = c(1, 2)),
                   aes(x=x, y=y)) +
  geom_path(linetype = 2, color = alpha("#ada4a4", 0.5), size = 1.2) +
  theme_void()


zoom_down <- ggplot(data = data.frame(x = c(1, 2),
                                     y = c(2, 1)),
                   aes(x=x, y=y)) +
  geom_path(linetype = 2, size = 1.2, color = alpha("#ada4a4", 0.5)) +
  theme_void()

  p_pz <- p+pz +inset_element(
  zoom_up, left = (-0.18-adj_x), bottom = zoom_funnel_upper-0.02, right =0.01, top = 1.02) + inset_element(
  zoom_down, left = (-0.18-adj_x), bottom = -0.02, right = 0.01, top = zoom_funnel_lower+0.02) 
  return(p_pz)
  }

    
}


plot_monotonic_effect <- function(m, raw_data = NULL, pred = NULL, outcome = NULL, xname = NULL, yname = NULL, title, add_pred = TRUE,
                                  ylim_adj = 10, point_color = "#4891ac", ylim = "auto", ylim_min = NA, ylim_max = NA, pred_data = NULL){
    
  if(is.null(raw_data)){
    raw_data <- m$data
  }
  tmp_cedat <- plot(conditional_effects(m, robust = TRUE), plot = F)[[pred]]$data
  raw_data$plot_outcome <- unlist(raw_data[which(names(raw_data) == outcome)])
  raw_data$plot_pred <- as.numeric(mapvalues(unlist(raw_data[which(names(raw_data) == pred)]), 
                                             from = levels(unlist(raw_data[which(names(raw_data) == pred)])), 
                                             to =1:nrow(tmp_cedat)))
  
  if(is.null(pred_data)){
    tmp_preddat <- plot(conditional_effects(m, method = "predict", robust = TRUE), plot = F)[[pred]]$data
    tmp_preddat$q25 <- unlist(ddply(raw_data, .(get(pred)), function(x) quantile(x$plot_outcome, 0.25))[2])
    tmp_preddat$q75 <- unlist(ddply(raw_data, .(get(pred)), function(x) quantile(x$plot_outcome, 0.75))[2])
  } else {
    tmp_preddat <- pred_data
    tmp_preddat$q25 <- tmp_preddat[,3]
    tmp_preddat$q75 <- tmp_preddat[,4]
  }
  

  

  x_labels <- as.numeric(as.character(sort(unique(raw_data[,pred]))))
  x_labels = ifelse(x_labels %in% c(14,19, 29), x_labels + 1, ifelse(x_labels == 38, x_labels + 2, x_labels))
  p <- ggplot(aes(x = factor(1:nrow(tmp_cedat)), y = estimate__, group = 1), data = tmp_cedat) + 
    geom_beeswarm(data = raw_data, aes(x = factor(plot_pred), y = plot_outcome, group = factor(plot_pred)), color = "#4891ac", alpha = .2) +
    geom_point(size = 2) + geom_line(size = 1.2) + geom_ribbon(aes(ymin=lower__, ymax=upper__), linetype=2, alpha=0.3) + 
    labs(x = xname, y = yname, title = title) + scale_x_discrete(labels= x_labels) +
    stat_summary(data = raw_data, aes(x = factor(plot_pred), y = plot_outcome), 
                 fun.y = mean,fun.ymin = mean, fun.ymax = mean, geom = "crossbar", width = 0.5, size = 0.5, color = "#4891ac")+
    stat_summary(data = tmp_preddat, aes(factor(1:nrow(tmp_preddat)), y = q25),fun.y = mean,fun.ymin = mean, fun.ymax = mean, 
                 geom = "crossbar", width = 0.5, size = 0.3, color = "#4891ac", linetype = 1) +
    stat_summary(data = tmp_preddat, aes(factor(1:nrow(tmp_preddat)), y = q75),fun.y = mean,fun.ymin = mean, fun.ymax = mean, 
                 geom = "crossbar", width = 0.5, size = 0.3, color = "#4891ac", linetype = 1) +
    theme_bw()
  if (add_pred) {
    p <- p +  geom_line(size = 0.8, data = tmp_preddat, aes(x = factor(1:nrow(tmp_preddat)), y = lower__), linetype = 2) +  
      geom_line(size = 0.8, data = tmp_preddat, aes(x = factor(1:nrow(tmp_preddat)), y = upper__), linetype = 2)
      if (!(ylim == "manual")) {
        p <- p + ylim(min(tmp_preddat$lower__)-ylim_adj, max(tmp_preddat$upper__)+ylim_adj) 
      }
  }
  if (ylim == "manual"){
    p <- p + ylim(get("ylim_min"), get("ylim_max")) 
  }
  return(p)
    
}

plot_numeric_effect <- function(m, raw_data = NULL, pred = NULL, outcome = NULL, xname = NULL, yname = NULL, title, add_pred = TRUE,
                                ylim_adj = 10, uncenter = TRUE, uncenter_correction = 0, point_color = "#4891ac", ylim = "auto", ylim_min = NA, ylim_max = NA, 
                                crop_y = FALSE, crop_y_min = NA, crop_y_max = NA, plot_raw_data = TRUE, pred_data = NULL, add_plots = NULL){
  
  if(is.null(raw_data)){
    raw_data <- m$data
  }
  
  tmp_cedat <- plot(conditional_effects(m, robust = TRUE), plot = F)[[pred]]$data
  
  if(is.null(pred_data)){
    tmp_preddat <- plot(conditional_effects(m, method = "predict", robust = TRUE), plot = F)[[pred]]$data
  }
  else{
    tmp_preddat <- pred_data
  }
  
  raw_data$plot_outcome <- unlist(raw_data[which(names(raw_data) == outcome)])
  if(uncenter == TRUE){
    raw_data$plot_pred <- unlist(raw_data[which(names(raw_data) == pred)])+abs(min((raw_data[which(names(raw_data) == pred)])))+uncenter_correction
    tmp_cedat$plot_pred <- unlist(tmp_cedat[which(names(tmp_cedat) == pred)])+abs(min((tmp_cedat[which(names(tmp_cedat) == pred)])))+uncenter_correction
    tmp_preddat$plot_pred <- unlist(tmp_preddat[which(names(tmp_preddat) == pred)])+abs(min((tmp_preddat[which(names(tmp_preddat) == pred)])))+uncenter_correction
  } else {
    raw_data$plot_pred <- unlist(raw_data[which(names(raw_data) == pred)])
    tmp_cedat$plot_pred <- unlist(tmp_cedat[which(names(tmp_cedat) == pred)])
    tmp_preddat$plot_pred <- unlist(tmp_preddat[which(names(tmp_preddat) == pred)])
  }
  y_med <- median(raw_data[,which(names(raw_data) == outcome)])
  y_q25 <- unname(quantile(raw_data[,which(names(raw_data) == outcome)], 0.25))
  y_q75 <- unname(quantile(raw_data[,which(names(raw_data) == outcome)], 0.75))
  p <- ggplot(aes(x = plot_pred, y = estimate__), data = tmp_cedat) + geom_line(size = 1.2)
  if(!is.null(add_plots)){
    for(i in 1:length(add_plots))
      p <- p+add_plots[i]
  }
  p <- p + geom_ribbon(aes(ymin=lower__, ymax=upper__), linetype=2, alpha=0.3) + 
  labs(x = xname, y = yname, title = title) + 
  # geom_hline(yintercept = y_med, size = 0.5, color = "#4891ac") +
  # geom_hline(yintercept = y_q25, size = 0.3, color = "#4891ac") +
  # geom_hline(yintercept = y_q75, size = 0.3, color = "#4891ac") +
  theme_bw()
  if (plot_raw_data){
      p <- p + geom_point(aes(x = plot_pred, y = plot_outcome), data = raw_data, size = 2, color = point_color, alpha = .2)
  }
  if (add_pred) {
    p <- p +  geom_line(size = 0.8, data = tmp_preddat, aes(x = plot_pred, y = lower__), linetype = 2) +  
      geom_line(size = 0.8, data = tmp_preddat, aes(x = plot_pred, y = upper__), linetype = 2) 
    if (!(ylim == "manual")) {
      p <- p + ylim(min(tmp_preddat$lower__)-ylim_adj, max(tmp_preddat$upper__)+ylim_adj) 
    }
  }
  if (ylim == "manual") {
    p <- p + ylim(get("ylim_min"), get("ylim_max")) 
  }
  if (crop_y == TRUE) {
    p <- p + coord_cartesian(ylim = c(crop_y_min, crop_y_max))
    
  }
  
  return(p)
}


load_brm_chkpt <- function(path){
  
  cmd_fit_path <- paste0(path, "/cmd_fit/")
  cmd_files <- list.files(cmd_fit_path)
  cmd_file_nums <- unlist(regmatches(cmd_files, gregexpr('\\(?[0-9,.]+', cmd_files)))
  cmd_file_nums <- as.numeric(gsub('\\(', '-', gsub(',', '', cmd_file_nums)))
  cmd_file_max <- cmd_files[which(cmd_file_nums == max(cmd_file_nums))]
  cmd_extracted <- readRDS(eval(paste0(cmd_fit_path, cmd_file_max)))
  
  
  args_extracted <- readRDS(paste0(path, "/stan_model/", "args.rds"))
  
  brm_extracted <- make_brmsfit(cmd_extracted@stanmodel, args_extracted$formula, 
                                args_extracted$data, path = args_extracted$path)
  
  return(brm_extracted)  
  
}



plot_brm_hypotheses <- function(hypothesis_list = list(), equiv_bound = c(), dens_scale_factor = 1, ...){
  
  # extract hypothesis data objects
  data_list <- lapply(hypothesis_list, function(x){x$samples[,1]})

  d_out <- data.frame()
  add_args <- list(...)
  f_env <- environment()
  list2env(add_args, f_env)
  if(!exists("fill_colors")){
    fill_colors <- c("#488558", "#73b387", "#61ac27", "#538736", "#0e2d1e", "#0f1b15")
  }
  if(!exists("label_size")){
    label_size <- 9
  }


  for(i in 1:length(data_list)){
    tmp_dat <- data_list[[i]]
    tmp_dat_dens <- density(tmp_dat)
    tmp_dat_dens <- data.frame(cbind(x = tmp_dat_dens[[1]], y = tmp_dat_dens[[2]]))
    tmp_dat_dens$y_sw <- tmp_dat_dens$y/(max(tmp_dat_dens$y)*dens_scale_factor)
    tmp_dat_dens$ind <- rep(hypothesis_list[[i]]$hypothesis$Hypothesis)
    tmp_dat_dens$zeroline <- rep(0)
    for(j in 1:nrow(tmp_dat_dens)){
      tmp_dat_dens$ci_fill[j] <- ifelse(j/nrow(tmp_dat_dens) >= .10 & j/nrow(tmp_dat_dens) <= .90, tmp_dat_dens$y[j], 0)
      tmp_dat_dens$ci_fill_sw[j] <- ifelse(j/nrow(tmp_dat_dens) >= .05 & j/nrow(tmp_dat_dens) <= .95, tmp_dat_dens$y_sw[j], 0)

    }
    dens_hdi <- hdi(tmp_dat, credMass = .90)
    tmp_dat_dens$hdi_fill <- ifelse(tmp_dat_dens$x > dens_hdi[1] & tmp_dat_dens$x < dens_hdi[2], tmp_dat_dens$y, 0)
    d_out <- rbind(d_out, tmp_dat_dens)
  }

  d_out$y_s <- d_out$y/(max(d_out$y)*dens_scale_factor)
  d_out$hdi_fill_s <- d_out$hdi_fill/(max(d_out$hdi_fill)*dens_scale_factor)
  d_out$ind <- factor(d_out$ind, levels = unique(d_out$ind))
  
  if(!exists("x_min")){
    x_min = -max(abs(d_out$x))-0.1*max(abs(d_out$x))
  }
  if(!("x_max" %in% names(add_args))){
    x_max = max(abs(d_out$x))+0.1*max(abs(d_out$x))
  }
  
  
  p_return <- ggplot(d_out)+ geom_area(aes(x, y = hdi_fill_s), alpha = .60) + 
  geom_line(aes(x = x, y = y_s)) + geom_line(aes(x = x, y = zeroline)) +
    facet_wrap(~ind, scales = "fixed", ncol = 1, strip.position = "left") +
    scale_x_continuous(limits = c(x_min, x_max), breaks = round(seq(x_min, x_max, length.out = 5),2)) +
    scale_y_continuous(breaks = c(), expand = c(0.01,0.01)) + 
    geom_vline(xintercept = 0)
  if(!is.null(equiv_bound)){
    p_return <- p_return + geom_vline(xintercept = equiv_bound[1], linetype="dashed")
    p_return <- p_return + geom_vline(xintercept = equiv_bound[2], linetype="dashed")
  }
  p_return <- p_return + labs(x = "Estimate size", y = "Estimate name") +
    theme_apa() + 
    # theme( panel.spacing=unit(-.01, "lines"),
    #                      strip.text.y.left = element_text(vjust = -0.007, hjust = -1, angle = 0), 
    #                      legend.position = "none") +
    theme( panel.spacing=unit(-.01, "lines"),
                         strip.text.y.left = element_blank(), 
                         legend.position = "none",  plot.margin = margin(0,0,0,0)) + 
    geom_text(aes(label = ind), x = x_min, y = 0.2, hjust = "left", check_overlap = TRUE, size = label_size/.pt)+
    aes(fill = ind) + scale_fill_manual(values = fill_colors)
  
  return(p_return)
  
}
