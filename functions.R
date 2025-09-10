#data generation - STRONG MISSPECIFICATION
simulate_dataset_strong = function(n, M_fixed,
                                   alpha, beta,
                                   lambdaC,
                                   gamma = 1.2,
                                   landmark_time = 3,
                                   max_time = 10,
                                   mean_CURE = rep(0, length(alpha) - 1L),
                                   sd_CURE = rep(1, length(alpha) - 1L),
                                   cor_CURE = diag(length(alpha) - 1L),
                                   seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  
  p_cure = length(alpha) - 1L
  p_surv = length(beta)
  
  cov_CURE = diag(sd_CURE) %*% cor_CURE %*% diag(sd_CURE)
  X = MASS::mvrnorm(n, mu = mean_CURE, Sigma = cov_CURE)
  
  all_data = list()
  
  for (i in 1:n) {
    times_i = sort(runif(M_fixed, 0, landmark_time))
    
    X_i = X[i, ]
    linpred_cure = alpha[1] + sum(alpha[-1] * X_i)
    p_i = 1 / (1 + exp(-linpred_cure))
    cured_i = rbinom(1, 1, p_i)
    
    # Simula Z fino al landmark time
    Z_i = matrix(NA, nrow = M_fixed, ncol = p_surv)
    for (j in 1:p_surv) {
      intercept = rnorm(1, 0, 1)
      slope = rnorm(1, 0, 0.7)
      Z_i[, j] = intercept + slope * times_i + rnorm(M_fixed, 0, 1)
    }
    
    # Simula tempo evento da landmark_time in poi
    if (cured_i == 1) {
      # Un solo tempo di predizione: landmark_time
      Z_landmark = Z_i[which.max(times_i), ]
      linpred_surv = sum(Z_landmark * beta)
      
      u = runif(1)
      T_star = (-log(u) / exp(linpred_surv))^(1 / gamma)
      event_time = landmark_time + T_star
    } else {
      event_time = Inf
    }
    
    # Censura da landmark_time
    censor_time = landmark_time + rexp(1, rate = lambdaC)
    obs_time = min(event_time, censor_time, max_time)
    status = as.integer(event_time <= censor_time & event_time <= max_time)
    
    df_i = data.frame(
      id = rep(i, M_fixed),
      time = times_i,
      obs_time = rep(obs_time, M_fixed),
      status = rep(status, M_fixed),
      cured = rep(cured_i, M_fixed)
    ) %>%
      dplyr::filter(time <= landmark_time)
    
    if (nrow(df_i) > 0) {
      df_i = cbind(df_i, Z_i[1:nrow(df_i), , drop = FALSE])
      df_i = cbind(df_i, matrix(rep(X_i, each = nrow(df_i)), ncol = length(X_i)))
      colnames(df_i)[6:(5 + p_surv)] = paste0("Z", 1:p_surv)
      colnames(df_i)[(6 + p_surv):(5 + p_surv + p_cure)] = paste0("X", 1:p_cure)
      
      all_data[[length(all_data) + 1]] = df_i
    }
  }
  
  data_long = bind_rows(all_data)
  return(data_long)
}



#data generation - MILD MISSPECIFICATION
simulate_dataset_mild = function(n, M_fixed,
                                 alpha, beta,
                                 lambdaC,
                                 gamma = 1.2,
                                 landmark_time = 3,
                                 max_time = 10,
                                 mean_CURE = rep(0, length(alpha) - 1L),
                                 sd_CURE = rep(1, length(alpha) - 1L),
                                 cor_CURE = diag(length(alpha) - 1L),
                                 seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  
  p_cure = length(alpha) - 1L
  p_surv = length(beta)
  
  cov_CURE = diag(sd_CURE) %*% cor_CURE %*% diag(sd_CURE)
  X = MASS::mvrnorm(n, mu = mean_CURE, Sigma = cov_CURE)
  
  all_data = list()
  
  for (i in 1:n) {
    times_i = sort(runif(M_fixed, 0, landmark_time))
    
    X_i = X[i, ]
    linpred_cure = alpha[1] + sum(alpha[-1] * X_i)
    p_i = 1 / (1 + exp(-linpred_cure))
    cured_i = rbinom(1, 1, p_i)
    
    # Simula Z fino al landmark time
    Z_i = matrix(NA, nrow = M_fixed, ncol = p_surv)  
    Z_i_deterministic = matrix(NA, nrow = M_fixed, ncol = p_surv)  
    
    for (j in 1:p_surv) {
      intercept = rnorm(1, 0, 1)
      slope = rnorm(1, 0, 0.7)
      mean_trend = intercept + slope * times_i
      
      Z_i_deterministic[, j] = mean_trend
      Z_i[, j] = mean_trend + rnorm(M_fixed, 0, 1)
    }
    
    
    # Simula tempo evento da landmark_time in poi
    if (cured_i == 1) {
      # Un solo tempo di predizione: landmark_time
      Z_landmark = Z_i_deterministic[which.max(times_i), ]
      linpred_surv = sum(Z_landmark * beta)
      
      u = runif(1)
      T_star = (-log(u) / exp(linpred_surv))^(1 / gamma)
      event_time = landmark_time + T_star
    } else {
      event_time = Inf
    }
    
    # Censura da landmark_time
    censor_time = landmark_time + rexp(1, rate = lambdaC)
    obs_time = min(event_time, censor_time, max_time)
    status = as.integer(event_time <= censor_time & event_time <= max_time)
    
    df_i = data.frame(
      id = rep(i, M_fixed),
      time = times_i,
      obs_time = rep(obs_time, M_fixed),
      status = rep(status, M_fixed),
      cured = rep(cured_i, M_fixed)
    ) %>%
      dplyr::filter(time <= landmark_time)
    
    if (nrow(df_i) > 0) {
      df_i = cbind(df_i, Z_i[1:nrow(df_i), , drop = FALSE])
      df_i = cbind(df_i, matrix(rep(X_i, each = nrow(df_i)), ncol = length(X_i)))
      colnames(df_i)[6:(5 + p_surv)] = paste0("Z", 1:p_surv)
      colnames(df_i)[(6 + p_surv):(5 + p_surv + p_cure)] = paste0("X", 1:p_cure)
      
      all_data[[length(all_data) + 1]] = df_i
    }
  }
  
  data_long = bind_rows(all_data)
  return(data_long)
}


#data generation - TRUE MODEL
simulate_dataset_true = function(n, M_fixed,
                                 alpha, beta,
                                 lambdaC,
                                 gamma = 1.2,
                                 landmark_time = 3,
                                 max_time = 10,
                                 mean_CURE = rep(0, length(alpha) - 1L),
                                 sd_CURE = rep(1, length(alpha) - 1L),
                                 cor_CURE = diag(length(alpha) - 1L),
                                 seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  
  p_cure = length(alpha) - 1L
  p_surv = length(beta)
  
  cov_CURE = diag(sd_CURE) %*% cor_CURE %*% diag(sd_CURE)
  X = MASS::mvrnorm(n, mu = mean_CURE, Sigma = cov_CURE)
  
  all_data = list()
  
  for (i in 1:n) {
    times_i = sort(runif(M_fixed, 0, landmark_time))
    
    X_i = X[i, ]
    linpred_cure = alpha[1] + sum(alpha[-1] * X_i)
    p_i = 1 / (1 + exp(-linpred_cure))
    cured_i = rbinom(1, 1, p_i)
    
    # Simula Z fino al landmark time
    Z_i = matrix(NA, nrow = M_fixed, ncol = p_surv)
    Z_random_effects = matrix(NA, nrow = 2, ncol = p_surv)  
    
    for (j in 1:p_surv) {
      intercept = rnorm(1, 0, 1)
      slope = rnorm(1, 0, 0.7)
      Z_random_effects[, j] = c(intercept, slope)
      Z_i[, j] = intercept + slope * times_i + rnorm(M_fixed, 0, 1)  
    }
    
    # Simula tempo evento da landmark_time in poi
    if (cured_i == 1) {
      # Un solo tempo di predizione: landmark_time
      Z_landmark = Z_random_effects[1, ] + Z_random_effects[2, ] * landmark_time
      linpred_surv = sum(Z_landmark * beta)
      
      u = runif(1)
      T_star = (-log(u) / exp(linpred_surv))^(1 / gamma)
      event_time = landmark_time + T_star
    } else {
      event_time = Inf
    }
    
    # Censura da landmark_time
    censor_time = landmark_time + rexp(1, rate = lambdaC)
    obs_time = min(event_time, censor_time, max_time)
    status = as.integer(event_time <= censor_time & event_time <= max_time)
    
    df_i = data.frame(
      id = rep(i, M_fixed),
      time = times_i,
      obs_time = rep(obs_time, M_fixed),
      status = rep(status, M_fixed),
      cured = rep(cured_i, M_fixed)
    ) %>%
      dplyr::filter(time <= landmark_time)
    
    if (nrow(df_i) > 0) {
      df_i = cbind(df_i, Z_i[1:nrow(df_i), , drop = FALSE])
      df_i = cbind(df_i, matrix(rep(X_i, each = nrow(df_i)), ncol = length(X_i)))
      colnames(df_i)[6:(5 + p_surv)] = paste0("Z", 1:p_surv)
      colnames(df_i)[(6 + p_surv):(5 + p_surv + p_cure)] = paste0("X", 1:p_cure)
      
      all_data[[length(all_data) + 1]] = df_i
    }
  }
  
  data_long = bind_rows(all_data)
  return(data_long)
}



#############################################################################################
#############################################################################################



#sensitivity and specificity
compute_sen_spe = function(c, O_hat, q_hat) {
  sen_hat = sum(q_hat * (O_hat > c)) / sum(q_hat)
  spe_hat = sum((1 - q_hat) * (O_hat < c)) / sum(1 - q_hat)
  return(c(sen_hat, spe_hat))
}



#############################################################################################
#############################################################################################



predict_random_effects_manual <- function(lmm, newdata, id_var = "id") {
  require(nlme)
  
  # Estrazione parametri dal modello
  D_hat <- getVarCov(lmm, type = "random.effects") # matrice di varianza RE
  beta_hat <- fixef(lmm)                           # effetti fissi
  sigma2_hat <- summary(lmm)$sigma^2               # varianza residuo
  
  # Formula effetti fissi e random
  X_form <- formula(lmm$call$fixed)
  Z_form <- formula(lmm$modelStruct$reStruct[[1]])
  
  ids <- unique(newdata[[id_var]])
  res <- matrix(NA, nrow = length(ids), ncol = ncol(as.matrix(D_hat)))
  rownames(res) <- ids
  
  for (i in seq_along(ids)) {
    df_i <- newdata[newdata[[id_var]] == ids[i], ]
    if (nrow(df_i) == 0) next
    
    yi <- model.response(model.frame(X_form, data = df_i))  # outcome
    Xi <- model.matrix(X_form, data = df_i)
    Zi <- model.matrix(Z_form, data = df_i)
    
    if (any(is.na(yi))) {
      non_missing <- which(!is.na(yi))
      yi <- yi[non_missing]
      Xi <- Xi[non_missing, , drop = FALSE]
      Zi <- Zi[non_missing, , drop = FALSE]
    }
    
    if (length(yi) == 0) {
      res[i, ] <- rep(0, ncol(Zi))  # nessuna osservazione → effetto random = 0
    } else {
      Vi <- Zi %*% D_hat %*% t(Zi) + sigma2_hat * diag(length(yi))
      temp <- yi - Xi %*% beta_hat
      bi <- t(D_hat %*% t(Zi) %*% solve(Vi) %*% temp)
      res[i, ] <- bi
    }
  }
  
  colnames(res) <- colnames(Zi)
  return(as.data.frame(res) %>% tibble::rownames_to_column(id_var))
}




#############################################################################################
#############################################################################################




## Method

cure_pred = function(n, seed=NULL, scenario, type = NULL, 
                     M_fixed,
                     alpha, beta,
                     lambdaC,
                     gamma = 1.2,
                     landmark_time = 3,
                     max_time = 10,
                     mean_CURE = rep(0, length(alpha) - 1L),
                     sd_CURE = rep(1, length(alpha) - 1L),
                     cor_CURE = diag(length(alpha) - 1L)){
  if (is.null(seed))  seed = sample(1:10000, 1)
  set.seed(seed)
  
  
  fun = match.fun(paste0("simulate_dataset_", type))
  
  
  ###################
  # Data generation #
  ###################
  
  if (scenario=="1"){
    dati = fun(n, M_fixed, alpha, beta, lambdaC, seed = seed)
  }
  
  if (scenario=="2"){
    dati = fun(n, M_fixed, alpha, beta, lambdaC, seed = seed)
  }
  
  if (scenario=="3"){
    dati = fun(n, M_fixed, alpha, beta, lambdaC, seed = seed)
    dati = dati %>%
      group_by(id) %>%
      group_modify(~ slice_sample(.x, n = sample(5:10, 1))) %>%
      ungroup()
  }
  
  if (scenario=="4"){
    dati = fun(n, M_fixed, alpha, beta, lambdaC, seed = seed)
  }
  
  if (scenario=="5"){
    dati = fun(n, M_fixed, alpha, beta, lambdaC, seed = seed)
  }
  
  if (scenario=="6"){
    dati = fun(n, M_fixed, alpha, beta, lambdaC, seed = seed)
    dati = dati %>%
      group_by(id) %>%
      group_modify(~ slice_sample(.x, n = sample(5:10, 1))) %>%
      ungroup()
  }
  
  if (scenario=="7"){
    dati = fun(n, M_fixed, alpha, beta, lambdaC, seed = seed)
  }
  
  if (scenario=="8"){
    dati = fun(n, M_fixed, alpha, beta, lambdaC, seed = seed)
  }
  
  if (scenario=="9"){
    dati = fun(n, M_fixed, alpha, beta, lambdaC, seed = seed)
    dati = dati %>%
      group_by(id) %>%
      group_modify(~ slice_sample(.x, n = sample(5:10, 1))) %>%
      ungroup()
  }
  
  if (scenario=="10"){
    dati = fun(n, M_fixed, alpha, beta, lambdaC, seed = seed)
  }
  
  if (scenario=="11"){
    dati = fun(n, M_fixed, alpha, beta, lambdaC, seed = seed)
  }
  
  if (scenario=="12"){
    dati = fun(n, M_fixed, alpha, beta, lambdaC, seed = seed)
    dati = dati %>%
      group_by(id) %>%
      group_modify(~ slice_sample(.x, n = sample(5:10, 1))) %>%
      ungroup()
  }
  
  
  
  ids = unique(dati$id)
  
  train_ids = ids[1:(n/2)]
  test_ids  = ids[(n/2+1):n]
  
  dati_test  = dati[dati$id %in% test_ids, ]
  dati = dati[dati$id %in% train_ids, ]
  
  
  
  #wide format dataset
  df = dati %>%
    group_by(id) %>%
    slice_tail(n = 1) %>%
    ungroup()
  
  df_test = dati_test %>%
    group_by(id) %>%
    slice_tail(n = 1) %>%
    ungroup()
  
  
  
  
  
  ##################
  ##### STEP 1 #####
  ##################
  
  #modelling the longitudinal covariates
  
  dati = dati %>%
    group_by(id) %>%
    mutate(meantime = mean(time, na.rm = TRUE))
  
  lmm1 = lme(Z1 ~ time, 
             data = dati, 
             random = ~ time | id,
             control = lmeControl(opt = "optim"))
  summary(lmm1)
  
  lmm2 = lme(Z2 ~ time, 
             data = dati, 
             random = ~ time | id,
             control = lmeControl(opt = "optim"))
  summary(lmm2)
  
  lmm3 = lme(Z3 ~ time, 
             data = dati, 
             random = ~ time | id,
             control = lmeControl(opt = "optim"))
  summary(lmm3)
  
  lmm4 = lme(Z4 ~ time, 
             data = dati, 
             random = ~ time | id,
             control = lmeControl(opt = "optim"))
  summary(lmm4)
  
  
  
  
  ##################
  ##### STEP 2 #####
  ##################
  
  ## RANDOM EFFECTS - TRAIN SET
  
  #summarizing longitudinal covariates at the landmark time
  random1 = ranef(lmm1)
  random2 = ranef(lmm2)
  random3 = ranef(lmm3)
  random4 = ranef(lmm4)
  
  random1 = random1 %>%
    tibble::rownames_to_column(var = "id")
  random1$id = as.numeric(random1$id)
  
  random1 = random1 %>% rename(random_intercept_Z1 = '(Intercept)',
                               random_slope_Z1 = time)
  
  dati_random = left_join(df, random1, by = "id")
  
  
  random2 = random2 %>%
    tibble::rownames_to_column(var = "id")
  random2$id = as.numeric(random2$id)
  
  random2 = random2 %>% rename(random_intercept_Z2 = '(Intercept)',
                               random_slope_Z2 = time)
  
  dati_random = left_join(dati_random, random2, by = "id")
  
  
  
  random3 = random3 %>%
    tibble::rownames_to_column(var = "id")
  random3$id = as.numeric(random3$id)
  
  random3 = random3 %>% rename(random_intercept_Z3 = '(Intercept)',
                               random_slope_Z3 = time)
  
  dati_random = left_join(dati_random, random3, by = "id")
  
  
  
  random4 = random4 %>%
    tibble::rownames_to_column(var = "id")
  random4$id = as.numeric(random4$id)
  
  random4 = random4 %>% rename(random_intercept_Z4 = '(Intercept)',
                               random_slope_Z4 = time)
  
  dati_random = left_join(dati_random, random4, by = "id")
  
  
  
  
  ## RANDOM EFFECT - TEST SET 
  
  dati_test = dati_test %>%
    group_by(id) %>%
    mutate(meantime = mean(time, na.rm = TRUE))
  
  #summarizing longitudinal covariates at the landmark time
  random1_test = predict_random_effects_manual(lmm1, dati_test)
  random2_test = predict_random_effects_manual(lmm2, dati_test)
  random3_test = predict_random_effects_manual(lmm3, dati_test)
  random4_test = predict_random_effects_manual(lmm4, dati_test)
  
  random1_test$id = as.numeric(random1_test$id)
  
  random1_test = random1_test %>% rename(random_intercept_Z1 = '(Intercept)',
                                         random_slope_Z1 = time)
  
  dati_random_test = left_join(df_test, random1_test, by = "id")
  
  
  random2_test$id = as.numeric(random2_test$id)
  
  random2_test = random2_test %>% rename(random_intercept_Z2 = '(Intercept)',
                                         random_slope_Z2 = time)
  
  dati_random_test = left_join(dati_random_test, random2_test, by = "id")
  
  
  
  random3_test$id = as.numeric(random3_test$id)
  
  random3_test = random3_test %>% rename(random_intercept_Z3 = '(Intercept)',
                                         random_slope_Z3 = time)
  
  dati_random_test = left_join(dati_random_test, random3_test, by = "id")
  
  
  
  random4_test$id = as.numeric(random4_test$id)
  
  random4_test = random4_test %>% rename(random_intercept_Z4 = '(Intercept)',
                                         random_slope_Z4 = time)
  
  dati_random_test = left_join(dati_random_test, random4_test, by = "id")
  
  
  
  
  
  ##################
  ##### STEP 3 #####
  ##################
  
  #post-landmark prediction with the cure model
  
  #MODEL 1: landmarking
  model1 = cureph(Surv.cure(time = obs_time, event = status) ~ 
                    X1 + X2 + X3 + X4,
                  formula2 = ~ random_intercept_Z1 + random_slope_Z1
                  + random_intercept_Z2 + random_slope_Z2
                  + random_intercept_Z3 + random_slope_Z3
                  + random_intercept_Z4 + random_slope_Z4, 
                  data = dati_random)
  summary(model1)
  
  
  
  #MODEL 2: LOCF
  model2 = cureph(Surv.cure(time = obs_time, event = status) ~ 
                    X1 + X2 + X3 + X4,
                  formula2 = ~ Z1 + Z2 + Z3 + Z4, 
                  data = dati_random)
  summary(model2)
  
  
  
  
  
  
  ##################
  ### PREDICTION ###
  ##################
  
  mysurv1 = survpred.cureph(model1, newdata = dati_random_test)
  
  mysurv2 = survpred.cureph(model2, newdata = dati_random_test)
  
  
  
  #### Predictive performance evaluation
  
  
  #INCIDENCE
  
  #linear predictor of the incidence
  O_hat1 = mysurv1[["logistic.linear.predict"]]
  O_hat2 = mysurv2[["logistic.linear.predict"]]
  

  cutoffs1 = quantile(O_hat1, probs = seq(0.01, 0.99, by = 0.01))
  cutoffs2 = quantile(O_hat2, probs = seq(0.01, 0.99, by = 0.01))
  
  roc_points1 = t(sapply(cutoffs1, compute_sen_spe, 
                         O_hat = O_hat1, q_hat = dati_random_test$cured))
  roc_points2 = t(sapply(cutoffs2, compute_sen_spe, 
                         O_hat = O_hat2, q_hat = dati_random_test$cured))
  
  roc_sensitivity1 = roc_points1[, 1]
  roc_sensitivity2 = roc_points2[, 1]
  
  roc_specificity1 = roc_points1[, 2]
  roc_specificity2 = roc_points2[, 2]
  
  roc_fpr1 = 1 - roc_specificity1  # FPR = 1 - Specificity
  roc_fpr2 = 1 - roc_specificity2  # FPR = 1 - Specificity
  
  auc_estimated1 = sum(diff(roc_specificity1) * (head(roc_sensitivity1, -1) + tail(roc_sensitivity1, -1)) / 2)
  auc_estimated2 = sum(diff(roc_specificity2) * (head(roc_sensitivity2, -1) + tail(roc_sensitivity2, -1)) / 2)
  
  
  
  
  #LATENCY
  dati_random_test$cox_linear_pred1 = mysurv1[["cox.linear.predict"]]
  dati_random_test$cox_linear_pred2 = mysurv2[["cox.linear.predict"]]
  
  roc_surv1 = list()  
  roc_surv2 = list()
  time_points = c(seq(min(dati_test$obs_time)+1,max(dati_test$obs_time), by = 1))
  
  for(i in 1:length(time_points)) {
    roc_surv1[[i]] = survivalROC(
      Stime = dati_random_test$obs_time,
      status = dati_random_test$status,
      marker = dati_random_test$cox_linear_pred1,
      predict.time = time_points[i],
      method = "KM"
    )
  }
  
  for(i in 1:length(time_points)) {
    roc_surv2[[i]] = survivalROC(
      Stime = dati_random_test$obs_time,
      status = dati_random_test$status,
      marker = dati_random_test$cox_linear_pred2,
      predict.time = time_points[i],
      method = "KM"
    )
  }
  
  names(roc_surv1) = as.character(time_points)
  names(roc_surv2) = as.character(time_points)
  
  auc_surv1 = rep(0, length(time_points))
  auc_surv2 = rep(0, length(time_points))
  
  for(i in 1:length(time_points)){
    auc_surv1[i] = roc_surv1[[i]][["AUC"]]
  }
  
  for(i in 1:length(time_points)){
    auc_surv2[i] = roc_surv2[[i]][["AUC"]]
  }
  
  
  
  
  ##########################
  ### BRIER SCORE - INCIDENCE
  ##########################
  
  p_hat1_test = plogis(mysurv1$logistic.linear.predict)
  p_hat2_test = plogis(mysurv2$logistic.linear.predict)
  
  brier_incidence1 = mean((p_hat1_test - dati_random_test$cured)^2)
  brier_incidence2 = mean((p_hat2_test - dati_random_test$cured)^2)
  
  
  ##########################
  ### BRIER SCORE - LATENCY
  ##########################
  
  cumhaz_baseline1 = data.frame(
    time = mysurv1$surv.cureph[,1],
    haz = mysurv1$cox.cumhaz 
  )
  eta1 = mysurv1$cox.linear.predict
  
  
  cumhaz_baseline2 = data.frame(
    time = mysurv2$surv.cureph[,1],
    haz = mysurv2$cox.cumhaz 
  )
  eta2 = mysurv2$cox.linear.predict
  
  
  
  S_hat_latency1 = matrix(NA, nrow = length(eta1), ncol = length(time_points))
  
  for (j in seq_along(time_points)) {
    t = time_points[j]
    
    H0_t = with(cumhaz_baseline1, approx(time, haz, xout = t, rule = 2)$y)
    
    S_hat_latency1[, j] = exp(-H0_t * exp(eta1))
  }
  
  
  S_hat_latency2 = matrix(NA, nrow = length(eta2), ncol = length(time_points))
  
  for (j in seq_along(time_points)) {
    t = time_points[j]
    
    H0_t = with(cumhaz_baseline2, approx(time, haz, xout = t, rule = 2)$y)
    
    S_hat_latency2[, j] = exp(-H0_t * exp(eta2))
  }
  
  
  I_event = sapply(time_points, function(t) {
    as.numeric(dati_random_test$obs_time <= t & dati_random_test$status == 1)
  })
  
  
  
  brier_latency1 = numeric(length(time_points))
  
  for (j in seq_along(time_points)) {
    brier_latency1[j] = mean((S_hat_latency1[, j] - (1 - I_event[, j]))^2, 
                             na.rm = TRUE)
  }
  
  
  
  brier_latency2 = numeric(length(time_points))
  
  for (j in seq_along(time_points)) {
    brier_latency2[j] = mean((S_hat_latency2[, j] - (1 - I_event[, j]))^2, 
                             na.rm = TRUE)
  }
  
  
  
  
  
  
  
  
  
  ###########
  # results #
  ###########
  
  #### incidence
  
  res_incidence = data.frame(
    AUC = c(auc_estimated1, auc_estimated2),
    Brier = c(brier_incidence1, brier_incidence2),
    Model = c("Model-based", "LOCF")
  )
  
  
  
  ### latency
  res_latency = data.frame(
    Time = rep(time_points, times = 2),
    AUC = c(auc_surv1, auc_surv2),
    Brier = c(brier_latency1, brier_latency2),
    Model = rep(c("Model-based", "LOCF"), each = length(time_points))
  )
  
  
  
  
  return(list(
    incidence = res_incidence,
    latency = res_latency
  ))
  
}


