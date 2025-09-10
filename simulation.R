###############
## Simulation 


args = commandArgs(trailingOnly = TRUE)


library(dplyr)
library(MASS)
library(nlme)
library(curephEM)
library(survivalROC)
library(doParallel)


####################
# initial settings #
####################
B = as.numeric(args[2]) #number of iterations 


cl = makeCluster(100)
registerDoParallel(cl)

result_mat = foreach(i = 1:B, .export = c(".GlobalEnv"), 
                     .inorder = FALSE,
                     .packages = c("dplyr", "MASS", "nlme", "curephEM", "survivalROC")) %dopar% {

                       # select scenario
                       scenario = args[3]
                       
                       #import parameters
                       source('parameters.R')
                       pars = make_par(scenario)
                       alpha = pars$alpha
                       beta = pars$beta
                       lambdaC = pars$lambdaC
                       M_fixed = pars$M_fixed
  
                       # import functions
                       source('functions.R')

                       # number of observations
                       n = as.numeric(args[1]) 

                       # data-generating process
                       type = args[4] 


                       seed_i = sample.int(1e6, 1)
                       print(paste("Iterazione:", i, "- Seed:", seed_i))
                       flush.console()

                       tryCatch(
                         {
                           result = cure_pred(n, seed = seed_i,
                                              scenario,
                                              type = type,
                                              M_fixed,
                                              alpha, beta,
                                              lambdaC,
                                              gamma = 1.2,
                                              landmark_time = 3,
                                              max_time = 10,
                                              mean_CURE = rep(0, length(alpha) - 1L),
                                              sd_CURE = rep(1, length(alpha) - 1L),
                                              cor_CURE = diag(length(alpha) - 1L))
                           return(c(result, seed = seed_i))
                         },
                         error = function(e) {
                           msg = paste("Errore all'iterazione", i, "- seed:", seed_i, "-", conditionMessage(e))
                           message(msg)
                           return(list(incidence = NA, latency = NA, seed = seed_i))
                         }
                       )
                     }



stopCluster(cl)


save.image(file = paste0("simulation_", args[3], "_", args[1], "_", args[4], ".RData"))
