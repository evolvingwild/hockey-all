#### --------------------------------------------------- ####
## -       XGBoost Modelling       |        06.02.18     - ##
#### --------------------------------------------------- ####


# Required packages
library(DiagrammeR); library(rsvg)
library(xgboost); require(glmnet)
require(caret); require(dplyr)

options(scipen = 999)
set.seed(250)


### Objects ###
c("SHOT",  "GOAL") -> st.shot_events
c("SHOT", "GOAL", "MISS") -> st.fenwick_events
c("SHOT", "GOAL", "MISS", "BLOCK" ) -> st.corsi_events
c("3v3", "5v5", "4v4", "5v4", "4v5", 
  "5v3", "3v5", "4v3", "3v4", "5vE", 
  "Ev5", "4vE", "Ev4", "3vE", "Ev3") %>% as.factor() -> st.strength_states
c("5v5", "4v4", "3v3") %>% as.factor() -> st.even_strength
c("5v4", "4v5", "5v3", "3v5", "4v3", "3v4", 
  "5vE", "Ev5", "4vE", "Ev4", "3vE", "Ev3") %>% as.factor() -> st.uneven_strength
c("5v4", "4v5", "5v3", "3v5", "4v3", "3v4") %>% as.factor() -> st.pp_strength
c("5vE", "Ev5", "4vE", "Ev4", "3vE", "Ev3") %>% as.factor() -> st.empty_net



## ----------------------------- ##
##  Current model prep process   ##
## ----------------------------- ##

### Please run the functions found the xG_preparation script prior to running of the following code.

# Initla pbp prep - for both strength states
pbp_part <- fun.pbp_expand(pbp_part)
pbp_part <- fun.pbp_index(pbp_part)

# Create model data frames - EV
pbp_prep_EV <- fun.pbp_prep(pbp_part, "EV")
model_prep_EV <- fun.model_prep(pbp_prep_EV, "EV") 


# For the purpose of this script, the other strength states (PP, SH, EN) will not be shown. If one 
# wishes to run/create a model for a different strength state, simply change "EV" to the preferred state. 
# All of the following code will remain the same unless one wishes to change the features for each model. 


## ------------------------- ##
##     XGBoost - training    ##
## ------------------------- ##
##########################################################

# Make sparse
model_data_sparse <- Matrix(model_prep_EV, sparse = TRUE)

# Separate into target / predictors
is_goal_vect <- model_data_sparse[, 1]
predictors_matrix <- model_data_sparse[, 2:ncol(model_data_sparse)]

# Full XGB data model 
full_xgb <- xgb.DMatrix(data = predictors_matrix, label = is_goal_vect)


## -- XGB cv loop -- ##
run_num <- 5
#run_num <- 200

best_df <- data.frame(matrix(nrow = run_num, ncol = 10))
best_ll <- data.frame(matrix(nrow = run_num, ncol = 5))


# Loop for paramter tuning
for (i in 1:run_num) {
  
  print(paste("### LOOP:", i, "###"))
  
  param <- list(objective = "binary:logistic", 
                eval_metric = "logloss", 
                eval_metric = "auc",
                
                #max_depth = sample(6:10, 1), # run 1
                #max_depth = sample(6:7, 1),  # run 2
                max_depth = 6,                # run 3
                
                #eta = runif(1, .01, .3),  # run 1
                #eta = runif(1, .05, .15), # run 2
                eta = runif(1, .06, .11),  # run 3
                #eta = .1,                 # misc
                
                #gamma = runif(1, 0.05, 0.15),  # run 1
                gamma = runif(1, 0.06, 0.12),   # run 2 / 3
                #gamma = 0,                     # misc
                #gamma = runif(1, 0.0, 0.2),    # misc
                
                
                #subsample = runif(1, .6, .9),   # run 1
                #subsample = runif(1, .75, .85), # run 2
                subsample = runif(1, .76, .84),  # run 3
                #subsample = .7,                 # misc
                
                #colsample_bytree = runif(1, .5, .8),  # run 1
                #colsample_bytree = runif(1, .75, .8), # run 2
                colsample_bytree = runif(1, .76, .8),  # run 3
                
                #min_child_weight = sample(1:40, 1), # run 1
                #min_child_weight = sample(5:35, 1), # run 2
                min_child_weight = sample(5:22, 1),  # run 3
                
                #max_delta_step = sample(1:10, 1)    # run 1
                max_delta_step = sample(4:8, 1)      # run 2 / run 3
                )
  
  # CV
  rm(.Random.seed, envir = globalenv())
  x <- round(runif(1) * 10000, 0)
  set.seed(x)
  
  cv_param <- xgb.cv(data = full_xgb, 
                     params = param, 
                     nthread = 4, 
                     nfold = 5, 
                     nrounds = 1000,
                     verbose = T, 
                     early_stopping_rounds = 25
                     )
  
  # Record results
  best_df[i, ] <-  unlist(param)
  best_ll[i, 1] <- min(cv_param$evaluation_log$test_logloss_mean)
  best_ll[i, 2] <- which.min(cv_param$evaluation_log$test_logloss_mean)
  best_ll[i, 3] <- max(cv_param$evaluation_log$test_auc_mean)
  best_ll[i, 4] <- which.max(cv_param$evaluation_log$test_auc_mean)
  best_ll[i, 5] <- x
}

gc()


## -- Clean up and find best paramter set -- ##

# Clean up and bind
colnames(best_df) <- names(param)
colnames(best_df)[2] <- c("eval_metric_1")
colnames(best_df)[3] <- c("eval_metric_2")
colnames(best_ll) <- c("ll", "ll_rounds", "auc", "auc_rounds", "seed")
best_all <- cbind(best_df, best_ll)
best_all <- na.omit(best_all)

# Arrange to get best run by AUC
best_all <- best_all %>% 
  mutate_at(vars(max_depth:colsample_bytree), funs(as.numeric(.))) %>% 
  mutate_if(is.numeric, funs(round(., 7))) %>% 
  arrange(desc(auc))



## -- Final parameters -- ##
# Parameters should be updated based on what the xgb.cv() results returned (in best_all above)

param_7_EV <- list(objective = "binary:logistic", 
                   eval_metric = "logloss", 
                   eval_metric = "auc",    
                   eta = .068, 
                   gamma = .12,
                   subsample = .78, 
                   max.depth = 6,           
                   colsample_bytree = .76,  
                   min_child_weight = 5,   
                   max_delta_step = 5,      
                   nthread = 4)

## -- CV rounds Loop -- ##
# Run to determine number of rounds and seed with the paramters held constant

run_num <- 5 # Update for preferred number of runs
cv_test <- data.frame(matrix(nrow = run_num, ncol = 5))

# Loop
for(i in c(1:run_num)) { 
  
  print(paste("### LOOP:", i, "###"))
  
  rm(.Random.seed, envir = globalenv())
  x <- round(runif(1) * 10000, 0)
  set.seed(x)
  
  cv_rounds <- xgb.cv(param = param_7_EV, 
                      data = full_xgb, 
                      nfold = 5, 
                      nrounds = 1000, 
                      verbose = 2, 
                      early_stopping_rounds = 25,
                      prediction = T) 
  
  # Record results
  cv_test[i, 1] <- which.max(cv_rounds$evaluation_log$test_auc_mean)
  cv_test[i, 2] <- max(cv_rounds$evaluation_log$test_auc_mean)
  cv_test[i, 3] <- which.min(cv_rounds$evaluation_log$test_logloss_mean)
  cv_test[i, 4] <- min(cv_rounds$evaluation_log$test_logloss_mean)
  cv_test[i, 5] <- x
  }

# Clean results and sort to find the number of rounds and seed to use
names(cv_test) <- c("AUC_rounds", "AUC", "LL_rounds", "LL", "seed")
cv_final <- cv_test %>% 
  arrange(desc(AUC)) %>% 
  add_row(AUC_rounds = mean(cv_test$AUC_rounds),
          AUC = mean(cv_test$AUC), 
          LL_rounds = mean(cv_test$LL_rounds), 
          LL = mean(cv_test$LL), 
          seed = mean(cv_test$seed))


# Train the final model
set.seed(556) # Change this based on the cv_final results

xg_train_xgb <- xgb.train(data = full_xgb,
                          params = param_7_EV, # Determined and set above
                          nround = 189,        # Change this based on the cv_final results
                          verbose = 2)


# Save model
#saveRDS(xg_train_xgb, "xG_model_XGB_7yr_EV_final_4.rds")


##########################################################



# The following parameter sets were used for our current models: 


## ----- EV ----- ##

# Seed: 556
# nrounds: 189

param_7_EV <- list(objective = "binary:logistic", 
                   eval_metric = "logloss", 
                   eval_metric = "auc",    
                   eta = .068, 
                   gamma = .12,
                   subsample = .78, 
                   max.depth = 6,           
                   colsample_bytree = .76,  
                   min_child_weight = 5,   
                   max_delta_step = 5,      
                   nthread = 4)


## ----- UE ----- ##

# Seed: 979
# nrounds: 167

param_UE_7 <- list(objective = "binary:logistic", 
                   eval_metric = "logloss", 
                   eval_metric = "auc",
                   eta = .049, 
                   gamma = .0002, 
                   subsample = .8,
                   max.depth = 6,
                   colsample_bytree = .76, 
                   min_child_weight = 10, 
                   max_delta_step = 7,
                   nthread = 5)



## ----- SH ----- ##

# Seed: 347
# nrounds: 139

param_SH_10 <- list(objective = "binary:logistic", 
                    eval_metric = "logloss", 
                    eval_metric = "auc",
                    eta = .0395, 
                    gamma = .0075, 
                    subsample = .886,
                    max.depth = 6,
                    colsample_bytree = .685, 
                    min_child_weight = 25, 
                    max_delta_step = 6,
                    nthread = 5)


## ----- EN ----- ##

# Seed: 304
# nrounds: 275
# This isn't being used in anything other than the game charts (which probably should just be removed)
param_EN_10 <- list(objective = "binary:logistic", 
                    eval_metric = "logloss", 
                    eval_metric = "auc",
                    eta = .0191, 
                    gamma = .195, 
                    subsample = .683,
                    max.depth = 6,
                    colsample_bytree = .728, 
                    min_child_weight = 4, 
                    max_delta_step = 10,
                    nthread = 5)





##### ----------------------                END                ---------------------- ##### 
