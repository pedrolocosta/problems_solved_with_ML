# Business problem definition
#-------------------------------------------------------------------------------
# assess the risk of granting credit to customers of financial institutions


# Data acquisition
#-------------------------------------------------------------------------------
Azure <- FALSE

if(Azure){
  source("src/plot_utils.R")
  df <- maml.mapInputPort(1)
}else{
  source("02.src/plot_utils.R")
  df <- read.csv("01.dataset/df_01.csv", sep = ",", header = T)
  df <- df[,-1]
}


# Create a training and test data partition
#-------------------------------------------------------------------------------
indexes <- sample(1:nrow(df), size = 0.6 * nrow(df))                            # separate samples in training and test 
df_train <- df[indexes,]
df_test <- df[-indexes,]

test_feature_vars <- df_test[,-1]                                               # separate feature and class variables
test_class_var <- df_test[,1]


# Preparation of the model using Feature Selection
#-------------------------------------------------------------------------------
library(caret) 
library(randomForest) 

run_feature_selection <- function(num_iters=20, feature_vars, class_var){       # function to list the most important variables
  variable_sizes <- 1:10
  control <- rfeControl(functions = rfFuncs, method = "cv", 
                        verbose = FALSE, returnResamp = "all", 
                        number = num_iters)
  results_rfe <- rfe(x = feature_vars, y = class_var, 
                     sizes = variable_sizes, 
                     rfeControl = control)
  return(results_rfe)
}

rfe_results <- run_feature_selection(feature_vars = df_train[,-1],              # applying the function
                                     class_var = df_train[,1])

rfe_results                                                                     # viewing the results
varImp((rfe_results))

# selected variable
# account.balance                23.8423112
# credit.duration.months         16.8357379
# credit.amount                  14.6009608
# previous.credit.payment.status 14.0563528
# marital.status                  8.1908121

# Creating and training a model
#-------------------------------------------------------------------------------
library(caret) 
                                                                                # Building a logistic regression model
formula_init <- as.formula("credit.rating ~ account.balance +                                 
                           credit.duration.months + credit.amount +
                           previous.credit.payment.status + marital.status")
model_v1 <- glm(formula = formula_init, data = df_train, family = "binomial")

summary(model_v1)                                                               # viewing the model


# Testing the  model
#-------------------------------------------------------------------------------
model_v1_test <- round(predict(model_v1, df_test, type="response"))             # testing the model_fs using test data


# Evaluating the model performance
#-------------------------------------------------------------------------------
library(ROCR) 
confusionMatrix(table(data = model_v1_test, reference = test_class_var),        # creating confusiono matrix
                positive = '1')

# Accuracy : 0.7275         
# 95% CI : (0.681, 0.7706)
# No Information Rate : 0.6925         
# P-Value [Acc > NIR] : 0.0705960 

# aborted model due to low performance

