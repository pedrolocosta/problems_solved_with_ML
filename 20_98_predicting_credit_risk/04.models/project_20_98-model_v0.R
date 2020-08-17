# Business problem definition
#-------------------------------------------------------------------------------
# assess the risk of granting credit to customers of financial institutions


# Data acquisition
#-------------------------------------------------------------------------------
Azure <- FALSE

if(Azure){
  source("src/plot_utils.R")
  source("src/ClassTools.R")
  df <- maml.mapInputPort(1)
}else{
  source("02.src/plot_utils.R")
  source("02.src/ClassTools.R")
  df <- read.csv("01.dataset/credit.csv", sep = ",", header = T)
}


# Exploratory data analysis
#-------------------------------------------------------------------------------
# general analysis -------------------------------------------------------------
head(df)                                                                        # initial data visualization
str(df)                                                                         # checking the types of variables
summary(df)                                                                     # viewing statistical summary
any(is.na(df))                                                                  # checking form missing values
View(df)

library(ggplot2)                                                                
lapply(colnames(df), function(x){                                               # overview of the variables
  if(is.integer(df[,x])) {
    ggplot(df, aes_string(x)) +
      geom_bar() +
      facet_grid(. ~ credit.rating) +
      ggtitle(paste("Taxa de crédito por",x))}})


# correlation analysis ---------------------------------------------------------
methods <- c("pearson", "spearman")                                             # listing the methods

cors <- lapply(methods, function(method)                                        # correlating the method with the variables
  (cor(df[, colnames(df)], method = method)))

require(lattice)
plot_cors <- function(x, labs){                                                 # associating the correlation to a plot
  diag(x) <- 0.0
  plot( levelplot(x,
                  main = paste("Correlation analysis using method", labs),
                  scales = list(x = list(rot = 90), cex = 1.0)) )
}
Map(plot_cors, cors, methods)                                                   # plotting the correlation maps


# Data Munging + Feature Engineering
#-------------------------------------------------------------------------------
# normalizing the numeric variables --------------------------------------------
scale_features <- function(df, variables){                                      # function to normalize the variables
  for (variable in variables){
    df[[variable]] <- scale(df[[variable]], center=T, scale=T)
  }
  return(df)
}

numeric_vars <- c("credit.duration.months", "age", "credit.amount")             # listing the numeric variable
df <- scale_features(df, numeric_vars)                                          # applying the normalization

str(df)                                                                         # checking the data frame

# converting variables to factor type (categorical) ---------------------------- 
to_factors <- function(df, variables){                                          # function to transform numeric variable in factor                            
  for (variable in variables){                                                  
    df[[variable]] <- as.factor(df[[variable]])
  }
  return(df)
}

categorical_vars <- c('credit.rating', 'account.balance',                       # listing the variables to be transformed
                      'previous.credit.payment.status', 'credit.purpose', 
                      'savings', 'employment.duration', 'installment.rate',
                      'marital.status', 'guarantor', 'residence.duration', 
                      'current.assets', 'other.credits', 'apartment.type', 
                      'bank.credits', 'occupation', 'dependents', 'telephone', 
                      'foreign.worker')
df <- to_factors(df = df, variables = categorical_vars)                         # applying the transformation
str(df)                                                                         # checking the data frame


# Create a training and test data partition
#-------------------------------------------------------------------------------
indexes <- sample(1:nrow(df), size = 0.6 * nrow(df))
df_train <- df[indexes,]
df_test <- df[-indexes,]


# Creating and training a model
#-------------------------------------------------------------------------------
library(caret) 
library(ROCR) 
source("02.src/plot_utils.R")                                                   # utility library for building graphs

test_feature_vars <- df_test[,-1]                                               # separate feature and class variables
test_class_var <- df_test[,1]

formula_init <- as.formula("credit.rating ~ .")                                 # Building a logistic regression model
model_v0 <- glm(formula = formula_init, data = df_train, family = "binomial")

summary(model_v0)                                                               # viewing the model


# Testing the  model
#-------------------------------------------------------------------------------
model_v0_test <- round(predict(model_v0, df_test, type="response"))             # testing the model_fs using test data


# Evaluating the model performance
#-------------------------------------------------------------------------------
confusionMatrix(table(data = model_v0_test, reference = test_class_var),        # creating confusiono matrix
                positive = '1')

# Accuracy : 0.765           
# 95% CI : (0.7203, 0.8057)
# No Information Rate : 0.68            
# P-Value [Acc > NIR] : 0.0001153 


model_v0_test <- predict(model_v0, test_feature_vars, type = "response")        # creating ROC curve
predictions <- prediction(model_v0_test, test_class_var)
par(mfrow = c(1,2))

png('03.analysis/model_v0-ROC_curve.png', width=500, height=500, res=72)
plot.roc.curve(predictions, title.text = "ROC curve")                           # plotting ROC curve
dev.off()
png('03.analysis/model_v0-precision-recall_curve.png', width=500, height=500, 
    res=72)
plot.pr.curve(predictions, title.text = "Precision/Recall curve")               # plotting Precision/Recall curve
dev.off()

if(Azure) {
  maml.mapOutputPort('df')
} else {
  write.csv(df, '01.dataset/df_01.csv')
}
