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


# Creating and training a model with otimization
#-------------------------------------------------------------------------------
library(caret) 
formula <- as.formula("credit.rating ~ .")                                      # Feature selection
control <- trainControl(method = "repeatedcv", number = 10, repeats = 2)

model_v2_fs <- train(formula, data = df_train, method = "glm", 
                     trControl = control)                                       # a new model for feature selection

importance <- varImp(model_v2_fs, scale = FALSE)                                # listing the most important variable

plot(importance)                                                                # plotting the most important variable


# Construindo o modelo com as variáveis selecionadas
                                                                                # Building a model with the features selected
formula_v2 <- as.formula("credit.rating ~ account.balance + 
                          previous.credit.payment.status + current.assets +
                          credit.purpose + saving")
model_v2 <- glm(formula = formula_v2, data = df_train, family = "binomial")     

summary(model_v2)                                                               # viewing the model


# Testing the  model
#-------------------------------------------------------------------------------
model_v2_test <- round(predict(model_v2, df_test, type="raw"))                  # testing the model_v2 using test data


# Evaluating the model performance
#-------------------------------------------------------------------------------
confusionMatrix(table(data = model_v2_test, reference = test_class_var),        # creating a confusion matrix
                positive = '1')

# Accuracy : 0.76           
# 95% CI : (0.7151, 0.801)
# No Information Rate : 0.6925         
# P-Value [Acc > NIR] : 0.001697  

model_v2_pred <- predict(model_v2, test_feature_vars, type = "raw")             # creating ROC curve
predictions <- prediction(model_v2_pred, test_class_var)
par(mfrow = c(1,2))

png('03.analysis/model_v2-ROC_curve.png', width=500, height=500, res=72)
plot.roc.curve(predictions, title.text = "ROC curve")                           # plotting ROC curve
dev.off()
png('03.analysis/model_v2-precision-recall_curve.png', width=500, height=500, 
    res=72)
plot.pr.curve(predictions, title.text = "Precision/Recall curve")               # plotting Precision/Recall curve
dev.off()

if(Azure) {
  maml.mapOutputPort('df')
}
install.packages('tinytex')
