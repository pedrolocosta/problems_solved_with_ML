
# Business problem definition
#-------------------------------------------------------------------------------
# Predicting medical expenses from patients in a hospital


# Data acquisition
#-------------------------------------------------------------------------------
# CSV file sent through the hospital and separated for training and testing data
df_train <- read.csv("df_train.csv")


# Knowing the data
#-------------------------------------------------------------------------------
# general analysis -------------------------------------------------------------
# initial data visualization
head(df_train)
# checking the types of variables
str(df_train)
# checking form missing values
any(is.na(df_train))
# viewing statistical summary
summary(df_train)

# correlation analysis ---------------------------------------------------------
# filtering variables and search for correlation
numerical_columns <- sapply(df_train, is.numeric)
data_cor <- cor(df_train[,numerical_columns])
data_cor
# creating a heatmap
#install.packages('corrplot')
library(corrplot)
corrplot(data_cor, method='color')
# creating a heatmap associated with the pie graphs
#install.packages('corrgram')
library(corrgram)
corrgram(df_train, order=TRUE, lower.panel=panel.shade, upper.panel=panel.pie,
         text.panel=panel.txt)
  # realiza that there is no clear relationship between the variables
# viewing the relationship between variables
pairs(df_train[c('age', 'bmi', 'children', 'expenses')])
  # None of the correlations in the matrix are considered strong, but there are 
  # some interesting associations. For example, age and BMI (BMI) appear to have 
  # a weak positive correlation, which means that with increasing age, body mass 
  # tends to increase. There is also a positive correlation moderate between age 
  # and spending, in addition to the number of children and spending. These 
  # associations imply that as age, body mass and number of children increase, 
  # the expected cost of health insurance goes up.

# analyzing the distribuition of the target variable ---------------------------
# statistical summary
summary(df_train$expenses)
# variance 
var(df_train$expenses)
# standard deviation 
sd(df_train$expenses)

# Boxplot 
boxplot(df_train$expenses, main = "Boxplot for target variable",
        xlab = "Value", horizontal=TRUE)

# Histogram 
#install.packages('ggplot2)
library(ggplot2)
ggplot(df_train, aes(x=expenses))+
  geom_histogram(bins=20, alpha=0.5, fill='blue')+
  theme_minimal()


# Creating and training a model
#-------------------------------------------------------------------------------
# searching for the more important variables for the model
library(caret)
model_ex <- train(expenses ~ .,
                  data = df_train,
                  method = "lm")

varImp(model_ex)

# creating other models using the more important variables
model_v1 <- lm(expenses ~ .,df_train)
model_v2 <- lm(expenses ~ age+bmi,df_train)


# Evaluating the model
#-------------------------------------------------------------------------------
# model visualization
model_v1
model_v2

# model statistical summary
summary(model_v1) # 0.7509
summary(model_v2) # 0.1173
  # discarding model_v2 due to poor R-squared performance

# plot the residuals graphics
plot(model_v1)


# Evaluating the residuals -----------------------------------------------------
# getting the residuals
res <- as.data.frame(residuals(model_v1))
head(res)

# residuals hitogram
hist(res)
ggplot(res, aes(residuals(model_v1)))+
 geom_histogram(fill='blue', alpha=0.5, binwidth=1)


# Testing the  model
#-------------------------------------------------------------------------------
df_test <- read.csv("df_test.csv")
model_v1_test <- predict(model_v1, df_test)
head(df_test)


# Model optimization
#-------------------------------------------------------------------------------
# Data Munging -----------------------------------------------------------------
# Adding a variable with twice the age value
df_train$age2 <- df_train$age^2
df_test$age2 <- df_test$age^2

# Adding an indicator for BMI >= 30
df_train$bmi30 <- ifelse(df_train$bmi >= 30, 1, 0)
df_test$bmi30 <- ifelse(df_test$bmi >= 30, 1, 0)

# creating the final model -----------------------------------------------------
model_v3 <- lm(expenses ~ age + age2 + children + bmi + gender +
                   bmi30 * smoker + region, data = df_train)

# evaluating the final model ---------------------------------------------------
summary(model_v3) # 0.8664 -> better R-squared

# testing the final model ------------------------------------------------------
model_v3_test <- predict(model_v3, df_test)

