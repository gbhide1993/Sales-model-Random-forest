## Using Random Foresting 
setwd("F:/Data Science Course Data/Defour Project")
sales_df <- read.csv("Train_Sales.csv")


##Treating NAs
names(sales_df)
library(mice)

#Loading the following package for looking at the missing values
install.packages("VIM")
library(VIM)
library(lattice)

str(sales_df)

set.seed(12345)
sales_df[sales_df == 0] <- NA ## replacing 0 values to NA's
sales_df <- sales_df
tempData <- mice(sales_df,m=3, maxit = 2 ,seed=500, where = is.na(sales_df))
summary(tempData)

## Imputed dataset
imputed_data <- complete(tempData,1)
densityplot(tempData)


sales_df <- imputed_data
##Spliting dataset into test & train
library(caTools)
split_sales_df <- sample.split(sales_df,SplitRatio = 0.8)
train_split <- subset(sales_df,split_sales_df == TRUE)
test_split <- subset(sales_df,split_sales_df == FALSE)


## Creating model
library(randomForest)
#?randomForest.formula

#Parameter tuning trial
library(caret)
tuneGrid <- expand.grid(.mtry = c(1: 10))
trControl <- trainControl(method = "cv",
                          number = 5,
                          search = "grid")

 ## finding best mtry
rf_default <- train(Item_Outlet_Sales~ Item_Weight + Item_Fat_Content + 
                      Item_Visibility + Item_Type + Item_MRP + Outlet_Establishment_Year + 
                       Outlet_Location_Type,
                    data = train_split,
                    method = "rf",
                    tuneGrid = tuneGrid,
                    importance = TRUE,
                    nodesize = 10,
                    metric = "RSME",
                    
                    trControl = trControl)

## Print the results
print(rf_default)

## Building model on train_split datasets
train_split_model <- randomForest(Item_Outlet_Sales~ Item_Weight + Item_Fat_Content + 
                                    Item_Visibility + Item_Type + Item_MRP + Outlet_Establishment_Year + 
                                     Outlet_Location_Type,data = train_split,mtry= 7, ntree=80 )
summary(train_split_model)

## Running & validating model on test_split
test_split_model <- randomForest(Item_Outlet_Sales~ Item_Weight + Item_Fat_Content + 
                                    Item_Visibility + Item_Type + Item_MRP + Outlet_Establishment_Year + 
                                     Outlet_Location_Type,data = test_split,mtry= 7, ntree=80 )
test_split_model$predicted

## Predicting on Test data set
predict_test_split <- data.frame(predict(train_split_model, test_split))

## trial calculating error in predicted 
trial_col <- data.frame(pred = test_split_model$predicted, actual = test_split$Item_Outlet_Sales)
error <- (trial_col$pred - trial_col$actual)
error_df <- data.frame(error)
error_mean <- mean(error)
diff <- (((trial_col$pred - trial_col$actual) / trial_col$actual) * 100 )
diff_data_frame <- data.frame(diff)
diff_mean <- mean(diff)

## Calculating RMSE of actual vs. predicted
RMSE(test_split$Item_Outlet_Sales,test_split_model$predicted)
RMSE(train_split$Item_Outlet_Sales,train_split_model$predicted)

R2 <- 1 - (sum((trial_col$actual-trial_col$pred)^2)/sum((trial_col$actual-mean(trial_col$actual))^2))



## Running on test data set
setwd("F:/Data Science Course Data/Defour Project")
test_data <- read.csv("Test_Sales.csv", header = TRUE)

## Imputing NA's from Test_sales

set.seed(12345)
test_data[test_data == 0] <- NA ## replacing 0 values to NA's
test_data <- test_data

tempData_Test <- mice(test_data,m=3, maxit = 2 ,seed=500, where = is.na(test_data))
imputed_data_test <- complete(tempData_Test,1)
test_data <- imputed_data_test



## Predicting values on Test_sales from previous train model.
predict_test <- data.frame(predict(train_split_model, test_data)) 
summary(predict_test)
predict_table <-  data.frame(table(predict_test)) 

sink("output_after_NA2.csv")
#getOption("max.print") 
options(max.print = 9999)
#?options
print(predict_test)
sink()
