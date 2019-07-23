## Using Random Foresting 
setwd("F:/Data Science Course Data/Defour Project")
sales_df <- read.csv("Train_Sales.csv")
str(sales_df)
set.seed(500)

new_data <- data.frame(cbind(Outlet_Type=c(sales_df$Outlet_Type), 
                             Item_Type = c(sales_df$Item_Type) , 
                             Outlet_Location_Type = c(sales_df$Outlet_Location_Type),
                             Item_Weight = c(sales_df$Item_Weight),
                             Item_Visibility = c(sales_df$Item_Visibility) , 
                             Item_MRP = c(sales_df$Item_MRP) , 
                             Outlet_Establishment_Year = c(sales_df$Outlet_Establishment_Year),
                             Item_Outlet_Sales = c(sales_df$Item_Outlet_Sales)))

summary(new_data)
names(new_data)

library(mice)
new_data[new_data == 0] <- NA ## replacing 0 values to NA's
tempData <- mice(new_data,m=5, maxit = 5 ,seed=500, where = is.na(new_data))
summary(tempData)
densityplot(tempData)
tempData$imp$sales_df.Item_Weight
tempData$imp$sales_df.Item_Visibility
imputed_data <- complete(tempData,3)
new_data <- imputed_data

##Spliting dataset into test & train
set.seed(500)
library(caTools)
split_sales_df <- sample.split(new_data,SplitRatio = 0.8)
train_split <- subset(new_data,split_sales_df == TRUE)
test_split <- subset(new_data,split_sales_df == FALSE)



library(randomForest)

#Parameter tuning trial
library(caret)
tuneGrid <- expand.grid(.mtry = c(1: 10))
trControl <- trainControl(method = "cv",
                          number = 5,
                          search = "grid")

## finding best mtry
rf_default <- train(Item_Outlet_Sales~ Item_Weight + 
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

set.seed(500)

## Building model on train_split datasets
train_split_model <- randomForest(Item_Outlet_Sales~ Item_Weight + 
                                    Item_Visibility + Item_Type + Item_MRP + Outlet_Establishment_Year + 
                                    Outlet_Location_Type,data = train_split,mtry= 2, ntree=80 )
summary(train_split_model)

## Predicting on Test data set
predict_test_split <- data.frame(predict(train_split_model, test_split))
summary(predict_test_split)
summary(test_split$Item_Outlet_Sales)

###############===== Running model on Test_sales dataset =====####################

## Running on test data set
setwd("F:/Data Science Course Data/Defour Project")
test_data <- read.csv("Test_Sales.csv", header = TRUE)
set.seed(500)


ab <- data.frame(cbind(Outlet_Type=c(test_data$Outlet_Type), 
                       Item_Type = c(test_data$Item_Type) , 
                       Outlet_Location_Type = c(test_data$Outlet_Location_Type),
                       Item_Weight = c(test_data$Item_Weight),
                       Item_Visibility = c(test_data$Item_Visibility) , 
                       Item_MRP = c(test_data$Item_MRP) , 
                       Outlet_Establishment_Year = c(test_data$Outlet_Establishment_Year)))
summary(ab)

## Imputing NA's
library(mice)
ab[ab == 0] <- NA ## replacing 0 values to NA's
temp <- mice(ab,m=5, maxit = 5 ,seed=500, where = is.na(ab))
summary(temp)
densityplot(temp)

imputed <- complete(temp,3)
ab <- imputed

## Predicting Outlet sales using model buit on train data.
predict_ab <- data.frame(predict(train_split_model, ab))
summary(predict_ab)


sink("xyz1.csv")
#getOption("max.print") 
options(max.print = 9999)
#?options
print(predict_ab)
sink()
