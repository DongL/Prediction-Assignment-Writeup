```{r}
###########################################
# Build a random forest model
###########################################

## Load up packages -------------------------------------------------------------------
library(caret)
library(ggplot2)
library(magrittr)
library(data.table)
library(dplyr)
library(doMC)


## Load up train and test datasets -------------------------------------------------------------------
train = read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv")
test = read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv")


## register multi-cores using DoMC package -------------------------------------------------------------------
registerDoMC(cores = detectCores() - 1)  



## Data preperation -------------------------------------------------------------------
# Find and remove the variable columes that contain more that 80% NAs. Those data may probably not useful for modeling due to the limited info provided. 
setDT(train)
setDT(test)
na_gt_80perc = train[, lapply(.SD, function(c) ((is.na(c) | c == "" ) %>% sum)/length(c)) > 0.8]

# Remove index variable - X, and outcome varaible(Y) - classe for both train and test dataset
tr = train[, c(!na_gt_80perc), with = F] %>% select(-classe, -X) %>% data.frame() 
te = test[, c(!na_gt_80perc), with = F] %>% select(-problem_id, -X) %>% data.frame()

## Train a random forest model using 5 fold cross validation ------------------------------------------------------------------- 
trCtl = trainControl(method = "cv", number = 5, allowParallel = T) 
x = tr
Y = train$classe 
mod2 = train(x, Y, method = 'rf', trControl = trCtl )

## Expected out of sample error -------------------------------------------------------------------
out_of_sample_error = mod2$results
confusionMatrix = confusionMatrix(mod2) 

## Prediction on 20 test cases -------------------------------------------------------------------
for(i in seq_along(x))
        levels(te[,i]) = levels(x[,i])
        
test.pred = predict(mod2, newdata = te)
```
