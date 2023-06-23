

## ---------------------------------------------------------------------------------------------------
# loading libraries
library(tidyverse)
library(tidymodels)
library(xgboost)

## ---------------------------------------------------------------------------------------------------
# Read data
tr <- read_csv("mouse_tr.csv") %>%
  mutate(celltype = factor(celltype))
ts <- read_csv("mouse_ts_mask.csv")
ts_sol <- read_csv("mouse_ts_samp.csv")


## ---------------------------------------------------------------------------------------------------
# Merging all the test sets using the commomn variable "location"
ts<- merge(x=ts,y=ts_sol, by="location", all.x = TRUE)


## ---------------------------------------------------------------------------------------------------
# Specify cross-validation method and number of folds.
xgb_trcontrol = trainControl(
  method = "cv",
  number = 10)
# Finally, train the model
model<- train( celltype~., data = tr[,-1], trControl= xgb_trcontrol, method= "xgbTree")

# Best values for hyperparameters
model$bestTune


## ---------------------------------------------------------------------------------------------------
# Label conversion
# XGBoost requires the classes to be in an integer format, starting with 0. So, the first class should be 0. The celltype factor is converted to the proper integer format.

# Convert the celltype factor to an integer class starting at 0
# This is picky, but it's a requirement for XGBoost

celltype<- tr$celltype

label_tr<- as.integer(tr$celltype)-1
tr$celltype<- NULL


label_ts<- as.integer(factor(ts$celltype))-1
ts$celltype<- NULL

train.data = as.matrix(tr[,-1])
train.label = label_tr
test.data = as.matrix(ts[,-1])
test.label = label_ts



## ---------------------------------------------------------------------------------------------------
# Create the xgb.DMatrix objects
# Next, we transform the training and testing data sets into xgb.DMatrix objects that are used for fitting the XGBoost model and predicting new outcomes.
# Transform the two data sets into xgb.Matrix
xgb.train = xgb.DMatrix(data=train.data,label=train.label)
xgb.test = xgb.DMatrix(data=test.data,label=test.label)


## ---------------------------------------------------------------------------------------------------
#XGBoost, like most other algorithms, works best when its parameters are hypertuned for optimal performance. The algorithm requires that we define the booster, objective, learning rate, and other parameters. The following uses a set of parameters that I found to be optimal through simple cross-validation.

#The multi:softprob objective tells the algorithm to calculate probabilities for every possible outcome for every observation.

# Define the parameters for multinomial classification
num_class = length(levels(celltype))
params = list(
  booster="gbtree",
  eta=0.3,
  max_depth=1,
  gamma=0,
  subsample=0.75,
  colsample_bytree=0.6,
  objective="multi:softprob",
  eval_metric="mlogloss",
  num_class=num_class
)


## ---------------------------------------------------------------------------------------------------
# Train the XGBoost classifer
xgb.fit=xgb.train(
  params=params,
  data=xgb.train,
  nrounds=150,
  nthreads=1,
  watchlist=list(val1=xgb.train,val2=xgb.test),
  verbose=0
)



## ---------------------------------------------------------------------------------------------------
#Now we can predict new outcomes given the testing data set that we set aside earlier. We use the predict function to predict the likelihood of each observation in test.data of being each celltype.

# Predict outcomes with the test data
xgb.pred = predict(xgb.fit,test.data,reshape=T)
xgb.pred = as.data.frame(xgb.pred)
colnames(xgb.pred) = levels(celltype)


## ---------------------------------------------------------------------------------------------------
# Make predictions
mouse_pred <- ts %>% # Use the predicted label with the highest probability
  mutate(celltype = apply(xgb.pred,1,function(x) colnames(xgb.pred)[which.max(x)]))

write_csv(mouse_pred[,c(1, 1005)], file="mouse_mypred.csv")



## ---------------------------------------------------------------------------------------------------
#Reference: 

#Kube, D., 2019. XGBoost Multinomial Classification Iris Example in R. [online] Rstudio-pubs-static.s3.amazonaws.com. Available at: <https://rstudio-pubs-static.s3.amazonaws.com/456044_9c275b0718a64e6286751bb7c60ae42a.html> [Accessed 27 May 2022].
#Berhane, F., 2018. Extreme Gradient Boosting with R | DataScience+. [online] Datascienceplus.com. Available at: <https://datascienceplus.com/extreme-gradient-boosting-with-r/> [Accessed 27 May 2022].


