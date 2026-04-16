#INSTALL ALL LIBRARIES
source("C:\\ResearchR\\Library_KB.R")
name <- "XGBTree_Base"
# Change to your directory


#DEFINE ML AlGORITHM
trainmethod = "xgbTree"
validation = 1
if (validation == 1) {out <- "70%training"} else {out <- "30%validation"}

#STEP1: Get Dataset
# Retrieve dataset
matchup_data_clean <- read.csv("C:\\Research\\dataKB\\matchup_data.csv")

#STEP2: Create TRAIN and VALIDATION
df <- data_partition(matchup_data_clean)


df <- df%>%
  holdout() %>%
  ungroup() %>%
  mutate(value = log(ResultMeasureValue)) %>%  
  filter(value!=-Inf)%>%
  mutate_if(is.character,as.factor) %>%
  as.data.frame()%>%suppressWarnings()


train_test <- create_train_validate(df = df,train_partition = 1,validate_partition = validation) 


train <- train_test$train_set
validate <- train_test$validate_set


#STEP 3: Develop a ML Model 
#Get Selected features : Feature_1 is the list of significant features used to develop this model
source("C:\\Research\\features_KB.R")


#Hyper parametric Tuning

#grid_base <- expand.grid(
#nrounds = seq(50, 70, 10),
#max_depth = seq(500, 900, 100), 
#colsample_bytree = c(0.05, 0.1),
#eta = c(0.05, 0.1),
#gamma = c(0.5, 1),
#min_child_weight = c(5, 15),
#subsample = c(0.5, 0.75))




grid_final <- expand.grid( 
  nrounds = 70, 
  max_depth =900, 
  colsample_bytree = 0.1, 
  eta = 0.05, 
  gamma =1, 
  min_child_weight =15, 
  subsample =0.5)


#Model Training
model <- train_model(train, features_2, grid_final, trainmethod)


#STEP 4: Model Performance Evaluation
#Estimation
output<- prediction_function(model, validate)


#Model Evaluation
evals <- output %>%
  mutate(Actual = (Actual), 
         Predicted = (Predicted)) %>%
  summarise(rmse = rmse(Actual, Predicted),         
            MAE = percent_bias_n(Actual, Predicted), 
            NSE = NSE(Predicted, Actual),        
            rsquared = cor(Actual, Predicted)^2  
  )


print_result(evals)

