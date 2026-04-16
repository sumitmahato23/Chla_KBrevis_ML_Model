
#INSTALL ALL LIBRARIES
source("C:\\ResearchR\\Library.R")
name <- "RRF_Base"


#DEFINE ML AlGORITHM
trainmethod = "RRF"
validation = 1
if (validation == 1) {out <- "70%training"} else {out <- "30%validation"}

#STEP1: Get Dataset
# Retrieve dataset
matchup_data_clean <- read.csv("C:\\Research\\data\\matchup_data.csv")

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
source("C:\\Research\\Features.R")


#Hyper parametric Tuning

#grid_base <- expand.grid(
#  mtry = seq(2,15,1),
#  coefReg = c(0.5, 0.7, 0.9),
#  coefImp = c(0.3, 0.5, 0.7)
#)



grid_final <- expand.grid(
  mtry = c(6,8,10),
  coefReg = 0.9, 
  coefImp = 0.5)


#Model Training
model <- train_model(train, features_1, grid_final, trainmethod)


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

