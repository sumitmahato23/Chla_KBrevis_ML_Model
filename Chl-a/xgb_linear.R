#INSTALL ALL LIBRARIES
source("C:\\ResearchR\\Library.R")
name <- "XGBLinear_Base"


#DEFINE ML AlGORITHM
trainmethod = "xgbLinear"
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

# grid_base <- expand.grid(
#   nrounds = seq(10,100,10),
#   alpha = seq(0.1,1,0.1),
#   lambda = seq(0.1,1,0.1),
#   eta = c(0.01, 0.05, 0.1))



grid_final <- expand.grid(
  nrounds = 40, 
  alpha = 0.1, 
  lambda= 1, 
  eta= 0.01) 


#Model Training
model <- train_model(train, features_1, grid_final, trainmethod)


#STEP 4: Model Performance Evaluation
#Estimation
output<- prediction_function(model, validate)


#Model Evaluation
evals <- output %>%
  mutate(Actual = (Actual), 
         Predicted = (Predicted)) %>%
  summarise(rmse = rmse(Actual, Predicted),          # Calculate RMSE
            MAE = percent_bias_n(Actual, Predicted), # Calculate MAE
            NSE = NSE(Predicted, Actual),        # Calculating NSE
            rsquared = cor(Actual, Predicted)^2  # Calculating R-squared
  )


print_result(evals)

