suppressPackageStartupMessages({
  
  library(feather)
  library(viridis)
  library(maps)
  library(magrittr)
  library(mlbench)
  library(caret)
  library(randomForest)
  library(doParallel)
  library(onehot)
  library(xgboost)
  library(Metrics)
  library(purrr)
  library(data.table)
  library(mltools)
  library(ggthemes)
  library(dplyr)
  library(ggplot2)
  library(mltools)
  library(CAST)
  library(future)
  library(foreign)
  library(dplyr)
  library(stringr)
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(purrr)
  library(tibble)
  library(stringr)
  library(forcats)
  library(UBL)
  library(hydroGOF)
  library(NbClust)
  
  
  holdout <- function(x) {
    set.seed(22)
    
    train <- x %>%
      group_by(time_group, long_group, mag) %>%
      sample_frac(.7) %>%
      ungroup() %>%
      dplyr::mutate(.partitions = 1)
    
    set.seed(22)
    
    validate <- x %>%
      anti_join(train) %>%
      dplyr::mutate(.partitions = 2)
    
    out <- train %>%
      bind_rows(validate) 
    
    return(out)
  }
  
  
})

#----------------------------------------------------------------
train_model <- function(train, features_1, grid_final, trainmethod) {
  set.seed(10)
  
  # Create a parallel cluster
  cl <- makePSOCKcluster(detectCores() - 2)
  registerDoParallel(cl)
  
  # Create spatial-temporal folds
  folds <- CreateSpacetimeFolds(train, spacevar = "long_group", timevar = "time_group", k = 3)
  
  # Define training control
  train_control_final <- trainControl(
    method = "cv",
    savePredictions = TRUE,
    returnResamp = 'final',
    index = folds$index,
    indexOut = folds$indexOut,
    verboseIter = TRUE,
    allowParallel = TRUE,
    p = 0.8
  )
  
  model <- caret::train(
    x = train[,features_1],
    y = train$log_Value,
    trControl = train_control_final,
    tuneGrid = grid_final,
    method = train_method,
    importance = T,
    verbose = TRUE)
  
  # Stop the parallel cluster
  stopCluster(cl)
  
  return(model)
}
#----------------------------------------------------------------

#---------------------------------------------------------------- 
percent_bias_n <- function(actual, predicted){
  return (mean((abs(actual-predicted)/actual)))
}

#----------------------------------------------------------------
prediction_function <- function(model, data) {
  pred<- predict(model,validate[,features_1])
  actual <- (validate$log_Value)
  lon = validate$lon
  lat = validate$lat
  date = validate$ActivityStartDate
  Collection.M = validate$SampleCollectionMethod.MethodIdentifier
  Analytical.M = validate$ResultAnalyticalMethod.MethodIdentifier
  ID <- validate$site_no
  
  output <- tibble(
    Predicted = exp(pred),
    Actual = exp(actual),
    date = as.Date(date),
    long = lon,
    lat = lat,
    Collection = Collection.M,
    Analytical = Analytical.M
  ) %>%
    mutate(
      residual = Actual - Predicted,
      year = year(date),
      month = month(date),
      error.1 = abs(Actual - Predicted) * 100 / Actual,
      error.2 = abs(Actual - Predicted) * 100 / Predicted
    )
  
  return(output)
}


#------------------------------------------------------------
run_feature_selection <- function(train, iter, out_folder, features) {
  tuneGrid1 <- expand.grid(
    nrounds = 100,
    eta = 0.01,
    lambda = 0.8,
    alpha = 0.8
  )
  
  set.seed(22)
  folds <- CreateSpacetimeFolds(
    train,
    spacevar = "long_group",
    timevar = "time_group",
    k = 3
  )
  
  control <- trainControl(
    method = "cv",
    savePredictions = 'none',
    returnResamp = 'final',
    index = folds$index,
    indexOut = folds$indexOut,
    p = 0.8
  )
  
  cl <- makePSOCKcluster(availableCores() - 2)
  registerDoParallel(cl)
  
  ffs <- ffs(
    train[, features],
    train$value,
    method = 'xgbLinear',
    metric = 'RMSE',
    tuneGrid = tuneGrid1,
    Control = control,
    verbose = TRUE
  )
  
  on.exit(stopCluster(cl))
  registerDoSEQ()
  
  ffsResults <- ffs$perf_all
  
  write.csv(
    ffsResults,
    sprintf('%s/ffsResults_%s.csv', out_loc, iter))
  
  
  save(
    ffs,
    file = sprintf('%s/ffs_%s.RData', out_loc, iter)
  )
  
  features_1 <- ffsResults[ffsResults$RMSE == min(ffsResults$RMSE), ]
  
  
  if (nrow(features_1) > 1) {
    features_1 <- features_1[features_1$nvar == min(features_1$nvar), ] %>%
      dplyr::select(-c(nvar, RMSE, SE)) %>%
      paste(.) %>% .[. != 'NA']
  } else {
    features_1 <- features_1 %>%
      dplyr::select(-c(nvar, RMSE, SE)) %>%
      paste(.) %>% .[. != 'NA']
  }
  
  write.csv(features_1, sprintf('%s/featuresSelected.csv', out_loc, iter))
  
  
  return(features_1)
}

#-------------------------------------------------------------------------------


data_partition <- function(df) {
  df$lat_group <- cut_number(df$Latitude, 2, right= F) 
  df$long_group<- cut_number(df$Longitude, 3, right= F)
  df<- df[!is.na(df$Chlorophyll.a..ug.L.), ]
  df <- df %>%
    mutate(ActivityStartDate = trimws(ActivityStartDate),
           date = lubridate::ymd(ActivityStartDate),
           julian = as.numeric(format(date, "%j")),
           value = as.numeric(Chlorophyll.a..ug.L.),
           space_group = paste0(lat_group,long_group)
    ) %>%
    filter(!is.na(value))
  
  df$time_group = cut_number(df$julian, 2, right=F)
  df$log_Value <- log(df$Chlorophyll.a..ug.L.)
  
  df$mag_c <- cut(df$log_Value, quantile(
    x = df$log_Value,
    c(0, 0.2, 0.4, 0.6, 0.7, 0.75, 0.8, 0.85, 0.9, 0.93, 0.96, 1),
    include.lowest = T))
  
  df$mag <- factor(df$mag_c, levels = levels(df$mag_c))
  
  return(df)
} 

#-------------------------------------------------------------------------------

create_train_validate <- function(df, label_value = NULL, train_partition, validate_partition) {
  if (is.null(label_value)) {
    df_filtered <- df %>% filter(.partitions %in% c(train_partition, validate_partition))
  } else {
    df_filtered <- df %>% filter(label == label_value, .partitions %in% c(train_partition, validate_partition))
  }
  
  train_set <- df_filtered %>%
    filter(.partitions == train_partition) %>%
    ungroup() %>%
    as.data.frame()
  
  validate_set <- df_filtered %>%
    filter(.partitions == validate_partition) %>%
    ungroup() %>%
    as.data.frame()
  
  return(list(train_set = train_set, validate_set = validate_set))
}
