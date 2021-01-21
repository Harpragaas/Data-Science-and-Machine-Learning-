# Regression and Classification code
# Regressor Model - 'ranger'
# Classification Model - 'xgbDART'

# Harpragaas Singh - u6424131
# Abhinav Pandey - u6724645

#Importing the libraries
library('caret')
library('dplyr')

#Load the dataset
data_df <- read.csv("sample_users_100k.csv.bz2", sep="\t", stringsAsFactors = F)
test_df = read.csv("testing_set_features.csv.bz2", sep="\t", stringsAsFactors = F)

# make botscore numerical and correct invalid data
data_df$botscore <- as.numeric(data_df$botscore)
toDel <- which(data_df$botscore < 0)
data_df <- data_df[-toDel,]
toDel <- which(is.na(data_df$botscore))

#Data Preprocessing
features = c(
  'friendsCount', #na to mean
  'followersCount', #na to mean
  'statusesCount', #na to mean
  'listedCount', #na to mean
  'influence', #na to mean
  'influence_percentile', #na to mean
  'favoritesCount', #na to mean
  'mcsize', #na to mean
  'retweetsCount', # na to median
  'sum_betweenness', # na to median
  'utcOffset', #na to mean
  'botscore') # drop na

# Same features as training set except for 'botscore'
features_test = c(
  'user_id',
  'friendsCount', #na to mean
  'followersCount', #na to mean
  'statusesCount', #na to mean
  'listedCount', #na to mean
  'influence', #na to mean
  'influence_percentile', #na to mean
  'favoritesCount', #na to mean
  'mcsize', #na to mean
  'retweetsCount', # na to median
  'sum_betweenness', # na to median
  'utcOffset') #na to mean

# keep only selected features
data_df <- data_df[, features]
test_df = test_df[, features_test]

#impute mean values in empty spaces for friendsCount
data_df$friendsCount[is.na(data_df$friendsCount)] <- mean(data_df$friendsCount, na.rm=TRUE)
test_df$friendsCount[is.na(test_df$friendsCount)] <- mean(test_df$friendsCount, na.rm=TRUE)

#impute mean values in empty spaces for followersCount
data_df$followersCount[is.na(data_df$followersCount)] <- mean(data_df$followersCount, na.rm=TRUE)
test_df$followersCount[is.na(test_df$followersCount)] <- mean(test_df$followersCount, na.rm=TRUE)

#impute mean values in empty spaces for statusesCount
data_df$statusesCount[is.na(data_df$statusesCount)] <- mean(data_df$statusesCount, na.rm=TRUE)
test_df$statusesCount[is.na(test_df$statusesCount)] <- mean(test_df$statusesCount, na.rm=TRUE)

#impute mean values in empty spaces for listedCount
data_df$listedCount[is.na(data_df$listedCount)] <- mean(data_df$listedCount, na.rm=TRUE)
test_df$listedCount[is.na(test_df$listedCount)] <- mean(test_df$listedCount, na.rm=TRUE)

#impute mean values in empty spaces for influence
data_df$influence[is.na(data_df$influence)] <- mean(data_df$influence, na.rm=TRUE)
test_df$influence[is.na(test_df$influence)] <- mean(test_df$influence, na.rm=TRUE)

#impute mean values in empty spaces for influence_percentile
data_df$influence_percentile[is.na(data_df$influence_percentile)] <- mean(data_df$influence_percentile, na.rm=TRUE)
test_df$influence_percentile[is.na(test_df$influence_percentile)] <- mean(test_df$influence_percentile, na.rm=TRUE)

#impute mean values in empty spaces for favoritesCount
data_df$favoritesCount[is.na(data_df$favoritesCount)] <- mean(data_df$favoritesCount, na.rm=TRUE)
test_df$favoritesCount[is.na(test_df$favoritesCount)] <- mean(test_df$favoritesCount, na.rm=TRUE)

#impute mean values in empty spaces for mcsize
data_df$mcsize[is.na(data_df$mcsize)] <- mean(data_df$mcsize, na.rm=TRUE)
test_df$mcsize[is.na(test_df$mcsize)] <- mean(test_df$mcsize, na.rm=TRUE)

#impute median values in empty spaces for sum_betweenness
data_df$sum_betweenness[is.na(data_df$sum_betweenness)] <- median(data_df$sum_betweenness, na.rm=TRUE)
test_df$sum_betweenness[is.na(test_df$sum_betweenness)] <- median(test_df$sum_betweenness, na.rm=TRUE)

#impute median values in empty spaces for retweetsCount
data_df$retweetsCount[is.na(data_df$retweetsCount)] <- median(data_df$retweetsCount, na.rm=TRUE)
test_df$retweetsCount[is.na(test_df$retweetsCount)] <- median(test_df$retweetsCount, na.rm=TRUE)

#impute mean values in empty spaces for utcOffset
data_df$utcOffset[is.na(data_df$utcOffset)] <- mean(data_df$utcOffset, na.rm=TRUE)
test_df$utcOffset[is.na(test_df$utcOffset)] <- mean(test_df$utcOffset, na.rm=TRUE)

# remove non-complete entries from training set
toKeep <- rowSums(is.na(data_df)) == 0
data_df <- data_df[toKeep, ]

# remove non-complete entries from test set
toKeep_test <- rowSums(is.na(test_df)) == 0
test_df <- test_df[toKeep_test, ]

# Fitting Random Forest to the dataset
model_rf <- train(form = botscore ~ .,
                  data = training_set, 
                  method = "ranger")

# predict the outcome on a test set
model_rf_pred <- predict(model_rf, test_set)

#Predicted vs Observed
# head(data.frame(predicted = model_rf_pred, 
#                 observed = test_set$botscore, row.names = NULL))
# 
# #Postresample - Model Predictors
# ARE_df <- data.frame(matrix(data = NA, nrow = nrow(test_set), ncol = 0))
# 
# metrics <- data.frame(matrix(data = NA, nrow = 3, ncol = 0))
# metrics <- cbind(metrics, RF = postResample(pred = model_rf_pred, 
#                                             obs = test_set$botscore) )
# metrics
# 
# ARE_df <- cbind(ARE_df, RF = abs(model_rf_pred - test_set$botscore) / test_set$botscore)
# ARE_df <- do.call(data.frame, lapply(ARE_df, function(x) replace(x, is.infinite(x), NA)))
# 
# # compare predicted outcome and true outcome
# metrics <- cbind(metrics, 
#                  RF = c(postResample(pred = model_rf_pred, obs = test_set$botscore), 
#                         mean(ARE_df$RF, na.rm = T) ) )

#Copying data_df to classification dataframe
class_data_df = data_df

# binarize the botscore
class_data_df$is_bot <- FALSE
class_data_df$is_bot[class_data_df$botscore > 0.5] <- TRUE
class_data_df$is_bot <- factor(class_data_df$is_bot)
class_data_df$botscore <- NULL

# Splitting the dataset into the Training set and Test set
# library(caTools)
# set.seed(123)
# split = sample.split(class_data_df$is_bot, SplitRatio = 0.8)
# training_set = subset(class_data_df, split == TRUE)
# test_set = subset(class_data_df, split == FALSE)

# Feature Scaling
#training_set = scale(training_set)
#test_set = scale(test_set)

# XGBoost classifier
ctrl <- trainControl(method = "repeatedcv", repeats = 5,
                     ## undersampling:
                     sampling = "down")

# train(is_bot ~ .)
classifier_xgb = train(is_bot ~ friendsCount + followersCount + statusesCount + listedCount + influence + influence_percentile + favoritesCount + mcsize + retweetsCount + sum_betweenness + utcOffset, data = class_data_df, 
                      method = "xgbDART",
                      trControl = ctrl)

# Making Prediction with XGBoost Model
xgb_pred = predict(classifier_xgb, test_df, type="raw")
postResample(pred = dt_pred, obs = test_df$is_bot)

#Add user_id and predicted scores to csv file
predicted_is_bot = data.frame(user_id = test_df$user_id,
                              botscore = test_df$botscore,
                              is_bot = xgb_pred, 
                              row.names = NULL)

write.csv(predicted_is_bot, file = 'predicted_is_bot.csv')


# Confusion Matrix for XGBoost classifier
# pred_obs <- data.frame(predicted = dt_pred, observed = test_set$is_bot)
# confusionMatrix(data = dt_pred, reference = test_set$is_bot, 
#               dnn = c("Predicted", "Observed"), positive = 'TRUE', mode = 'everything')

# Measures for XGBoost
# measures <- c(precision = precision(data = dt_pred, 
#                                     reference = test_set$is_bot,
#                                     relevant = 'TRUE'),
#               recall = recall(data = dt_pred, 
#                               reference = test_set$is_bot, 
#                               relevant = 'TRUE'),
#               fmeasure = F_meas(data = dt_pred, 
#                                 reference = test_set$is_bot, 
#                                 relevant = 'TRUE') )
# print(measures, digits = 2)