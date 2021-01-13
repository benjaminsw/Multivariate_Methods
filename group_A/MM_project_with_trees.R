data <- read.csv("Data_group_A.csv", header = TRUE)
head(data)
set.seed(2020)

# Install Libraries
library(dplyr)
library(class)
library(MASS)

#### Cleaning and transforming data ####

# Removing columns
data <- data[, -c(1, 3, 8, 11, 12, 13, 16, 18, 19, 23 )]
data <- data[, -c(1,12,13,15)]
colnames(data)

# Variable transformations 
glimpse(data)

data$score <- data$imdb_score
data <- data[, -13] # now remove old imdb_score variable
data$score <- ifelse(data$score<=6,0,1)
data$score <- as.factor(data$score)
str(data$score)

# rounding numbers
data$num_critic_for_reviews <- round(data$num_critic_for_reviews,0)
data$director_facebook_likes <- round(data$director_facebook_likes,0)
data$num_user_for_reviews <- round(data$num_user_for_reviews,0)
data$aspect_ratio <- round(data$aspect_ratio, 2)

data <- as.data.frame(data)



#impute missing value with column means using only data
sum(is.na(data$score)) # no NA's in score variable
colnames(data)
for(i in c(1:14)){
  data[is.na(data[,i]), i] <- mean(data[,i], na.rm = TRUE)
}
colSums(is.na(data))
summary(data)



#### Splitting the Data ####
n <- nrow(data)  
ind1 <- sample(c(1:n),round((3/5)*n))  
ind2 <- sample(c(1:n)[-ind1],round(n/5))  
ind3 <- setdiff(c(1:n),c(ind1,ind2))  

#Standardise all variables 
# Let data1 be the dataset without the y variable imdb   # scores
# colnames(data)
# data1 <- data[, -15] # data1 is data without the score                         # (response variable)
# data.standard<- data1
# data.standard <- scale(data.standard)
# 
# train.data.st <- data.standard[ind1,]  
# valid.data.st <- data.standard[ind2, ]  
# test.data.st <- data.standard[ind3,] 

data.st <- data
data.st[,1:14] <- scale(data.st[,1:14])
as.data.frame(data.st)
train.st <- data.st[ind1,]
valid.st <- data.st[ind2,]
test.st <- data.st[ind3,]

#### kNN ####
y <- data[, 15]
corr.class.rate <- rep(NA,20)

for (k in 1:20){
  set.seed(2020)
  pred <- knn(train.st[,-15],valid.st[,-15],y[ind1],k=k)
  corr.class.rate[k] <- sum(pred==y[ind2])/nrow(valid.st[,-15])
}

best.k <- which.max(corr.class.rate)
best.k 
b = max(corr.class.rate)
b


plot(1:20, corr.class.rate, type = "l", xlab = "k",
     ylab = "Correct Classification Rate",
     main = "kNN ")
abline(v=best.k, col="red")


pred <- knn(train.st[,-15],test.st[,-15],y[ind1],k=best.k)
pred
#sum(pred == y[ind3]) / nrow(test.data.st) # test data


#### Cross-Validation ####
corr.class.rate1 <- rep(NA, 20)

for (k in 1:20) 
{
  set.seed(2020)
  pred <- knn.cv(data.standard[, -14], y, k=k)
  corr.class.rate1[k] <- sum(pred == y) / nrow(data.standard)
}

best1.k <- which.max(corr.class.rate1)
best1.k  ## k=5 gives the best classification rate

max(corr.class.rate1)

plot(1:20, corr.class.rate1, type = "l", xlab = "k", 
     ylab = "Correct Classification Rate",
     main = "Cross-Validation")

abline(v=best1.k, col="red")


pred <- knn(train.data.st,test.data.st,y[ind1],k=best1.k)
pred
sum(pred == y[ind3]) / nrow(test.data.st) # test data


#LDA ####
y <- data[, 15]
y <- ifelse(y==1, "High Score", "Low Score")

fit.lda <- lda(train.st[,-15], y[ind1])

#windows()
plot(fit.lda)

pred.labels.ld <- predict(fit.lda, 
                          newdata = valid.st[,-15])

corr.class.lda.valid <- sum(pred.labels.ld$class == y[ind2]) / length(y[ind2])
corr.class.lda.valid

table(y[ind2], pred.labels.ld$class)

#classification results for test data set

test.ld <- predict(fit.lda, 
                   newdata = test.data.st)
corr.class.lda.test <- sum(test.ld$class == y[ind3]) / length(y[ind3])
corr.class.lda.test

table(y[ind3], test.ld$class)

#QDA ####

fit.qda <- qda(train.st[,-15], y[ind1])
fit.qda

pred.labels.qd <- predict(fit.qda, 
                          newdata = valid.st[,-15])
names(pred.labels.qd)

corr.class.qda.valid <- sum(pred.labels.qd$class == y[ind2]) / length(y[ind2])
corr.class.qda.valid

table(y[ind2], pred.labels.qd$class)

#classification results for test data set

test.ld <- predict(fit.lda, 
                   newdata = test.data.st)
corr.class.lda.test <- sum(test.ld$class == y[ind3]) / length(y[ind3])
corr.class.lda.test

table(y[ind3], test.ld$class)


#CVA ####

fit.cva <- lda(train.st[,-15], y[ind1], prior=c(0.5,0.5)) 
#windows()
plot(fit.cva)

pred.labels.cv <- predict(fit.cva, 
                          newdata = valid.st[,-15])


corr.class.cva.valid <- sum(pred.labels.cv$class == y[ind2]) / length(y[ind2])

corr.class.cva.valid

table(y[ind2], pred.labels.cv$class)

#classification results for test data set

test.cv <- predict(fit.cva, 
                 newdata = test.data.st)
corr.class.cva.test <- sum(test.cv$class == y[ind3]) / length(y[ind3])
corr.class.cva.test

table(y[ind3], test.cv$class)

#tree

library(rpart)
library(rpart.plot)

#as.data.frame(train.data.st)

View(train.data.st)

mov.rt <- rpart(score~., data=train.st, method="class") 
rpart.plot(mov.rt,type=2,extra=4) 

#validation set

mov.valid <- predict(mov.rt, newdata=valid.st[, -15],type="class") 
#classification tree
table(valid.st[, 15], mov.valid)

# Pruning 
printcp(mov.rt)

plotcp(mov.rt)

mov.rt.pruned <- rpart(score~., data=train.st, method="class",cp=0.037234) 
rpart.plot(mov.rt.pruned,type=2,extra=4)

#test set

mov.pred <- predict(mov.rt, newdata=test.st[, -15],type="class") 
#classification tree
table(test.st[, 15], mov.pred)

# Pruning 
printcp(mov.rt)

plotcp(mov.rt)

mov.rt.pruned <- rpart(score~., data=train.st, method="class",cp=0.037234) 
rpart.plot(mov.rt.pruned,type=2,extra=4)



###### random forest ########
# import library
if (!require('randomForest')) install.packages('randomForest'); library('randomForest')
if (!require('pROC')) install.packages('pROC'); library('pROC')
if (!require('caret')) install.packages('caret'); library('caret')
if (!require('mlbench')) install.packages('mlbench'); library('mlbench')
if (!require('e1071')) install.packages('e1071'); library('e1071')
set.seed(2020)
print(prop.table(table(train.st[,15]))*100)
print(prop.table(table(valid.st[,15]))*100)
print(prop.table(table(test.st[,15]))*100)
# Tune RF  
# Extend Caret, set up engine
# ***note: this part take time to run***
metric <- "Accuracy"
customRF <- list(type = "Classification", library = "randomForest", loop = NULL)
customRF$parameters <- data.frame(parameter = c("mtry", "ntree"), class = rep("numeric", 2), label = c("mtry", "ntree"))
customRF$grid <- function(x, y, len = NULL, search = "grid") {}
customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
  randomForest(x, y, mtry = param$mtry, ntree=param$ntree, ...)
}
customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata)
customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata, type = "prob")
customRF$sort <- function(x) x[order(x[,1]),]
customRF$levels <- function(x) x$classes
# train model
control <- trainControl(method="repeatedcv", number=10, repeats=3)
tunegrid <- expand.grid(.mtry=c(1:14), .ntree=c(10, 50, 100, 500, 1000, 5000))
set.seed(1)
rf_classifier <- train(score ~ ., data=train.st, method=customRF, metric=metric, tuneGrid=tunegrid, trControl=control)
summary(rf_classifier)
plot(rf_classifier, main="ntree and mtry Optimisation")

# variable importance type 2 plot
varImpPlot(rf_classifier$finalModel,type=2, main="Variable Importance Plot")

# predict class
rf_prob_df <- as.data.frame(predict(rf_classifier$finalModel, newdata=valid.st[, -15], type="prob"))
rf_prob_df$observed <- valid.st[,15]

# ROC curve
roc(rf_prob_df$observed, rf_prob_df$'1', plot=TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", main="ROC plot", col="#377eb8", lwd=4, print.auc=TRUE)
# find out optimal threshold 
roc_info <- roc(rf_prob_df$observed, rf_prob_df$'1', legacy.axes=TRUE)
str(roc_info)
# extract just the information that we want 
roc_df <- data.frame(
  tpp=roc_info$sensitivities*100,         # tpp = true positive percentage
  fpp=(1 - roc_info$specificities)*100,   # fpp = false positive precentage
  thresholds=roc_info$thresholds)
# now let's look at the thresholds between FPP 40% and 50%...
roc_df[roc_df$fpp > 40 & roc_df$fpp < 50,]

# Confusion Matrix - Random Forest
# select tpp=91.52542 fpp=41.93548 thresholds=0.4655
rf_predicted_label <- ifelse(rf_prob_df[,2]>0.4655,1,0)
rf_cm <- table(rf_predicted_label, valid.st[,15])
rf_cm 
# accuracy
(rf_cm[1]+rf_cm[4])/sum(rf_cm)

#### flag column names
col_old_df <- data.frame("colname"=colnames(data), "keep" = "No")
col_new_df<-colnames(train.st)
col_old_df$keep[col_old_df$colname %in% col_new_df] <- "Yes"

## test performance on test dataset
rf_prob_df <- as.data.frame(predict(rf_classifier$finalModel, newdata=test.st[, -15], type="prob"))
rf_prob_df$observed <- test.st[,15]

rf_predicted_label <- ifelse(rf_prob_df[,2]>0.4655,1,0)
rf_cm <- table(rf_predicted_label, test.st[,15])
rf_cm 
# accuracy
(rf_cm[1]+rf_cm[4])/sum(rf_cm)


