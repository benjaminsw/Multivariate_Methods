
### Manual Search
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
tunegrid <- expand.grid(.mtry=c(ncol(train.st[,1:14])-1))
modellist <- list()
for (ntree in c(500, 1000, 1500, 2000, 2500)) {
  set.seed(1)
  fit <- train(score ~ ., data=train.st, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control, ntree=ntree)
  key <- toString(ntree)
  modellist[[key]] <- fit
}
# compare results
results <- resamples(modellist)
summary(results)
dotplot(results)




##################### new ###########################
customRF <- list(type = "Classification",library = "randomForest",loop = NULL)
customRF$parameters <- data.frame(parameter = c("mtry", "ntree"),
                                  class = rep("numeric", 2),
                                  label = c("mtry", "ntree"))
customRF$grid <- function(x, y, len = NULL, search = "grid") {}

customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs) {
  randomForest(x, y,
               mtry = param$mtry,
               ntree=param$ntree)
}
#Predict label
customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata)
#Predict prob
customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata, type = "prob")
customRF$sort <- function(x) x[order(x[,1]),]
customRF$levels <- function(x) x$classes

if (!require('doParallel')) install.packages('doParallel'); library('doParallel')
# library(doParallel)
cores <- makeCluster(detectCores()-1)
registerDoParallel(cores = cores)
# train model
metric <- "Accuracy"
control <- trainControl(method="repeatedcv", 
                        number=10, 
                        repeats=3,
                        allowParallel = TRUE)

tunegrid <- expand.grid(.mtry=c(1:15),.ntree=c(1000,1500))

set.seed(123)

custom <- train(score ~ ., data=train.st,
                method=customRF, 
                metric=metric, 
                tuneGrid=tunegrid, 
                trControl=control)

summary(custom)
plot(custom)
