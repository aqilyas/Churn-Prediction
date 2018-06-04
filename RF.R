#### Random forest ......
library(pROC)

trIndex <- createDataPartition(Churncv$Churn, p = .8, list = FALSE)
testrf = Churncv[-trIndex,]
trainrf = Churncv[trIndex,]

library(randomForest)
library(h2o)
#rf1 = h2o.randomForest(training_frame = trainrf, validation_frame = testrf, 
#                       x= 1:46, y = 47, ntrees = 200, stopping_rounds = 2,
#                       score_each_iteration = T)
rfmod <- randomForest(Churn ~ ., data = trainrf, ntree = 150, mtry = 5)
rfpred = predict(rfmod, testrf, type = "prob")[,2]
plot(roc(testrf$Churn, rfpred),
     print.thres = T,
     print.auc=T)

## Now on the final set
rfpred = predict(rfmod, Churntest, type = "prob")[,2]
jpeg("rffin.jpg")
plot(roc(Churntest$Churn, rfpred),
     print.thres = T,
     print.auc=T)
title("Random Forest")
dev.off()


