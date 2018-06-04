## Code for SVM ....
library(pROC)


trIndex <- createDataPartition(Churncv$Churn, p = .8, list = FALSE)
testsvm = Churncv[-trIndex,]
trainsvm = Churncv[trIndex,]

library(e1071)

gammalist <- c(0.005,0.01,0.015,0.02,0.025,0.03,0.035,0.04,0.045,0.05)
## Radial function

tune.out <- tune.svm(Churn ~., data=trainsvm, 
                     kernel='radial', cost=2^(-1:5), gamma = gammalist)
summary(tune.out)
summary(tune.out$best.model)
svm1 <- predict(tune.out$best.model, testsvm[,-47])
confusionMatrix(svm1, testsvm$Churn)

#ROC and AUC
roc(testsvm$Churn, as.numeric(svm1))
par(pty="s")
plot(roc(testsvm$Churn, as.numeric(svm1)),
     print.thres = T,
     print.auc=T)

## Linear function
tune.out2 <- tune.svm(Churn ~., data=trainsvm, 
                     kernel='linear', cost=2^(-1:5), gamma = gammalist)
svm2 <- predict(tune.out2$best.model, testsvm[,-47])
confusionMatrix(svm2, testsvm$Churn)

#ROC and AUC

roc(testsvm$Churn, as.numeric(svm2))
plot(roc(testsvm$Churn, as.numeric(svm2)),
     print.thres = T,
     print.auc=T)

##### Now SVm on the testing set. 

svmfinal <- predict(tune.out$best.model, Churntest[,-47])
confusionMatrix(svmfinal, Churntest$Churn)
jpeg("svmfin.jpg")
plot(roc(Churntest$Churn, as.numeric(svmfinal)),
     print.thres = T,
     print.auc=T)
title("SVM")
dev.off()
roc(Churntest$Churn, as.numeric(svmfinal))

