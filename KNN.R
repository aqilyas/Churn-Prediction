## code for Knn ....
# Dataset used is Churncv
#now spliting again
trIndex <- createDataPartition(Churncv$Churn, p = .8, list = FALSE)
testknn = Churncv[-trIndex,]
trainknn = Churncv[trIndex,]
# k =5
modknn5 <- class::knn(cl = trainknn$Churn,
                  test = testknn[,1:46],
                  train = trainknn[,1:46],
                  k = 5,
                  prob = TRUE)
roc(testknn$Churn, attributes(modknn5)$prob)  ##auc = 0.344
par(pty="s")
plot(roc(testknn$Churn, attributes(modknn5)$prob),
          print.thres = T,
          print.auc=T)
#k=10
modknn10 <- class::knn(cl = trainknn$Churn,
                  test = testknn[,1:46],
                  train = trainknn[,1:46],
                  k = 10,
                  prob = TRUE)
roc(testknn$Churn, attributes(modknn10)$prob)
plot(roc(testknn$Churn, attributes(modknn10)$prob),
     print.thres = T,
     print.auc=T)

#k=15
modknn15 <- class::knn(cl = trainknn$Churn,
                       test = testknn[,1:46],
                       train = trainknn[,1:46],
                       k = 15,
                       prob = TRUE)
roc(testknn$Churn, attributes(modknn15)$prob)
plot(roc(testknn$Churn, attributes(modknn15)$prob),
     print.thres = T,
     print.auc=T)

#k=30
modknn30 <- class::knn(cl = trainknn$Churn,
                       test = testknn[,1:46],
                       train = trainknn[,1:46],
                       k = 30,
                       prob = TRUE)
roc(testknn$Churn, attributes(modknn30)$prob)
plot(roc(testknn$Churn, attributes(modknn30)$prob),
     print.thres = T,
     print.auc=T)


## Probabilities of predictions are in : attributes(mod)$prob

### NOw on the final set
modknn30 <- class::knn(cl = trainknn$Churn,
                       test = Churntest[,1:46],
                       train = trainknn[,1:46],
                       k = 30,
                       prob = TRUE)
roc(Churntest$Churn, attributes(modknn30)$prob)
jpeg("knnfin.jpg")
plot(roc(Churntest$Churn, attributes(modknn30)$prob),
     print.thres = T,
     print.auc=T)
title("KNN")
dev.off()





