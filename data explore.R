
#Changing no internet service to "NO"
cols_recode1 <- c(10:15)
for(i in 1:ncol(churn[,cols_recode1])) {
  churn[,cols_recode1][,i] <- as.factor(mapvalues(churn[,cols_recode1][,i], from =c("No internet service"),to=c("No")))
}

#changing no phone service to "NO"
churn$MultipleLines <- as.factor(mapvalues(churn$MultipleLines, from=c("No phone service"),to=c("No")))

#Grouping tenure

group_tenure <- function(tenure){
  if (tenure >= 0 & tenure <= 12){
    return('0-12 Month')
  }else if(tenure > 12 & tenure <= 24){
    return('12-24 Month')
  }else if (tenure > 24 & tenure <= 48){
    return('24-48 Month')
  }else if (tenure > 48 & tenure <=60){
    return('48-60 Month')
  }else if (tenure > 60){
    return('> 60 Month')
  }
}

data$tenure_group <- sapply(data$tenure,group_tenure)
data$tenure_group <- as.factor(data$tenure_group)

#Exploratory plots

p1 <- ggplot(data, aes(x=gender)) + ggtitle("Gender") + xlab("Gender") +
  +     geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()

 p2 <- ggplot(data, aes(x=SeniorCitizen)) + ggtitle("Senior Citizen") + xlab("Senior Citizen") + 
  +     geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()

 jpeg("explore1.jpg")
 p5 <- ggplot(data, aes(x=PhoneService)) + ggtitle("Phone Service") + xlab("Phone Service") +
   geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
 p6 <- ggplot(data, aes(x=MultipleLines)) + ggtitle("Multiple Lines") + xlab("Multiple Lines") + 
   geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
 p7 <- ggplot(data, aes(x=InternetService)) + ggtitle("Internet Service") + xlab("Internet Service") + 
   geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
 p8 <- ggplot(data, aes(x=OnlineSecurity)) + ggtitle("Online Security") + xlab("Online Security") +
   geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
 grid.arrange(p5, p6, p7, p8, ncol=2) 
 dev.off()
 
 jpeg("explore2.jpg")
 p9 <- ggplot(data, aes(x=OnlineBackup)) + ggtitle("Online Backup") + xlab("Online Backup") +
   geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
 p10 <- ggplot(data, aes(x=DeviceProtection)) + ggtitle("Device Protection") + xlab("Device Protection") + 
   geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
 p11 <- ggplot(data, aes(x=TechSupport)) + ggtitle("Tech Support") + xlab("Tech Support") + 
   geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
 p12 <- ggplot(data, aes(x=StreamingTV)) + ggtitle("Streaming TV") + xlab("Streaming TV") +
   geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
 grid.arrange(p9, p10, p11, p12, ncol=2)
 dev.off()
 
 jpeg("explore3.jpg")
 p13 <- ggplot(data, aes(x=StreamingMovies)) + ggtitle("Streaming Movies") + xlab("Streaming Movies") +
   geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
 p14 <- ggplot(data, aes(x=Contract)) + ggtitle("Contract") + xlab("Contract") + 
   geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
 p15 <- ggplot(data, aes(x=PaperlessBilling)) + ggtitle("Paperless Billing") + xlab("Paperless Billing") + 
   geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
 p16 <- ggplot(data, aes(x=PaymentMethod)) + ggtitle("Payment Method") + xlab("Payment Method") +
   geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
 #p17 <- ggplot(data, aes(x=tenure_group)) + ggtitle("Tenure Group") + xlab("Tenure Group") +
  # geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
 grid.arrange(p13, p14, p15, p16, ncol=2)
 
dev.off()
 
 
 