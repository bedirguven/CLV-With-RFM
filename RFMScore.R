# Getting data
data <- read.table("CDNOW_sample.txt")

#Removed first column because we don't need it.
data$V1<- NULL

#Changed column names for understandable
colnames(data)<-c("ID","DATE","COUNT","PRICE")

#Changed id and date variable type
data$ID <- as.factor(as.character(data$ID))
data$DATE <- as.Date(as.character(data$DATE),"%Y%m%d")

#We need to define date value as referance date
refDate <- max(data$DATE)

#Calculating recency value
library(dplyr)
recency <- data %>% group_by(ID) %>% summarise(Recency = as.numeric(refDate) - as.numeric(max(DATE)))

#Calculating frequency
frequency <- data %>% group_by(ID) %>% summarise(Frequency = n())

#Calculating monetary
monetary <- data %>% group_by(ID) %>% summarise(Monetary = sum(PRICE))

#Merging prepared data
rfmValue <- merge(recency,frequency,by="ID")
rfmValue <- merge(rfmValue,monetary,by="ID")

#Calculating monetary score between 1-5
quantile(rfmValue$Monetary)
monetaryRanks <- cut(rfmValue$Monetary,breaks = c(0,20,45,105,1000,6600))
levels(monetaryRanks) <- c(1,2,3,4,5)
head(monetaryRanks)

#Calculating recency score between 1-5
quantile(rfmValue$Recency)
recencyRanks <- cut(rfmValue$Recency,breaks = c(0,60,217,473,506,550))
levels(recencyRanks) <- c(5,4,3,2,1)
head(recencyRanks)

#Calculating frequency score between 1-5
quantile(rfmValue$Frequency)
frequencyRanks <- cut(rfmValue$Frequency,breaks = c(0,1,2,3,7,60))
levels(frequencyRanks) <-  c(1,2,3,4,5)
head(frequencyRanks)


#Merging frequency,recency and monetray data in one table
rfmScores <- data.frame(cbind(rfmValue$ID,recencyRanks,frequencyRanks,monetaryRanks))
colnames(rfmScores) <- c("ID","Recency","Frequency","Monetary")
