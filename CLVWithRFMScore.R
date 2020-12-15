df <- read.table("CDNOW_master.txt",header=F)

head(df)

# construct a data frame with the necessary columns of customer ID, transaction date, and money amount paid by a customer # per transaction
df <- as.data.frame(cbind(df[,1],df[,2],df[,4]))

# add appropriate column names for the above three column and 
names <- c("ID","Date","Amount")
names(df) <- names

#tranfer the text column type to date type
df[,2] <- as.Date(as.character(df[,2]),"%Y%m%d")

head(df)

#Historical transactional data with 18 months timeframe.This dataset will act as Training data
startDate_history <- as.Date("19970101","%Y%m%d")
endDate_history <- as.Date("19980228","%Y%m%d")

getDataFrame <- function (df,startDate,endDate,tIDColName="ID",tDateColName="Date",tAmountColName="Amount"){
  
  #order the dataframe by date descendingly
  df <- df[order(df[,tDateColName],decreasing = TRUE),]
  
  #remove the record before the start data and after the end Date
  df <- df[df[,tDateColName]>= startDate,]
  df <- df[df[,tDateColName]<= endDate,]
  
  #remove the rows with the duplicated IDs, and assign the df to a new df.
  newdf <- df[!duplicated(df[,tIDColName]),]
  
  # calculate the Recency(days) to the endDate, the smaller days value means more recent
  Recency<-as.numeric(difftime(endDate,newdf[,tDateColName],units="days"))
  
  # add the Days column to the newdf data frame
  newdf <-cbind(newdf,Recency)
  
  #order the dataframe by ID to fit the return order of table() and tapply()
  newdf <- newdf[order(newdf[,tIDColName]),]
  
  # calculate the frequency
  fre <- as.data.frame(table(df[,tIDColName]))
  Frequency <- fre[,2]
  newdf <- cbind(newdf,Frequency)
  
  #calculate the Money per deal
  m <- as.data.frame(tapply(df[,tAmountColName],df[,tIDColName],sum))
  Monetary <- m[,1]/Frequency
  newdf <- cbind(newdf,Monetary)
  
  return(newdf)
  
} 

history <- getDataFrame(df,startDate_history,endDate_history)

#Forecast transactional data with 2 months time frame
startDate_forecast <- as.Date("19980301","%Y%m%d")
endDate_forecast <- as.Date("19980430","%Y%m%d")

#Historical dataset with distinct customers
forecast <- getDataFrame(df,startDate_forecast,endDate_forecast)

head(history)

#Setting purchasing cycle to specified forecast period
history$Recency<- history$Recency %/% 60 

#Categorize Monetary values into bins with size of $10 each
breaks<-seq(0,round(max(history$Monetary)+9),by=10)
history$Monetary<-as.numeric(cut(history$Monetary,breaks,labels=FALSE))

#Adding "Buy" or "No Buy" column to the training/historical dataset
Buy<-rep(0,nrow(history))
history<-cbind(history,Buy)

#Identifying customers who purchased during the forecast period
history[history$ID %in% forecast$ID, ]$Buy<-1

#Create training data set with specific conditions
train<-history
head(train)

require(plyr)
#Function calculating probabilities of "Buy" and "Repurchase" grouped by R,F,M values(individually or in combination)
getPercentages <- function(df,colNames){
  
  Var<-c(colNames,"Buy")
  
  df<-df[,names(df) %in% Var,drop=F]
  
  
  a <- ddply(df,Var,summarize,Number=length(Buy))
  b <- ddply(a,
             .(),
             .fun=function(x){
               transform(x, Percentage=with(x,round(ave(Number,a[,names(a) %in% Var,drop=F],FUN=sum)/ave(Number,a[,names(a) %in% colNames,drop=F],FUN=sum),2)))
             })
  
  b<-b[b$Buy==1,-1]
  
  return(b)
  
}

#Calculation for the "Buy" percentage based on Recency
colNames<-c("Recency")
p<-getPercentages(train,colNames)

# logistic regression on Purchase Percentage - Recency
r.glm=glm(Percentage~Recency,family=quasibinomial(link='logit'),data=p)
p_r<-p

#Calculation for the "Buy" percentage based on Frequency
colNames<-c("Frequency")
p<-getPercentages(train,colNames)

# logistic regression on Purchase Percentage - Frequency
f.glm=glm(Percentage~Frequency,family=quasibinomial(link='logit'),data=p)
p_f<-p

#Calculation for the "Buy" percentage based on Monetary
colNames<-c("Monetary")
p<-getPercentages(train,colNames)

# logistic regression on Purchase Percentage - Monetary
m.glm=glm(Percentage~Monetary,family=quasibinomial(link='logit'),data=p)
p_m<-p

#plot and draw fit curves of Percentage ~ r,f,m
par(mfrow=c(1,3),oma=c(0,0,2,0))

plot(p_r$Recency,p_r$Percentage*100,xlab="Recency",ylab="Probablity of Purchasing(%)")
lines(lowess(p_r$Recency,p_r$Percentage*100),col="blue",lty=2)

plot(p_f$Frequency,p_f$Percentage*100,xlab="Frequency",ylab="Probablity of Purchasing(%)")
lines(lowess(p_f$Frequency,p_f$Percentage*100),col="blue",lty=2)

plot(p_m$Monetary,p_m$Percentage*100,xlab="Monetary",ylab="Probablity of Purchasing(%)")
lines(lowess(p_m$Monetary,p_m$Percentage*100),col="blue",lty=2)

title("Percentages ~ Recency, Frequency, Monetary", y=10,outer=TRUE)

par(mfrow=c(1,1))

model<-glm(Buy~Recency+Frequency,family=quasibinomial(link='logit'),data=train)
pred<-predict(model,data.frame(Recency=c(0),Frequency=c(1)),type='response')

#Calculation for CLTV based on Recency and Frequency after n periods
getCLV<-function(r,f,m,n,cost,periods,dr,pModel){
  
  df<-data.frame(period=c(0),r=c(r),f=c(f),n=c(n),value=c(0))
  
  for(i in 1:periods){
    backstep<-df[df$period==2-1,]
    nrow<-nrow(backstep)
    for(j in 1:nrow){
      r<-backstep[j,]$r 
      f<-backstep[j,]$f 
      n<-backstep[j,]$n 
      p<-predict(pModel,data.frame(Recency=r,Frequency=f),type='response')[1]
      buyers<-n*p
      
      #Predict 'Buy' probability for this period
      df<-rbind(df,c(i,0,f+1,buyers,buyers*(m-cost) / (1+dr)^i)) 
      
      #Predict 'No-Buy' probability for this period
      df<-rbind(df,c(i,r+1,f,n-buyers,(n-buyers)*(-cost)  / (1+dr)^i ))
    }
  }
  
  return(sum(df$value))
  
}

## Calculating the CLV for a customer with R=0,F=1,average profit=100,discount rate=0.02 for 3 periods
v<-getCLV(0,1,100,1,0,4,0.02,model)
v
