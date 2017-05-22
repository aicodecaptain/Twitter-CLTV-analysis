
library(dplyr)
library(ggplot2)
library(readr)
library(DataExplorer)
library(survival)

df$Recency <- round(as.numeric(difftime(Sys.Date(),
                                                df[,1],units="days")) )

salesM <- aggregate(df[,3],list(df$Account),sum)
names(salesM) <- c("idCustomer","Monetary")

salesF <- aggregate(df[,3],list(df$Account),length)
names(salesF) <- c("idCustomer","Frequency")

salesR <- aggregate(df[,4],list(df$Account),min)
names(salesR) <- c("idCustomer","Recency")

temp <- merge(salesF,salesR,"idCustomer")
salesRFM <- merge(temp,salesM,"idCustomer")


salesRFM$rankR <- cut(salesRFM$Recency,5,labels=F)  
salesRFM$rankF <- cut(salesRFM$Frequency,5,labels=F)  
salesRFM$rankM <- cut(salesRFM$Monetary,5,labels=F)

salesRFM <- salesRFM[with(salesRFM, order(-rankR, -rankF, -rankM)), ]
head(salesRFM, n=10)

groupRFM <- count(salesRFM, rankR, rankF,rankM)
groupRFM <- salesRFM$rankR*100 + salesRFM$rankF*10 + 
  salesRFM$rankM
salesRFM <- cbind(salesRFM,groupRFM)


ggplot(salesRFM, aes(factor(groupRFM))) +
  geom_bar() +
  theme(axis.title = element_text(color="#666666", face="bold"))

# remove high contributing variables
 name_list <- c(4503599636020167,18462539,247046579,4503599629858023,4503599635418211,126208667,4503599628594039,4503599632615107,4503599646903651,89858807,4503599653493835,4503599633060559,4503599637715491,4503599633207583,215541671,4503599647939419,4503599633638539,4503599627512515)
 tempdf <- tempdf[ ! tempdf$Account %in% name_list, ]
 write.csv(tempdf,"tempdf.csv")

 
 dt = sort(sample(nrow(salesrfm), nrow(data)*.7))
 train<-data[dt,]
 test<-data[-dt,]
 
 smp_size <- floor(0.75 * nrow(salesrfm))
 
 ## set the seed to make your partition reproductible
 set.seed(123)
 train_ind <- sample(seq_len(nrow(salesrfm)), size = smp_size)
 
 train <- salesrfm[train_ind, ]
 test <- salesrfm[-train_ind, ]
 
 attach(train)
 ltv.surv <- survreg(Surv(Frequency, score)~ Recency,dist="w")
 ltv.predict <- predict (ltv.surv,  newdata=salesrfm)
 View(ltv.predict)
 
 