getwd()
rm(list=ls())
setwd("/Users/apple/Desktop")
file_a <- read.csv(file.choose(),header = TRUE)
file <- file_a[,-1]
summary(file)

#Missing Values Deletion
file$MINIMUM_PAYMENTS[is.na(file$MINIMUM_PAYMENTS)] <- 0
file$CREDIT_LIMIT[is.na(file$CREDIT_LIMIT)] <- 0
summary(file)


corrm <- cor(file)

write.csv(corrm,"/Users/apple/Desktop/Datasets/Credit_card/corrm.csv")

#Data Preparation Outlier Treatment 
mystats <- function(x) {
      nmiss<-sum(is.na(x))
      a <- x[!is.na(x)]
      med <- median(a)
      m <- mean(a)
      n <- length(a)
      s <- sd(a)
      min <- min(a)
      p1<-quantile(a,0.01)
      p5<-quantile(a,0.05)
      p10<-quantile(a,0.10)
      q1<-quantile(a,0.25)
      q2<-quantile(a,0.5)
      q3<-quantile(a,0.75)
      p90<-quantile(a,0.90)
      p95<-quantile(a,0.95)
      p99<-quantile(a,0.99)
      max <- max(a)
      UC <- m+3*s
      LC <- m-3*s
      outlier_flag<- max>UC | min<LC
      return(c(n=n, nmiss=nmiss, outlier_flag=outlier_flag, median=med,mean=m, stdev=s,min = min, p1=p1,p5=p5,p10=p10,q1=q1,q2=q2,q3=q3,p90=p90,p95=p95,p99=p99,max=max, UC=UC, LC=LC ))
}

diag_stats<-t(data.frame(apply(file, 2, mystats)))
View(diag_stats)
write.csv(diag_stats, "diag_stats.csv")
names(file)

file$BALANCE[file$BALANCE>quantile(file$BALANCE,0.95,na.rm = T)] <- quantile(file$BALANCE,0.95,na.rm = T)
file$BALANCE_FREQUENCY[file$BALANCE>quantile(file$BALANCE_FREQUENCY,0.95,na.rm = T)] <- quantile(file$BALANCE_FREQUENCY,0.95,na.rm = T)
file$PURCHASES[file$PURCHASES>quantile(file$PURCHASES,0.95,na.rm = T)] <- quantile(file$PURCHASES,0.95,na.rm = T)
file$ONEOFF_PURCHASES[file$ONEOFF_PURCHASES>quantile(file$ONEOFF_PURCHASES,0.95,na.rm = T)] <- quantile(file$ONEOFF_PURCHASES,0.95,na.rm = T)
file$INSTALLMENTS_PURCHASES[file$INSTALLMENTS_PURCHASES>quantile(file$INSTALLMENTS_PURCHASES,0.95,na.rm = T)] <- quantile(file$INSTALLMENTS_PURCHASES,0.95,na.rm = T)
file$CASH_ADVANCE[file$CASH_ADVANCE>quantile(file$CASH_ADVANCE,0.95,na.rm = T)] <- quantile(file$CASH_ADVANCE,0.95,na.rm = T)
file$PURCHASES_FREQUENCY[file$PURCHASES_FREQUENCY>quantile(file$PURCHASES_FREQUENCY,0.95,na.rm = T)] <- quantile(file$PURCHASES_FREQUENCY,0.95,na.rm = T)
file$ONEOFF_PURCHASES_FREQUENCY[file$ONEOFF_PURCHASES_FREQUENCY>quantile(file$ONEOFF_PURCHASES_FREQUENCY,0.95,na.rm = T)] <- quantile(file$ONEOFF_PURCHASES_FREQUENCY,0.95,na.rm = T)
file$PURCHASES_INSTALLMENTS_FREQUENCY[file$PURCHASES_INSTALLMENTS_FREQUENCY>quantile(file$PURCHASES_INSTALLMENTS_FREQUENCY,0.95,na.rm = T)] <- quantile(file$PURCHASES_INSTALLMENTS_FREQUENCY,0.95,na.rm = T)
file$CASH_ADVANCE_TRX[file$CASH_ADVANCE_TRX>quantile(file$CASH_ADVANCE_TRX,0.95,na.rm = T)] <- quantile(file$CASH_ADVANCE_TRX,0.95,na.rm = T)
file$PURCHASES_TRX[file$PURCHASES_TRX>quantile(file$PURCHASES_TRX,0.95,na.rm = T)] <- quantile(file$PURCHASES_TRX,0.95,na.rm = T)
file$CREDIT_LIMIT[file$CREDIT_LIMIT>quantile(file$CREDIT_LIMIT,0.95,na.rm = T)] <- quantile(file$CREDIT_LIMIT,0.95,na.rm = T)
file$PAYMENTS[file$PAYMENTS>quantile(file$PAYMENTS,0.95,na.rm = T)] <- quantile(file$PAYMENTS,0.95,na.rm = T)
file$MINIMUM_PAYMENTS[file$MINIMUM_PAYMENTS>quantile(file$MINIMUM_PAYMENTS,0.95,na.rm = T)] <- quantile(file$MINIMUM_PAYMENTS,0.95,na.rm = T)
file$PRC_FULL_PAYMENT[file$PRC_FULL_PAYMENT>quantile(file$PRC_FULL_PAYMENT,0.95,na.rm = T)] <- quantile(file$PRC_FULL_PAYMENT,0.95,na.rm = T)


#Factor Analysis
require(psych)
require(GPArotation)
scree(corrm, factors=T, pc=T, main="scree plot", hline=NULL, add=FALSE) ### SCREE PLOT
data.frame(eigen(corrm)$values )  
require(dplyr)
eigen_values <- mutate(data.frame(eigen(corrm)$values)
                       ,cum_sum_eigen=cumsum(eigen.corrm..values)
                       , pct_var=eigen.corrm..values/sum(eigen.corrm..values)
                       , cum_pct_var=cum_sum_eigen/sum(eigen.corrm..values))  # CALCULATING VARIANCE, CUMULATIVE VARIANCE etc... 
summary(file)
FA<-fa(r=corrm, 6, rotate="varimax", fm="ml")               ### CONDUCTING FACTOR ANALYSIS
print(FA)                                                    ### PRINT THE RESULTS
FA_SORT<-fa.sort(FA)                                         ### SORTING THE LOADINGS
ls(FA_SORT)                                                  ### LISTING OUT THE OBJECTS
FA_SORT$loadings
#FA_SORT$e.values                                            ### FINDING EIGEN VALUES FROM THE RESULTS
Loadings<-data.frame(FA_SORT$loadings[1:ncol(file),]) ### CAPTURING ONLY LOADINGS INTO DATA FRAME
write.csv(Loadings, "/Users/apple/Desktop/Datasets/Credit_card/loadings1.csv") ### SAVING THE FILE


file <- subset(file,select = -c(PURCHASES,CASH_ADVANCE,CASH_ADVANCE_FREQUENCY,PURCHASES_FREQUENCY))
inputdata_final <-file
names(inputdata_final)
#Prepare final Data
#standardizing the data
inputdata_final = scale(inputdata_final)
View(inputdata_final)
#View(inputdata_final)
#building clusters using k-means clustering 
cluster_three <- kmeans(inputdata_final,3)
cluster_four <- kmeans(inputdata_final,4)
cluster_five <- kmeans(inputdata_final,5)
cluster_six <- kmeans(inputdata_final,6)

file_new<-cbind(file,km_clust_3=cluster_three$cluster,km_clust_4=cluster_four$cluster,
                 km_clust_5=cluster_five$cluster ,km_clust_6=cluster_six$cluster   )
View(file_new)


###Profiling

require(tables)
tt<-cbind(tt<-cbind(tabular(1+factor(km_clust_3)+factor(km_clust_4)+factor(km_clust_5)+factor(km_clust_6)
                            ~ Heading()*length*All(file[1]), data=file_new)),
          tabular( 1+factor(km_clust_3)+factor(km_clust_4)+factor(km_clust_5)+factor(km_clust_6)
                   ~ Heading()*mean*All(file), data=file_new))
tt1<-as.data.frame.matrix(tt)
#View(tt1)
names(tt1)

rownames(tt1)<-c("Overall", "KM3_1" ,"KM3_2" ,"KM3_3", "KM4_1" ,"KM4_2" ,"KM4_3", "KM4_4" ,"KM5_1" ,"KM5_2", "KM5_3" ,"KM5_4" ,"KM5_5", "KM6_1" ,"KM6_2" ,"KM6_3", "KM6_4" ,"KM6_5" ,"KM6_6")
colnames(tt1)<-c("Count","BALANCE","BALANCE_FREQUENCY","ONEOFF_PURCHASES","INSTALLMENTS_PURCHASES",
                 "ONEOFF_PURCHASES_FREQUENCY","PURCHASES_INSTALLMENTS_FREQUENCY","CASH_ADVANCE_TRX","PURCHASES_TRX",
                 "CREDIT_LIMIT","PAYMENTS","MINIMUM_PAYMENTS","PRC_FULL_PAYMENT","TENURE")
cluster_profiling<-t(tt1)

write.csv(cluster_profiling, "/Users/apple/Desktop/Datasets/Credit_card/cluster_profiling_withoutoutliers.csv") 











