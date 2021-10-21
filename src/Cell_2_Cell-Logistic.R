rm(list = ls())
setwd("/Users/apple/Desktop/Datasets/Cell2cell")
getwd()
file <- read.csv("Proactive Attrition Management-Logistic Regression Case Study.csv",header = TRUE)
data <- file[,1:76] # dropping Calibrat and ChurnDep as asked 
dim(data)
summary(data)

#Variable Segregation in Numerical & Categorical and their relevance

split(names(data),sapply(data, function(x) paste(class(x), collapse=" ")))
cont_vars <- c("REVENUE" , "MOU" ,     "RECCHRGE" ,"DIRECTAS" ,"OVERAGE" , "ROAM"  ,   "CHANGEM" , "EQPDAYS",
               "CHANGER",  "DROPVCE","BLCKVCE" , "UNANSVCE" ,"CUSTCARE" ,"THREEWAY", "MOUREC"  ,
               "OUTCALLS" ,"INCALLS"  ,"PEAKVCE",  "OPEAKVCE","DROPBLK"  ,"CALLWAIT" ,"SETPRC" )

cat_vars <- c( "CHURN"  ,  "MONTHS" ,  "UNIQSUBS", "ACTVSUBS" ,"PHONES" ,  "MODELS"   , "CUSTOMER" ,"AGE1"  ,  
               "AGE2"  ,   "CHILDREN" ,"CREDITA"  ,"CREDITAA" ,"CREDITB"  ,"CREDITC" , "CREDITDE" ,"CREDITGY" ,"CREDITZ" ,
               "PRIZMRUR" ,"PRIZMUB", "PRIZMTWN", "REFURB" ,  "WEBCAP" ,  "TRUCK" ,   "RV" ,      "OCCPROF",  "OCCCLER" ,
               "OCCCRFT" , "OCCSTUD" , "OCCHMKR", "OCCRET" ,  "OCCSELF" , "OWNRENT", "MARRYUN", "MARRYYES" ,"MARRYNO" ,
               "MAILORD" , "MAILRES" , "MAILFLAG", "TRAVEL"  , "PCOWN"  ,  "CREDITCD" ,"RETCALLS", "RETACCPT","NEWCELLY",
               "NEWCELLN" ,"REFER" ,   "INCMISS"  ,"INCOME" ,  "MCYCLE" ,  "CREDITAD", "SETPRCM" , "RETCALL")


# Checking Numerical Univariate analysis

var_Summ=function(x){
            Var_Type=class(x)
            n<-length(x)
            nmiss<-sum(is.na(x))
            mean<-mean(x,na.rm=T)
            std<-sd(x,na.rm=T)
            var<-var(x,na.rm=T)
            min<-min(x,na.rm=T)
            p1<-quantile(x,0.01,na.rm=T)
            p5<-quantile(x,0.05,na.rm=T)
            p10<-quantile(x,0.1,na.rm=T)
            q1<-quantile(x,0.25,na.rm=T)
            q2<-quantile(x,0.5,na.rm=T)
            q3<-quantile(x,0.75,na.rm=T)
            p90<-quantile(x,0.9,na.rm=T)
            p95<-quantile(x,0.95,na.rm=T)
            p99<-quantile(x,0.99,na.rm=T)
            max<-max(x,na.rm=T)
            UC1=mean(x,na.rm=T)+3*sd(x,na.rm=T)
            LC1=mean(x,na.rm=T)-3*sd(x,na.rm=T)
            UC2=quantile(x,0.99,na.rm=T)
            LC2=quantile(x,0.01,na.rm=T)
            iqr=IQR(x,na.rm=T)
            UC3=q3+1.5*iqr
            LC3=q1-1.5*iqr
            outlier_flag <- max>1.5*(p99)
            return(c(Var_Type=Var_Type, n=n,nmiss=nmiss,outlier=outlier_flag,mean=mean,std=std,var=var,min=min,
                     p1=p1,p5=p5,p10=p10,q1=q1,q2=q2,q3=q3,p90=p90,p95=p95,p99=p99,
                     max=max,iqr=iqr))
      }
Numerica_diags<-t(data.frame(apply(data[cont_vars], 2, var_Summ)))
View(Numerica_diags)
write.csv(Numerica_diags,"Numerical_Diagonistics.csv")

#####---------------------Missing Value Treatment-----------############
apply(is.na(data[,]),2,sum)

#Missing Value Treatment
data[,cont_vars] <- apply(data.frame(data[,cont_vars]), 2, function(x){x <- replace(x, is.na(x), mean(x, na.rm=TRUE))})
data[,cat_vars] <- apply(data.frame(data[,cat_vars]), 2, function(x){x <- replace(x, is.na(x), which.max(prop.table(table(x))))})
apply(is.na(data[,]),2,sum)

###########-------------Outlier Treatments----------##################
data$REVENUE[data$REVENUE>quantile(data$REVENUE,0.99,na.rm = T)] <- quantile(data$REVENUE,0.99,na.rm = T)
data$MOU[data$MOU>quantile(data$MOU,0.99,na.rm = T)] <- quantile(data$MOU,0.99,na.rm = T)
data$RECCHRGE[data$RECCHRGE>quantile(data$RECCHRGE,0.99,na.rm = T)] <- quantile(data$RECCHRGE,0.99,na.rm = T)
data$DIRECTAS[data$DIRECTAS>quantile(data$DIRECTAS,0.99,na.rm = T)] <- quantile(data$DIRECTAS,0.99,na.rm = T)
data$OVERAGE[data$OVERAGE>quantile(data$OVERAGE,0.99,na.rm = T)] <- quantile(data$OVERAGE,0.99,na.rm = T)
data$ROAM[data$ROAM>quantile(data$ROAM,0.99,na.rm = T)] <- quantile(data$ROAM,0.99,na.rm = T)
data$CHANGEM[data$CHANGEM>quantile(data$CHANGEM,0.99,na.rm = T)] <- quantile(data$CHANGEM,0.99,na.rm = T)
data$CHANGER[data$CHANGER>quantile(data$CHANGER,0.99,na.rm = T)] <- quantile(data$CHANGER,0.99,na.rm = T)
data$DROPVCE[data$DROPVCE>quantile(data$DROPVCE,0.99,na.rm = T)] <- quantile(data$DROPVCE,0.99,na.rm = T)
data$BLCKVCE[data$BLCKVCE>quantile(data$BLCKVCE,0.99,na.rm = T)] <- quantile(data$BLCKVCE,0.99,na.rm = T)
data$UNANSVCE[data$UNANSVCE>quantile(data$UNANSVCE,0.99,na.rm = T)] <- quantile(data$UNANSVCE,0.99,na.rm = T)
data$CUSTCARE[data$CUSTCARE>quantile(data$CUSTCARE,0.99,na.rm = T)] <- quantile(data$CUSTCARE,0.99,na.rm = T)
data$THREEWAY[data$THREEWAY>quantile(data$THREEWAY,0.99,na.rm = T)] <- quantile(data$THREEWAY,0.99,na.rm = T)
data$MOUREC[data$MOUREC>quantile(data$MOUREC,0.99,na.rm = T)] <- quantile(data$MOUREC,0.99,na.rm = T)
data$OUTCALLS[data$OUTCALLS>quantile(data$OUTCALLS,0.99,na.rm = T)] <- quantile(data$OUTCALLS,0.99,na.rm = T)
data$INCALLS[data$INCALLS>quantile(data$INCALLS,0.99,na.rm = T)] <- quantile(data$INCALLS,0.99,na.rm = T)
data$PEAKVCE[data$PEAKVCE>quantile(data$PEAKVCE,0.99,na.rm = T)] <- quantile(data$PEAKVCE,0.99,na.rm = T)
data$OPEAKVCE[data$OPEAKVCE>quantile(data$OPEAKVCE,0.99,na.rm = T)] <- quantile(data$OPEAKVCE,0.99,na.rm = T)
data$DROPBLK[data$DROPBLK>quantile(data$DROPBLK,0.99,na.rm = T)] <- quantile(data$DROPBLK,0.99,na.rm = T)
data$SETPRC[data$SETPRC>quantile(data$SETPRC,0.99,na.rm = T)] <- quantile(data$SETPRC,0.99,na.rm = T)
data$CALLWAIT[data$CALLWAIT>quantile(data$CALLWAIT,0.99,na.rm = T)] <- quantile(data$CALLWAIT,0.99,na.rm = T)
data$EQPDAYS[data$EQPDAYS>quantile(data$EQPDAYS,0.99,na.rm = T)] <- quantile(data$EQPDAYS,0.99,na.rm = T)


##########-------- At this Stage our data is clean -------###############
cont_var <- data[,cont_vars]
cat_var <- data[,cat_vars]
dim(cont_var) #22 continuous variables
dim(cat_var)  #52 Categorical variables 
###########---------- Variable Reduction----------########
#  Chi sq test on the categorical variables Individually 
c <- table(data$RETCALL,data$CHURN)
chisq.test(c)


[1] ""    "MONTHS"   "UNIQSUBS" "ACTVSUBS" "PHONES"   "MODELS"   "" "AGE1"     "AGE2"    
[10] "" "CREDITA"  "CREDITAA" "CREDITB"  "CREDITC"  "CREDITDE" "" ""  "PRIZMRUR"
[19] "PRIZMUB"  "PRIZMTWN" "REFURB"   "WEBCAP"   ""    ""       ""  "OCCCLER"  "" 
[28] ""  ""  "OCCRET"   ""  "OWNRENT"  "MARRYUN"  "" "MARRYNO"  "MAILORD" 
[37] "MAILRES"  "" ""   ""    "CREDITCD" "RETCALLS" "RETACCPT" "NEWCELLY" ""
[46] "REFER"    "INCMISS"  "INCOME"   ""   "" "SETPRCM"  "RETCALL" 


# Insignificant/ Independent Variables ---TO BE REJECTED DUE TO HIGH P VALUES
# TRAVEL
# PCOWN
# NEWCELLN
# MCYCLE
# CREDITAD
# OCCSTUD
# OCCSELF
# OCCHMKR
# OCCCRFT
# OCCPROF
# MARRYYES
# MAILFLAG
# CUSTOMER
# CREDITGY
# CHILDREN
# CREDITZ
# TRUCK
# TRUCK


#Subsetting 
cat_var <- subset(cat_var,select = -c(TRAVEL,PCOWN,NEWCELLN,MCYCLE,CREDITAD,OCCSTUD,OCCSELF,OCCHMKR,
                                OCCCRFT,OCCPROF,MARRYYES,MAILFLAG,CUSTOMER,CREDITGY,CHILDREN,
                                CREDITZ,TRUCK))


# Co-relarion matrix - Continuous Variables
corrm <- cor(cont_var)
View(corrm)
write.csv(corrm,"correlation_matrix.csv")


# Factor Analyssis
require(dplyr)
require(GPArotation)
require(psych)
eigen_values <- mutate(data.frame(eigen(corrm)$values)
                       ,cum_sum_eigen=cumsum(eigen.corrm..values)
                       , pct_var=eigen.corrm..values/sum(eigen.corrm..values)
                       , cum_pct_var=cum_sum_eigen/sum(eigen.corrm..values))  # CALCULATING VARIANCE, CUMULATIVE VARIANCE etc... 

View(eigen_values)
write.csv(eigen_values,"eigen_values.csv")
scree(corrm, factors=T, pc=T, main="scree plot", hline=NULL, add=FALSE) 
as.matrix(eigen_values)   
# Factor Size 15 we are getting eigen value of 0.99564 and variance of 74% . So making factor analysis on 15 factors first 
# and same is observed in the scree plot 

FA<-fa(r=corrm, 6, rotate="varimax", fm="ml")               ### CONDUCTING FACTOR ANALYSIS
FA                                                ### PRINT THE RESULTS
FA_SORT<-fa.sort(FA)                                         ### SORTING THE LOADINGS
ls(FA_SORT)                                                  ### LISTING OUT THE OBJECTS
FA_SORT$loadings
#FA_SORT$e.values                                            ### FINDING EIGEN VALUES FROM THE RESULTS
Loadings<-data.frame(FA_SORT$loadings[1:ncol(cont_var),]) ### CAPTURING ONLY LOADINGS INTO DATA FRAME

write.csv(Loadings,"Factor_Analysis.csv")

# Variables to drop after factor analysis are 
# MOU DIRECTAS ROAM DROPBLK CHANGER REVENUE

cont_var <- subset(cont_var,select = -c(MOU ,DIRECTAS, ROAM, DROPBLK, CHANGER))

dim(cont_var)# we are left with 17 continuous variables 
dim(cat_var) # 35 categorical variables 

# Further Refining to be  done in the model building 
mydata <- cbind(cont_var,cat_var)
#Splitting data into Training, Validaton and Testing Dataset
train_ind <- sample(1:nrow(mydata), size = floor(0.70 * nrow(mydata)))

training<-mydata[train_ind,]
testing<-mydata[-train_ind,]
names(training)
#Building Models for training dataset

fit<-glm(CHURN~REVENUE+RECCHRGE+OVERAGE+CHANGEM+EQPDAYS+DROPVCE+BLCKVCE+UNANSVCE+CUSTCARE+
               THREEWAY+MOUREC+OUTCALLS+INCALLS+PEAKVCE+OPEAKVCE+CALLWAIT+SETPRC+
               MONTHS+UNIQSUBS+ACTVSUBS+PHONES+MODELS+AGE1+AGE2+CREDITA+CREDITAA+
               CREDITB+CREDITC+CREDITDE+PRIZMRUR+PRIZMUB+PRIZMTWN+REFURB+WEBCAP+RV+
               OCCCLER+OCCRET+OWNRENT+MARRYUN+MARRYNO+MAILORD+MAILRES+CREDITCD+RETCALLS+
               RETACCPT+NEWCELLY+REFER+INCMISS+INCOME+SETPRCM+RETCALL,data = training,
         family = binomial(logit))

summary(fit)
require(MASS)
# step <- stepAIC(fit,direction = "both")

fit1 <- glm(CHURN ~  RECCHRGE + OVERAGE + CHANGEM + EQPDAYS + DROPVCE + 
                  BLCKVCE + CUSTCARE + THREEWAY + MOUREC + PEAKVCE + SETPRC + 
                  MONTHS + UNIQSUBS + ACTVSUBS + PHONES + AGE1 + CREDITAA + 
                  CREDITB + CREDITDE + PRIZMRUR + PRIZMUB + REFURB + WEBCAP + 
                  OCCCLER + MARRYNO + MAILRES + RETACCPT + REFER + INCMISS + 
                  SETPRCM + RETCALL,
            data=training,family = binomial(logit))

fit2 <- glm(CHURN ~  RECCHRGE + OVERAGE + CHANGEM + EQPDAYS + DROPVCE + 
                  BLCKVCE + CUSTCARE + THREEWAY + SETPRC + 
                  MONTHS + UNIQSUBS + ACTVSUBS + PHONES + AGE1 + CREDITAA + 
                  CREDITB + CREDITDE + PRIZMRUR + PRIZMUB + REFURB + WEBCAP + 
                  OCCCLER + MARRYNO + MAILRES + RETACCPT + REFER + INCMISS + 
                  SETPRCM + RETCALL,
            data=training,family = binomial(logit))

summary(fit1)
# We see all the variables in my model are considerably significant 
library("car")
as.matrix(vif(fit1))
# VIF of some values are considerably too high 
# Dropping Revenue coz VIF is 9(too high)
Concordance(fit1)

###############--VALIDATION--##############
#Decile Scoring for 
##Training dataset
train1<- cbind(training, Prob=predict(fit1, type="response")) 
View(train1)

##Creating Deciles
decLocations <- quantile(train1$Prob, probs = seq(0.1,0.9,by=0.1))
train1$decile <- findInterval(train1$Prob,c(-Inf,decLocations, Inf))
View(train1)

#Decile Analysis Reports
require(sqldf)
fit_train_DA <- sqldf("select decile, count(decile) as count,min(Prob) as Min_prob
                      , max(Prob) as max_prob
                      , sum(CHURN) as CHURN_Count
                      , (count(decile)-sum(CHURN)) as Non_CHURN_Count 
                      from train1
                      group by decile
                      order by decile desc")
View(fit_train_DA)
write.csv(fit_train_DA,"fit_train_DA1.csv",row.names = F)



##Creating Deciles  Testing
test1<- cbind(testing, Prob=predict(fit1, testing,type="response")) 
View(train1)

decLocations <- quantile(test1$Prob, probs = seq(0.1,0.9,by=0.1))
test1$decile <- findInterval(test1$Prob,c(-Inf,decLocations, Inf))
names(test1)
#Decile Analysis Reports
require(sqldf)

fit_test_DA <- sqldf("select decile, count(decile) as count, min(Prob) as Min_prob
                     , max(Prob) as max_prob 
                     , sum(CHURN) as churn_cnt
                        , (count(decile)-sum(CHURN)) as Non_CHURN_Count 
                     from test1
                     group by decile
                     order by decile desc")
View(fit_test_DA)
write.csv(fit_test_DA,"fit_test_DA1.csv",row.names = F)

#######----------Performanc eof the Model-------########
train1<- cbind(training, Prob=predict(fit1, type="response")) 
View(train1)
require(ROCR)
pred_train_fit2 <- prediction(train1$Prob, train1$CHURN)
perf_fit2 <- performance(pred_train_fit2, "tpr", "fpr")
plot(perf_fit2)
abline(0, 1)
performance(pred_train_fit2, "auc")@y.values


#####------ Confusion MAtrix-------#######

test<-cbind(testing, Prob=predict(fit1, testing, type="response"))
View(test)
test$Pred_Churn <- ifelse(test$Prob>0.28, 1,0)
sum(test$Pred_Churn)

# Confusion matrix
train1$Pred_Churn <- ifelse(train1$Prob>0.28, 1,0)
test1$Pred_Churn <- ifelse(test1$Prob>0.28, 1,0)

table( train1$CHURN,train1$Pred_Churn)
table( test1$CHURN,test1$Pred_Churn)

############______ END OF FILE____________############










