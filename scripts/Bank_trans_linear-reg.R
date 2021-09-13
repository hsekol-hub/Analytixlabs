rm(list=ls())
setwd("/Users/apple/Desktop/Datasets/Bank Transaction")
getwd()
file <- read.csv("file_changed.csv",header = TRUE)
data <- file # Creating a Copy of the original file
names(file)
dim(data)
data <- data[,-c(1:2)] # Removing the X and CustomerId column from the temporary dataset
names(data)

# Creating new Response variables 
data$total_spent <- data$cardspent+data$card2spent
# Removing not required variables 
data <- subset(data,select = -c(cardspent,card2spent))
names(data)
dim(data)

# After careful observation the missing values are represented in the dataset by "#NULL!" 
# So i decide to remove such variables 
# Replacing all "#NULL!" with NA's for easy computation 
data[data=="#NULL!"] <- 'NA'
summary(data)

#Spliting out numerical an d categorical variables 
var_num <- sapply(data,is.numeric)
temp_num_data <- data[,var_num]
temp_cat_data <- data[,!var_num]
names(data)
# Some variables are having wrong datatypes so manually checking and separating Nominal and Continuous data 
summary(temp_num_data)
summary(temp_cat_data)
#Categorical in num commutebike commutenonmotor 
#Numerical in cat lncardten lnwiremon lnwireten cardten lncardmon lnequipten lnequipmon lntollten 
#lntollmon lnlongten longten commutetime lnothdebt lncreddebt
data$lncardten <- as.numeric(data$lncardten)
data$lnwiremon <- as.numeric(data$lnwiremon)
data$cardten <- as.numeric(data$cardten)
data$lnwireten <- as.numeric(data$lnwireten)
data$lncardmon <- as.numeric(data$lncardmon)
data$lnequipten <- as.numeric(data$lnequipten)
data$lnequipmon <- as.numeric(data$lnequipmon)
data$lntollten <- as.numeric(data$lntollten)
data$lntollmon <- as.numeric(data$lntollmon)
data$longten <- as.numeric(data$longten)
data$lnlongten <- as.numeric(data$lnlongten)
data$commutetime <- as.numeric(data$commutetime)
data$lnothdebt <- as.numeric(data$lnothdebt)
data$lncreddebt <- as.numeric(data$lncreddebt)
data$commutebike <- as.character(data$commutebike)
data$commutenonmotor <- as.character(data$commutenonmotor)
data$tollfree <- as.character(data$tollfree)
data$multline <- as.character(data$multline)
#Spliting out numerical an d categorical variables 
var_num <- sapply(data,is.numeric)
temp_num_data <- data[,var_num]
temp_cat_data <- data[,!var_num]
summary(temp_num_data)
summary(temp_cat_data)
dim(temp_num_data)  # We have 52 Continuous variables 
dim(temp_cat_data)  # We have 78 Discrete variables 

# Performing a Diag test on the complete dataset where we calculate the following variables as 
# listed out in the data frame of the excel sheet 
mystats <- function(x) {
      nmiss<-sum(is.na(x))
      c <- class(x)
      a <- x[!is.na(x)]
      m <- mean(a,na.rm = T)
      med=median(a,na.rm = T)
      n <- length(a)
      s <- sd(a,na.rm = T)
      min <- min(a,na.rm = T)
      q1<-quantile(a,0.25,na.rm = T)
      q2<-quantile(a,0.5,na.rm = T)
      q3<-quantile(a,0.75,na.rm = T)
      p99<-quantile(a,0.99,na.rm = T)
      max <- max(a,na.rm = T)
      UC <- m+3*s
      LC <- m-3*s
      outlier_flag<- max>1.5*(p99)
      return(c(class=c,n=n, nmiss=nmiss, outlier_flag=outlier_flag, mean=m,median=med, stdev=s,min = min,
               q1=q1,q2=q2,q3=q3,p99=p99,max=max, UC=UC, LC=LC ))
}
diag_stats <- t(data.frame(apply(temp_num_data,2,mystats))) 
write.csv(diag_stats,"Numerical Diagonstics Test.csv")
View(diag_stats)

# Removing the Numerical variables which tend to have higher percentage of missing values
temp_num_data <- subset(temp_num_data,select = -c(lntollmon,lntollten,lnequipmon,lnequipten,
                                                  lncardmon,lncardten,lnwiremon,lnwireten))

data <- subset(data,select = -c(lntollmon,lntollten,lnequipmon,lnequipten,lncardmon,lncardten,
                                lnwiremon,lnwireten))
summary(temp_num_data)
##--- Mean value Imputation----####
temp_num_data$cardten[is.na(temp_num_data$cardten)] <- 267.3
temp_num_data$lnlongten[is.na(temp_num_data$lnlongten)] <- 436.3
temp_num_data$longten[is.na(temp_num_data$longten)] <- 2211
temp_num_data$commutetime[is.na(temp_num_data$commutetime)] <- 17.4
temp_num_data$lncreddebt[is.na(temp_num_data$lncreddebt)] <- 270.5
temp_num_data$lnothdebt[is.na(temp_num_data$lnothdebt)] <- 285.7
summary(temp_num_data)
diag_stats <- t(data.frame(apply(temp_num_data,2,mystats)))
View(diag_stats)

#################---------------------- Missing Numerical Treated-----------##################

#################---------------------- Missing categoriacl Treatment-----------##################
mystats1 <- function(x)
{
      nmiss=sum(is.na(x))
      class=class(x)
      return(c(class=class,no_miss=nmiss))
}
diag_stats1 <- t(data.frame(apply(temp_cat_data,2,mystats1)))
write.csv(diag_stats1,"Categorical Diagonstics Test.csv")
View(diag_stats1)

#Looking for the missing values in the categorical variables we obtain the following results and so we decide to remove them 
summary(temp_cat_data)
temp_cat_data <- apply((temp_cat_data), 2, function(x){x <- replace(x, is.na(x), which.max(prop.table(table(x))))})
temp_diag_stats <- t(data.frame(apply(temp_cat_data,2,mystats1)))
View(temp_diag_stats)



###-------------------------Numerical Outlier Treatmeny---------------------------------###########
diag_stats <- as.data.frame(diag_stats)
vars_with_outliers <- names(temp_num_data[diag_stats$outlier_flag==T])
vars_with_outliers # These are my variables with outlier_flag == TRUE so they need to be treated first 
summary(temp_num_data[vars_with_outliers])

temp_num_data$income[temp_num_data$income>quantile(temp_num_data$income,0.99,na.rm = T)] <- quantile(temp_num_data$income,0.99,na.rm = T)
temp_num_data$creddebt[temp_num_data$creddebt>quantile(temp_num_data$creddebt,0.99,na.rm = T)] <- quantile(temp_num_data$creddebt,0.99,na.rm = T)
temp_num_data$othdebt[temp_num_data$othdebt>quantile(temp_num_data$othdebt,0.99,na.rm = T)] <- quantile(temp_num_data$othdebt,0.99,na.rm = T)
temp_num_data$longmon[temp_num_data$longmon>quantile(temp_num_data$longmon,0.99,na.rm = T)] <- quantile(temp_num_data$longmon,0.99,na.rm = T)
temp_num_data$tollmon[temp_num_data$tollmon>quantile(temp_num_data$tollmon,0.99,na.rm = T)] <- quantile(temp_num_data$tollmon,0.99,na.rm = T)
temp_num_data$tollten[temp_num_data$tollten>quantile(temp_num_data$tollten,0.99,na.rm = T)] <- quantile(temp_num_data$tollten,0.99,na.rm = T)
temp_num_data$equipten[temp_num_data$equipten>quantile(temp_num_data$equipten,0.99,na.rm = T)] <- quantile(temp_num_data$equipten,0.99,na.rm = T)
temp_num_data$equipmon[temp_num_data$equipmon>quantile(temp_num_data$equipmon,0.99,na.rm = T)] <- quantile(temp_num_data$equipmon,0.99,na.rm = T)
temp_num_data$cardmon[temp_num_data$cardmon>quantile(temp_num_data$cardmon,0.99,na.rm = T)] <- quantile(temp_num_data$cardmon,0.99,na.rm = T)
temp_num_data$wiremon[temp_num_data$wiremon>quantile(temp_num_data$wiremon,0.99,na.rm = T)] <- quantile(temp_num_data$wiremon,0.99,na.rm = T)
temp_num_data$wireten[temp_num_data$wireten>quantile(temp_num_data$wireten,0.99,na.rm = T)] <- quantile(temp_num_data$wireten,0.99,na.rm = T)
temp_num_data$total_spent[temp_num_data$total_spent>quantile(temp_num_data$total_spent,0.99,na.rm = T)] <- quantile(temp_num_data$total_spent,0.99,na.rm = T)

diag_stats <- t(data.frame(apply(temp_num_data,2,mystats)))
View(diag_stats) # And we see that all the outliers have been capped to 99th percentile value

#############------ OUTLIERS & MISSING VALUES ARE TREATED NOW ----------------------##########
ncol(temp_num_data) #44 Continuous values
ncol(temp_cat_data) #78 Nominal/Categorical Values
###########----------------  AT THIS STAGE ALL OUR DATA IS CLEAN---------------#####################




# Co-relarion matrix - Continuous Variables
corrm <- cor(temp_num_data)
write.csv(corrm,"correlation_matrix.csv")
ncol(data)

# Factor Analyssis
require(dplyr)
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
require(GPArotation)
require(psych)
FA<-fa(r=corrm, 15, rotate="varimax", fm="ml")               ### CONDUCTING FACTOR ANALYSIS
FA                                                ### PRINT THE RESULTS
FA_SORT<-fa.sort(FA)                                         ### SORTING THE LOADINGS
ls(FA_SORT)                                                  ### LISTING OUT THE OBJECTS
FA_SORT$loadings
#FA_SORT$e.values                                            ### FINDING EIGEN VALUES FROM THE RESULTS
Loadings<-data.frame(FA_SORT$loadings[1:ncol(temp_num_data),]) ### CAPTURING ONLY LOADINGS INTO DATA FRAME

write.csv(Loadings,"Factor_Analysis.csv")

# Variables to drop after FACTOR LOADINGS are spoused longten wireten pets commutetime equipten lnothdebt
# lncreddebt lninc cardmon longmon cardtenurecat cardtenure

temp_num_data <- subset(temp_num_data,select = -c(spoused ,longten ,wireten ,pets, commutetime,
                                                  equipten ,lnothdebt, lncreddebt ,lninc ,cardmon ,
                                                  longmon, cardtenurecat ,cardtenure))
file <- cbind(temp_num_data,temp_cat_data)
dim(file)  # Left with 109 Variables
hist(file$total_spent)
summary(file$total_spent)
file$total_spent <- log(file$total_spent)
hist(log(file$total_spent))  # Now it is normally distributed
file$total_spent

fit <- aov(total_spent~.,data = file)  # Correlation of categorical variables
summary(fit)
View(fit)

# fit1 <- lm(total_spent~age+	ed+	employ+	income+	debtinc+	creddebt+	othdebt+	
#                  reside+	pets_cats+	pets_dogs+	pets_birds+	pets_reptiles+	pets_small+
#                  pets_saltfish+	pets_freshfish+	address+	cars+	carvalue+	card2tenure+
#                  carditems+	card2items+	tenure+	lnlongmon+	lnlongten+	tollmon+	tollten+
#                  equipmon+	cardten+	wiremon+	hourstv+	region+	townsize+	gender+
#                  agecat+	birthmonth+	edcat+	jobcat+	union+	empcat+	retire+
#                  inccat+	default+	jobsat+	marital+	spousedcat+	homeown+	hometype+
#                  addresscat+	carown+	cartype+	carcatvalue+	carbought+	carbuy+
#                  commute+	commutecar+	commutemotorcycle+	commutecarpool+	commutebus+	
#                  commuterail+	commutepublic+	commutebike+	commutewalk+	commutenonmotor+
#                  telecommute+	reason+	polview+	polparty+	polcontrib+	vote+	card+	cardtype+
#                  cardbenefit+	cardfee+	card2+	card2type+	card2benefit+	card2fee+
#                  card2tenurecat+	active+	bfast+	churn+	tollfree+	equip+
#                  callcard+	wireless+	multline+	voice+	pager+	internet+	callid+
#                  callwait+	forward+	confer+	ebill+	owntv+	ownvcr+	owndvd+
#                  owncd+	ownpda+	ownpc+	ownipod+	owngame+	ownfax+	news+
#                  response_01+	response_02+	response_03,data=file)
summary(fit1)
require(MASS)
step<- stepAIC(fit,direction="both") # performing Stepwise regression method on the above variables
# to perform variables reduction 
file$jobcat <- (as.numeric(file$jobcat))
file$carown <- (as.numeric(file$carown))
file$reason <- (as.numeric(file$reason))
file$card <- (as.numeric(file$card))
file$card2 <- (as.numeric(file$card2))



# ==================Model Fitting -----------------------------------------------------------_#

set.seed(123)
#Splitting data into Training, Validaton and Testing Dataset
train_ind <- sample(1:nrow(file), size = floor(0.70 * nrow(file)))

training<-file[train_ind,]
testing<-file[-train_ind,]

fit2 <- lm(total_spent ~ age + employ + income + pets_reptiles + carditems + 
                 card2items + gender  + jobcat + retire + 
                 carown + commuterail  + reason + card + 
                 card2 + churn + ownvcr + owncd,data=training)   # 18 variables have been selected as til now 

step<- stepAIC(fit2,direction="both")

library(car)
require(MASS)

ls(step)
summary(fit2) # F Value R2 and adj R2 p-value 
as.matrix(vif(fit2))

####____________________Creating Standardized Estimate/Beta Values____________##########
require("QuantPsyc")
lm.beta
function (MOD) 
{
      b <- summary(MOD)$coef[-1, 1]
      sx <- sd(MOD$model[-1])
      sy <- sd(MOD$model[1])
      beta <- b * sx/sy
      return(beta)
}

a <- lm.beta(fit2)
as.matrix(a)  # Standardized Beta Values 
# diagnostic plots 
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(fit2)


#--------------------------------------------------------------------------------------------------

#######################SCORING USING PREDICT FUNCTION
t1<-cbind(training, pred_spent = exp(predict(fit2)))
names(t1)
View(t1)
dim(t1)

t2<-cbind(testing, pred_spent=exp(predict(fit2,testing)))
View(t2)
dim(t2)

##################################Decile Analysis Reports - t1(training)

# find the decile locations 
decLocations <- quantile(t1$pred_spent, probs = seq(0.1,0.9,by=0.1))

# use findInterval with -Inf and Inf as upper and lower bounds
t1$decile <- findInterval(t1$pred_spent,c(-Inf,decLocations, Inf))

require(sqldf)
t1_DA <- sqldf("select decile, count(decile) as count, avg(pred_spent) as avg_pred_spent,   
               avg(exp(total_spent)) as avg_total_spent,
               avg(log(pred_spent)) as ln_avg_pred_spent,   
               avg(total_spent) as ln_avg_total_spent
               from t1
               group by decile
               order by decile desc")

View(t1_DA)
write.csv(t1_DA,"mydata1_DA.csv")

##################################Decile Analysis Reports - t2(testing)
t2$decile <- findInterval(t2$pred_spent,c(-Inf,decLocations, Inf))

require(sqldf)
t2_DA <- sqldf("select decile, count(decile) as count, avg(pred_spent) as avg_pred_spent,   
               avg(exp(total_spent)) as avg_total_spent,
               avg(log(pred_spent)) as ln_avg_pred_spent,   
               avg(total_spent) as ln_avg_total_spent
               from t2
               group by decile
               order by decile desc")

View(t2_DA)
write.csv(t2_DA,"mydata2_DA.csv")

#############-----------------End of File-------------###################




