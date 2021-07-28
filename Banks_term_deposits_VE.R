#  title: 'Bank Data - Fixed Deposits '
#  author: "Venkata Earanti"
#  date: "7/27/2021"

# 1. INTRODUCTION

#As part of HarvardX Data Science Capstone Project-Choose Your Own project, I selected the analysis of Bank Marketing data (http://archive.ics.uci.edu/ml/datasets/Bank+Marketing).The goal of this analysis is to make a binary classification model that predicts if a client will subscribe to a term deposit. There are 41188 rows and   21 variables in the data. The variables are related to the age, job, marital status, education, etc. and some are related to the economy like employment, consumer confidence etc. The data has been split into training and test set.  The model will be built on the training data and the testing will be done on the test data. 



# 2. DATA ANALYSIS

#Given below is the description of the data. 

##    1.Input variables:

#1 - age (numeric)

#2 - job : type of job (categorical: "admin.","blue-collar","entrepreneur","housemaid","management","retired","self-employed","services","student","technician","unemployed","unknown")

#3 - marital : marital status (categorical: "divorced","married","single","unknown"; note: "divorced" means divorced or widowed)

#4 - education (categorical: "basic.4y","basic.6y","basic.9y","high.school","illiterate","professional.course","university.degree","unknown")

#5 - default: has credit in default? (categorical: "no","yes","unknown")

#6 - housing: has housing loan? (categorical: "no","yes","unknown")

#7 - loan: has personal loan? (categorical: "no","yes","unknown")# related with the last contact of the current campaign:

#8 - contact: contact communication type (categorical: "cellular","telephone") 

#9 - month: last contact month of year (categorical: "jan", "feb", "mar", ..., "nov", "dec")

#10 - day_of_week: last contact day of the week (categorical: "mon","tue","wed","thu","fri")

#11 - duration: last contact duration, in seconds (numeric). Important note:  this attribute highly affects the output target (e.g., if duration=0 then y="no"). Yet, the duration is not known before a call is performed. Also, after the end of the call y is obviously known. Thus, this input should only be included for benchmark purposes and should be discarded if the intention is to have a realistic predictive model.

#12 - campaign: number of contacts performed during this campaign and for this client (numeric, includes last contact)

#13 - pdays: number of days that passed by after the client was last contacted from a previous campaign (numeric; 999 means client was not previously contacted)

#14 - previous: number of contacts performed before this campaign and for this client (numeric)

#15 - poutcome: outcome of the previous marketing campaign (categorical: "failure","nonexistent","success")
# social and economic context attributes

#16 - emp.var.rate: employment variation rate - quarterly indicator (numeric)

#17 - cons.price.idx: consumer price index - monthly indicator (numeric)     

#18 - cons.conf.idx: consumer confidence index - monthly indicator (numeric)     

#19 - euribor3m: euribor 3 month rate - daily indicator (numeric)

#20 - nr.employed: number of employees - quarterly indicator (numeric)

#Output variable (desired target):
  #  21 - y - has the client subscribed a term deposit? (binary: "yes","no")
#The outcome variable will be renamed as subscription in further analysis. 



## 2.Summary of the data.



library(plyr)
library(ggplot2)
library(knitr)
library(dplyr)
banks_data = read.csv('https://raw.githubusercontent.com/evprasad/BankData/f7ca2f389ee89e21b90b37aeb1a87dc0c60cedcd/bank-additional-full.csv',stringsAsFactors = F,sep = ";")
#The data is directly being downloaded here for program to read dynamically
colnames(banks_data)[which(colnames(banks_data)=='y')]="subscription"
summary(banks_data)

#The variable 'subscription' is the outcome variable that will be modelled against all the other variables.  The relationship of the outcome with the input factors will be assessed using plots and then there will be the assessment of the relationship using models like logistic regression and random forest.

## 3.Distribution of age vs Subscription


ggplot(banks_data, aes(age,fill=subscription))+geom_boxplot() +ggtitle('Distribution of age vs subscription')


#The distribution of age is wider for those who take term deposit as compared to those who don't. 


## 4.Distribution of Duration vs Subscription



ggplot(banks_data, aes(duration,fill=subscription))+geom_boxplot() +ggtitle('Distribution of Duration vs subscription')

#Duration is on average higher for those to who subscribe to term deposit. 


## 5.Distribution of Campaign vs Subscription


ggplot(banks_data, aes(campaign,fill=subscription))+geom_boxplot() +ggtitle('Distribution of Campaign vs subscription')

#Campaign is on average higher for those to who don't subscribe to term deposit.


##  6.Distribution of Previous vs Subscription



ggplot(banks_data, aes(previous,fill=subscription))+geom_boxplot() +ggtitle('Distribution of previous vs subscription')

#Previous is on average higher for those to who  subscribe to term deposit.


##  7.Distribution of Employment variation rate vs subscription


ggplot(banks_data, aes(emp.var.rate,fill=subscription))+geom_boxplot()  +ggtitle('Distribution of emp.var.rate vs subscription')

#Employee variation rate is on average higher for those to who don't subscribe to term deposit.


##  8.Distribution of consumer price index vs subscription


ggplot(banks_data, aes(cons.price.idx,fill=subscription))+geom_boxplot() +ggtitle('Distribution of cons.price.idx vs subscription')

#Consumer price index is on average higher for those to who don't subscribe to term deposit.


##  9.Distribution of Consumer confidence index vs subscription

ggplot(banks_data, aes(cons.conf.idx,fill=subscription))+geom_boxplot()+ggtitle('Distribution of cons.conf.idx vs subscription')

#Consumer confidence index is on average higher for those to who subscribe to term deposit.


##  10.Distribution of Euribor 3 month rate vs subscription



ggplot(banks_data, aes(euribor3m,fill=subscription))+geom_boxplot()+ggtitle('Distribution of euribor3m vs subscription')

#euribor3m is on average lower for those to who subscribe to term deposit.


##  11.Distribution of Number of employees vs Subscription


ggplot(banks_data, aes(nr.employed,fill=subscription))+geom_boxplot()+ggtitle('Distribution of nr.employed vs subscription')

#Number of employees ( quarterly ) is on average lower for those to who subscribe to term deposit.


## 12.Job  vs subscription


barplot(t(prop.table(table(banks_data$job, banks_data$subscription),margin = 1)),
        beside=T,col = c('blue','red'),main = 'Barplot of job vs subscription',
        legend.text = c('no','yes'),ylim=c(0,1.5),las=2,cex.names  = 0.8)

#Retired people and students have a higher chance of subscribing to term deposit.


## 13.Barplot of marital status vs subscription


barplot(t(prop.table(table(banks_data$marital, banks_data$subscription),margin = 1)),
        beside=T,col = c('blue','red'),main = 'Barplot of marital status vs subscription',
        legend.text = c('no','yes'),ylim=c(0,1.4))

#There is not much difference in chances of term deposit subscription based on marital status.


## 14.Barplot of education vs subscription

par(mgp=c(5,0.05,5))
barplot(t(prop.table(table(banks_data$education, banks_data$subscription),margin = 1)),
        beside=T,col = c('blue','red'),main = 'Barplot of education vs subscription',
        legend.text = c('no','yes'),ylim=c(0,1.5),las=2,cex.names  = 0.7)

#Illiterates have a higher chance of subscribing to term deposit.


## 15.Barplot of default vs subscription

# setting default margin
par(mar=c(5.1, 4.1, 4.1, 2.1), mgp=c(3, 1, 0), las=0)


barplot(t(prop.table(table(banks_data$default, banks_data$subscription),margin = 1)),
        beside=T,col = c('blue','red'),main = 'Barplot of default vs subscription',
        legend.text = c('no','yes'),ylim=c(0,1.8),las=2,cex.names  = 0.7)

#Those who have not defaulted are more likely to subscribe to term deposit


## 16.Barplot of housing vs subscription


barplot(t(prop.table(table(banks_data$housing, banks_data$subscription),margin = 1)),
        beside=T,col = c('blue','red'),main = 'Barplot of housing vs subscription',
        legend.text = c('no','yes'),ylim=c(0,1.8),las=2,cex.names  = 0.7)

#Housing levels are not a good indicator of chances of term deposit subscription.


## 17.Barplot of loan vs subscription


barplot(t(prop.table(table(banks_data$loan, banks_data$subscription),margin = 1)),
        beside=T,col = c('blue','red'),main = 'Barplot of loan vs subscription',
        legend.text = c('no','yes'),ylim=c(0,1.8),las=2,cex.names  = 0.7)

#Status of loan is not a good indicator of subscription to term deposit.


## 18.Barplot of contact vs subscription


barplot(t(prop.table(table(banks_data$contact, banks_data$subscription),margin = 1)),
        beside=T,col = c('blue','red'),main = 'Barplot of contact vs subscription',
        legend.text = c('no','yes'),ylim=c(0,1.8),las=2,cex.names  = 0.7)

#Cellular contact appears to be more efficient at attracting term deposits.


## 19.Barplot of month vs subscription

barplot(t(prop.table(table(banks_data$month, banks_data$subscription),margin = 1)),
        beside=T,col = c('blue','red'),main = 'Barplot of month vs subscription',
        legend.text = c('no','yes'),ylim=c(0,1.8),las=2,cex.names  = 0.7)

#Subscirptions  occur relatively more in December, March, September and October.


## 20.Barplot of day of week vs subscription


barplot(t(prop.table(table(banks_data$day_of_week, banks_data$subscription),margin = 1)),
        beside=T,col = c('blue','red'),main = 'Barplot of day_of_week vs subscription',
        legend.text = c('no','yes'),ylim=c(0,1.8),las=2,cex.names  = 0.7)

#Days of week are not having much impact on whether a person will subscribe a term deposit.


## 21.Barplot of pday vs subscription

# converting pdays to contacted / not contacted
banks_data$pdays = ifelse(banks_data$pdays == 999,'not contacted','contacted')

barplot(t(prop.table(table(banks_data$pdays, banks_data$subscription),margin = 1)),
        beside=T,col = c('blue','red'),main = 'Barplot of pdays vs subscription',
        legend.text = c('no','yes'),ylim=c(0,1.8),las=2,cex.names  = 0.7)

#If person has been contacted in a previous campaign, there is big chance he will subscribe to term deposit.


## 22.Barplot of poutcome vs subscription



barplot(t(prop.table(table(banks_data$poutcome, banks_data$subscription),margin = 1)),
        beside=T,col = c('blue','red'),main = 'Barplot of poutcome vs subscription',
        legend.text = c('no','yes'),ylim=c(0,1.8),las=2,cex.names  = 0.7)

#From the above plot of poutcome vs subscription, it can be seen that the proportion of term deposit subscription is higher if the poutcome is "success". Since poutcome refers to the previous campaign outcome, it can be said that if the previous marketing campaign was successful, the new one will be successful too in getting the customer to subscribe the term deposit.


# converting categorical data to factors
# converting categorical data to factors
for ( j in c('subscription','job','marital','education','default','contact','housing','loan',
             'month','day_of_week','pdays','poutcome')){
  banks_data[,j]<-factor(banks_data[,j])
}




# checking correlation
print('The correlation matrix are given below:')
kable(round(cor(banks_data[,!(colnames(banks_data) %in% c('subscription','job','marital','education','default','contact','housing','loan',
                                                          'month','day_of_week','pdays','poutcome'))]),2))


#emp.var.rate, cons.price.idx, euribor3m and nr.employed  are highly correlated. Therefore, only one of these variables will be used. The consumer price index will be used in the anlaysis. Also, day of week will be deleted. 

# 3. MODELING APPROACH

#The data has been split into training and test sets. The training set comprises of 70% of the data while the test set has 30% of the data. The sample assignment to training and test has been done randomly.

# splitting into traning and test set
cols_used = colnames(banks_data)[!( colnames(banks_data)%in% c('emp.var.rate',  'euribor3m', 'nr.employed', 'day_of_week' ))]
X = banks_data[,cols_used]
set.seed(1)
# using 70% data as training
train_idx = sample(x = nrow(X),size = round(0.7*nrow(X)),replace = F)
Train = X[train_idx,]
Test = X[-train_idx,]



#A logistic regression model has been fit to predict the subscription using all the remaining variables. Logistic regression models log odds of success with the linear combination of the input variables :
#  log(p/(1-p)) = C +  C1*X1 + C2*X2 ... + Cn*Xn, where p is the probability of successful term deposit subscription and Xi are the input variables. 
#Given below is the summary of the model. 

##  Logistic regression model


mod_logistic = glm(subscription~., data = Train, family = binomial())
summary(mod_logistic)


#It can be observed that the variables like jobs, default status, duration, consumer price index, consumer confidence index are significant since the p-values are less than 5%. 



# testing the model
prediction_logistic = predict(mod_logistic,newdata = Test,type = 'response')
prediction_logistic = ifelse(prediction_logistic>0.5,'yes','no')
print("The confusion matrix using the logistic regression model is below :")
mat1 = table(Test$subscription,prediction_logistic)
kable(mat1)
print('The accuracy of the model is :')
(mat1[1,1]+mat1[2,2])/sum(mat1)



#Further, a random forest model has been fit to the same data. Random forest model is an ensemble of decision trees and it doesn't impose any assumption of linearity in the model. It fits multiple decision trees to the data and then uses all of their predictions to get a final prediction. It is a different model from logistic regression in that it uses decision trees while logistic regression uses linearity assumption.

##  Random forest model


if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
library(randomForest)
mod_rf = randomForest(subscription~., data = Train)
# plotting the variable importance
print('given below is the variable importance plot from random forest')
varImpPlot(mod_rf)


#It can be seen that the largest decrease in mean gini( impurity) is brought about by duration , age etc. therefore, these are the variables withe highest importance. 


# testing the model
prediction_rf = predict(mod_rf,newdata = Test)
print("The confusion matrix using the random forest model is below :")
mat = table(Test$subscription,prediction_rf)
kable(mat)
print('The accuracy of the random forest model is :')
(mat[1,1]+mat[2,2])/sum(mat)


#   4.  RESULTS

#The logistics regression model's accuracy is 90.6% while that for the random forest model is 91.1%. The variable importance plot of random forest model shows mean decrease in the gini impurity index and it shows that duration, age, job etc are very important factors. Important variables using logistic regression are job, duration, default status etc which can be verified from the p-values of their categories being less than 0.05. This implies there is a good amount of similarity in the variable importance given by both models on the training data. The confusion matrix using the logistic regression model is :
  
print(kable(mat1))

#The confusion matrix using the random forest model is :
  
print(kable(mat))


#   5.  CONCLUSION

#To attract the fixed deposits, it is observed that the bank should focus on:
  
  
#  - Customers who have had a longer contact duration compared to others on average. The variable duration is significant in both logistic regression and random forest and from the plots it can be seen that those who subscribe to term deposits have has a longer contact duration.

#- Customers with lower age.  On average lower for people who subscribe to term deposits and age is a significant variable in random forest model

#- Customers who have not defaulted. It can be seen from the plots that subscriptions are taken largely by those who have never defaulted on loans and default status is significant variable in logistic regression.

#- consumer confidence index. It is significant in both the models therefore the bank should put more marketing efforts in times when the consumer confidence is high.

#- Retirees and students. They can be seen to be subscribing to term deposits from the plots

