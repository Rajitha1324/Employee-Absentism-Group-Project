#R Project
#EMPOYEE ABSENTISM

#Predict the trend in absenteeism of employees of a given company and what actions should the company 
#undertake to reduce such absenteeism.


#Load the data into R
library(readxl)
Employeedata<-read_excel("C:/Users/rr/Downloads/data_set.xls")
Employeedata= as.data.frame(Employeedata)
View(Employeedata)
dim(Employeedata) # number of rows & columns in data
str(Employeedata)
summary(Employeedata)

#Plottings
#histogram of absence hours
hist(Employeedata$`Absenteeism time in hours`, col = "blue")

#boxplot of Reasons for Absence
boxplot(Employeedata$`Absenteeism time in hours` ~ Employeedata$`Reason for absence`, col = c(1:29))
#Reason 9 (Diseases of the circulatory system) causes longer absences,
#followed by Reason 12 (Diseases of the skin and subcutaneous tissue).

#Top 5 Most absence hours per observation
library(dplyr)
abs5 <- data.frame(Employeedata[,c(1,3,4,2,21)])
head(arrange(abs5, desc(abs5$Absenteeism.time.in.hours)), n = 5)
#Employee 9 has been absent for 120h (=15d) on Tuesdays in July over 3 years for 
#reason 6 (Diseases of the nervous system) and 112h (=14d) on Tuesdays in March for 
#reason 12 (Diseases of the skin and subcutaneous tissue)

#How the absence hours are distributed over the years. In the case of the 120h, the employee must have 
#been absent almost every Tuesday in July over the three years. If there are e.g. 8h, one doesn't 
#know when that happened and how the hours are distributed.

#Bottom 5 absence hours per observation
head(arrange(abs5, abs5$Absenteeism.time.in.hours, abs5$ID), n = 5)
#There are some absence hours = 0, reason = 0 and also month = 0.
#That looks like the dataset contains all the employees, and some haven't been absent in certain months 
#or have never been absent (month = 0).

#Employees without any absence
subset(abs5,abs5$Month.of.absence==0)
#There are 3 employees without any absence hours during the 3 years
#When these 3 employees have started to work for the company - maybe that was just a month before 
#the data collection has finished

#Absence Hours per month
barplot(table(abs5$Absenteeism.time.in.hours,abs5$Month.of.absence),xlim = c(0,16),ylim = c(0,100), main="Absence Hours per Month", xlab="Months",col=c("Blue"))
#March has the highest sum of absence hours

#Absence Hours per week
barplot(table(abs5$Absenteeism.time.in.hours,abs5$Day.of.the.week),xlim = c(0,6),ylim = c(0,200), main="Absence Hours per Weekday", xlab="Weeks",col=c("Blue"))
#Mondays has the highest sum of absence hours

#Weekday and Month wise Absence hours
df <- data.frame(aggregate(abs5$Absenteeism.time.in.hours ~ abs5$Month.of.absence + abs5$Day.of.the.week, data = abs5, sum, na.rm = TRUE))
head(arrange(df,desc(df$abs5.Absenteeism.time.in.hours)),5)
#The Tuesdays in July have the highest sum of absence hours, 2nd are the Mondays in March.

#Number of employees per month and weekday
df2 <- data.frame(aggregate(abs5$ID, by=list(Months = abs5$Month.of.absence,Weeks = abs5$Day.of.the.week), FUN=length))
head(arrange(df2,desc(df2$x)))
#Most employees are absent on Wednesdays in May, 2nd are Mondays in March

#Absence Hours and Number of employee per reasons
df3 <- data.frame(aggregate(abs5$Absenteeism.time.in.hours ~ abs5$Reason.for.absence, data = abs5, sum, na.rm = TRUE))
df4 <- data.frame(aggregate(abs5$ID, by=list(reason = abs5$Reason.for.absence), FUN=length))
df5 <- cbind(df3,No.ofemployees = df4[,2])
head(arrange(df5,desc(df5$abs5.Absenteeism.time.in.hours)))
#Reason 13 (musculoskeletal system and connective tissue) causes the most absence hours, and 18 employees are affected.
#Reason 19 (Injury, poisoning and certain other consequences of external causes) has the 2nd place, 16 employees are affected
#Reason 23 (medical consultation) causes the 3rd most absence hours and most of the employees (24) are affected.
#Reason 20 (external causes of morbidity and mortality) includes traffic accident: just an additional ICD-10 code and not included in that dataset

#Cumulated absence hours and reasons
df6 <- arrange(df5,desc(df5$abs5.Absenteeism.time.in.hours))
df6<- cbind(df6,cumsum=cumsum(df6$abs5.Absenteeism.time.in.hours),cumper=100*cumsum(df6$abs5.Absenteeism.time.in.hours)/sum(df6$abs5.Absenteeism.time.in.hours))
head(df6)
#only five reasons cause more than 50% of absence hours:
#13: Diseases of the musculoskeletal system and connective tissue
#19: Injury, poisoning and certain other consequences of external causes
#23: medical consultation
#28: dental consultation
#10: Diseases of the respiratory system

#sum of absence hours per month and reason
df7 <- data.frame(aggregate(abs5$Absenteeism.time.in.hours ~ abs5$Reason.for.absence + abs5$Month.of.absence, data = abs5, sum, na.rm = TRUE))
head(arrange(df7,desc(df7$abs5.Absenteeism.time.in.hours)),5)
#The two top categories stick out:
#13: Diseases of the musculoskeletal system and connective tissue - peak in April
#19: Injury, poisoning and certain other consequences of external causes - peak in March

#Number of employees per month and reason
table(abs5$Reason.for.absence,abs5$Month.of.absence)
#Reasons with the highest number of absent employees:
#23: medical consultation - less during Brazilian summer
#28: dental consulatation - more or less evenly distributed


#Transform the datatypes
name<-c(2:5,12:17)
Employeedata[,name]<-lapply(Employeedata[,name],factor)
str(Employeedata)

anyNA(Employeedata)
library(DMwR)

Employeedata=as.data.frame(knnImputation(Employeedata,k = 5))
str(Employeedata)


#checking for missing values
colSums(is.na(Employeedata))


#correlation calculation
# select numeric variables
df <- dplyr::select_if(Employeedata, is.numeric)

# calulate the correlations to see for multicollinearity
library(ggplot2)
r <- cor(df, use="all.obs")
round(r,2)
library(ggcorrplot)
ggcorrplot(r, 
           hc.order = TRUE, 
           type = "lower",
           lab = TRUE)


Empdata= as.data.frame(subset(Employeedata,select = -c(Weight,Height)))

#we can see our data is not normalised.
#The machine learning algorithm will be dominated by the variables that use a larger scale,
#adversely affecting model performance
str(Empdata)
nums_list<-names(dplyr::select_if(Empdata[,-c(1,9,19)],is.numeric))
nums_list
for (i in nums_list)
{
  Empdata[,i] <- (Empdata[,i] - min(Empdata[,i])) / 
    (max(Empdata[,i]) - min(Empdata[,i]))
}
summary(Empdata)
str(Empdata)


#PREDICTIVE MODELS



#Divide data into train and test
set.seed(121)
train.index = sample(1:nrow(Empdata), 0.8 * nrow(Empdata))
train = Empdata[ train.index,]
test  = Empdata[-train.index,]



#LINEAR REGRESSION
lm_model <- lm(`Absenteeism time in hours`~. , data = train)
summary(lm_model)

lm_pred <- predict(lm_model)

plot(train$`Absenteeism time in hours`,col='green',type='l',lty=2)
lines(lm_pred,col='blue')

library(caret)
print(postResample(pred =lm_pred, obs = test$`Absenteeism time in hours`))

#GRADIENT BOOST

#Develop Model on training data
library(gbm)
fit_XGB = gbm(`Absenteeism time in hours`~., data = train, n.trees = 500, interaction.depth = 2)

XG_predictions = predict(fit_XGB, test)

#Plotting graph of actual vs predicted values
plot(test$`Absenteeism time in hours`,type="l",lty=2,col="green")
lines(XG_predictions,col="blue")

print(postResample(pred =XG_predictions, obs = test$`Absenteeism time in hours`))

#DECISION TREE
library(rpart)
dt_model = rpart(`Absenteeism time in hours`~ ., data = train, method = "anova")

#Plot the tree
library(rpart.plot)
windows()
rpart.plot(dt_model)

#predicting the test set results
dt_predictions = predict(dt_model, test)

#Plotting graph of actual vs predicted values
plot(test$`Absenteeism time in hours`,type="l",lty=2,col="green")
lines(dt_predictions,col="blue")

library(caret)
print(postResample(pred =dt_predictions, obs = test$`Absenteeism time in hours`))





#RANDOM FOREST
library(randomForest)
#Training model using training data
rf_model = randomForest(x= train,y=train$`Absenteeism time in hours`,ntree=500)

#Predicting for test data
rf_predictions = predict(rf_model, test)

#Plotting graph of actual vs predicted values
plot(test$`Absenteeism time in hours`,type="l",lty=2,col="green")
lines(rf_predictions,col="blue")

print(postResample(pred =rf_predictions, obs = test$`Absenteeism time in hours`))
