# Analysis of MBA Salaries
# NAME: ANKIT KASHYAP
# EMAIL: ankitkashyap.te15@rvce.edu.in
# COLLEGE: R.V COLLEGE OF ENGINEERING 
####### MBA Starting Salaries #######
##setting the directory and assigning a variabel to the data frame
setwd("C:/Users/Ankit/Desktop/harvard")

#Reading the dataset and creating a data frame
mbasal.df<-read.csv(paste("MBA Starting Salaries Data.csv",sep = ""))

#Viewing the data frame
View(mbasal.df)

##Analyzing the summary of the data and describing the variables
library(psych)
describe(mbasal.df)
summary(mbasal.df)
##Creating a new data frame for the students who had a starting salary
startsalary.df<-mbasal.df$salary[mbasal.df$salary>999]

##Drawing boxplots among comparable parameters

boxplot(mbasal.df, xlab="Value", ylab="Parameters", main="BoxPlot Presentation Of differnt Parameters")

##Individual boxplots for comparable parameter
boxplot(startsalary.df,horizontal = TRUE,xlab="Salary",main="Boxplot Presentation of sarting salaries")

par(mfrow=c(1,2))
with(mbasal.df, boxplot(mbasal.df$f_avg,main="Fall MBA AVerage",ylab="Average"))
with(mbasal.df, boxplot(mbasal.df$s_avg,main="Spring MBA Average",ylab="Average"))
par(mfrow=c(1,1))

par(mfrow=c(1,3))
with(mbasal.df, boxplot(mbasal.df$gmat_qpc,main="GMAT Quantative Percentile",ylab="Percentile"))
with(mbasal.df, boxplot(mbasal.df$gmat_vpc,main="GMAT Verbal Percentile",ylab="Percentile"))
with(mbasal.df, boxplot(mbasal.df$gmat_tpc,main="GMAT Total Percentile",ylab="Percentile"))
par(mfrow=c(1,1))

boxplot(mbasal.df$gmat_tot,main="Total GMAT Score",ylab="Score")
boxplot(mbasal.df$work_yrs,main="Total Working Experience",ylab="Years")


## Bar Plots to visualize the distribution of each variable independently

count1<-table(mbasal.df$age)
barplot(count1,main="Barplot for Age",xlab="Years")

count2<-table(mbasal.df$sex)
barplot(count2,main="Barplot for Sex",xlab="Sex 1=Male 2=Female")

count3<-table(mbasal.df$quarter)
barplot(count3,main="Barplot for Quarter",xlab="Quarter")

count4<-table(mbasal.df$frstlang)
barplot(count4,main="Barplot for First Language",xlab="1=English,2=Others")

count5<-table(mbasal.df$satis[mbasal.df$satis<998])
barplot(count5,main="Barplot for satisfaction of course",xlab="Satisfaction Level")

##Scatter Plots/ Plots to understand how are the variables correlated pair-wise

salary1.df<-mbasal.df[which(mbasal.df$salary>999),]
View(salary1.df)
library(car)

scatterplot(salary1.df$salary,salary1.df$age,main="Starting Salary of MBA Graduates",ylab = "Age", xlab="Salary",cex=1.1,pch=19)
scatterplot(salary1.df$salary,salary1.df$work_yrs,main="Starting Salary of MBA Graduates",ylab = "Work Experience", xlab="Salary",cex=1.1,pch=19)
scatterplot(salary1.df$salary,salary1.df$gmat_tpc,main="Starting Salary of MBA Graduates",ylab = "GMAT percentile", xlab="Salary",cex=1.1,pch=19)


##Plots for binary categorical data with starting salaries
plot(jitter(salary1.df$salary),jitter(salary1.df$sex),main="Starting Salary of MBA Graduates",ylab = "Sex", xlab="Salary",cex=1.1)
plot(jitter(salary1.df$salary),jitter(salary1.df$frstlang),main="Starting Salary of MBA Graduates",ylab = "First Language", xlab="Salary",cex=1.1)
plot(jitter(salary1.df$salary),jitter(salary1.df$satis),main="Starting Salary of MBA Graduates",ylab = "Degree of satisfaction", xlab="Salary",cex=1.1)


library(car)
scatterplotMatrix(
  salary1.df[
    ,c("salary","work_yrs","gmat_tpc")],
  spread=FALSE, smoother.args=list(lty=2),
  main="Scatter Plot Matrix", diagonal = "histogram")


##Correlation tests to find relationship between different parameters
# Correlation matrix,covariance matrix, Corrgram

x<-mbasal.df[,c("age","sex","gmat_tot","gmat_qpc", "gmat_vpc","gmat_tpc","s_avg", "f_avg","quarter", "work_yrs", "frstlang", "salary","satis")]
y<-mbasal.df[,c("salary","gmat_tpc","work_yrs","satis","age")]
cor(x,y)
cov(x,y)
var(x,y)

#Visualizing relation through corrplots

library(corrplot)
corrplot(corr=cor(salary1.df[,c(1:13)],use = "complete.obs"), method = "ellipse")
library(gplots)
corrplot.mixed(corr=cor(salary1.df[,c(1:13)],use = "complete.obs"), upper = "ellipse", tl.pos = "lt", col = colorpanel(50, "red", "gray60", "blue4"))

#VIsualizing by corrgram
 
library(corrgram)

corrgram(salary1.df, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Corrgram of MBA Starting Salaries")

##Chi Square Test 
mytable<-xtabs(~sex+work_yrs,data=salary1.df)
chisq.test(mytable)
addmargins(mytable)

##Because p value is more than 0.05 we cannot reject the null hpothesis and
##the parameter sex and work experience are independant

mytable1<-xtabs(~satis+work_yrs,data=salary1.df)
chisq.test(mytable1)
addmargins(mytable1)

##Since te p value is less than 0.05 we can't reject the null hypothesis and the
##parameters of work experience and level of satisfaction are not independant

mytable2<-xtabs(~sex+frstlang,data=salary1.df)
chisq.test(mytable2)
addmargins(mytable2)

##Because p value is more than 0.05 we cannot reject the null hpothesis and
##the parameter sex and first language are independant

mytable3<-xtabs(~work_yrs+frstlang,data=salary1.df)
chisq.test(mytable3)
addmargins(mytable3)

##Since te p value is less than 0.05 we can't reject the null hypothesis and the
##parameters of work experience and level of satisfaction are not independant 


##Average Salary of male is greater than average salary of females. 
t.test(salary ~ sex,alternative= "greater",data=salary1.df)
##Since the p value is more than 0.05 we can't reject the null hypothesis

##Average salary of people whose first language is English is greater than average
##salary of other language speakers
 
t.test(salary ~ frstlang,alternative= "greater",data=salary1.df)
##Since the p value is more than 0.05 we can't reject the null hypothesis


##Average GMAT percentile of male is greater than that of female
t.test(gmat_tpc ~ sex,alternative= "greater",data=salary1.df)
##Since the p value is more than 0.05 we can't reject the null hypothesis


##Generating A Multi Variable Linear Regressional Model for MBA Starting Salary
##1.

linear1.mod<- lm(salary~ work_yrs + gmat_tot -1, data = salary1.df)
summary(linear1.mod)
#Coefficients of the model
coefficients(linear1.mod)
#Residuals of the model
residuals(linear1.mod)
#Fitting the model
fitted(linear1.mod)
##   Model:    salary = b0 + b1*work_yrs + b2*gmat_tot
##  b0 = -1(assumption),  b1 = 3264.2887,b2= 146.7158
## Model:    salary = -1 + 3264.2887*work_yrs+146.7158*gmat_tot


##2.
linear2.mod<-lm(salary~work_yrs+age*frstlang+gmat_tot+sex-1,data= salary1.df)
summary(linear2.mod)
#Coefficients of the model
linear2.mod$coefficients
#Residuals of the model
residuals(linear2.mod)
#Fitting the model
fitted(linear2.mod)
##   Model:    salary = b0 + b1*work_yrs + b2*age +b3*frstlang +b4*gmat_tot +b5*sex +b6*sex*frstlang
##  b0 = -1(assumption),  b1 = -958.86681,b2=2905.80137,b3=-15290.15225,b4=40/35853,b5=-2260.92128,b6=794.41173
## Model:    salary = -1 + -958.86681*work_yrs + 2905.80137*age +-1290.15225*frstlang +40.35853*gmat_tot + -2260.92128*sex + 794.41173*sex*frstlang



##3.
linear3.mod<-lm(salary~work_yrs+age,data=salary1.df)
summary(linear3.mod)
#Coefficients of the model
linear3.mod$coefficients
#Residuals of the model
residuals(linear3.mod)
#Fitting the model
fitted(linear3.mod)
##   Model:    salary = b0 + b1*work_yrs + b2*age
##  b0 = 36967.4546,  b1 = 388.8347,b2= 2413.7599
## Model:    salary = 36967.4546 + 388.8347*work_yrs+2413.7599*age

##Creating a subset for those who did not get a job
salary2.df<-mbasal.df[which(mbasal.df$salary<998),]

##Chi square test
mytable4<-xtabs(~sex+work_yrs,data=salary2.df)
chisq.test(mytable4)
addmargins(mytable4)
##Since the p value is more than 0.05 we can't reject the null hypothesis and
##the parameter sex and work experience are independant

mytable5<-xtabs(~sex+frstlang,data=salary2.df)
chisq.test(mytable5)
addmargins(mytable5)
##Since the p value is more than 0.05 we can't reject the null hypothesis and
##the parameter sex and first language are independant



##################################CHALLENGE#########################
#Logistic Regression Analysis for students who got a job
salary1.df$sex<-factor(salary1.df$sex)
is.factor(salary1.df$sex)
logic1.mod<-glm(sex~.,family = binomial(link = 'logit'),data=salary1.df)
summary(logic1.mod)
anova(logic1.mod,test = "Chisq")
fitted.results<-predict(logic1.mod,data=salary1.df,type = 'response')
fitted.results<-ifelse(fitted.results>0.5,1,0)
misClassificError<-mean(fitted.results != salary1.df$sex)
print(paste('Accuracy',1-misClassificError))


#Logistic Regression Analysis for students who did not got a job
salary2.df$sex<-factor(salary2.df$sex)
is.factor(salary2.df$sex)
logic2.mod<-glm(sex~.,family = binomial(link = 'logit'),data=salary2.df)
summary(logic2.mod)
anova(logic2.mod,test = "Chisq")
fitted.results<-predict(logic2.mod,data=salary2.df,type = 'response')
fitted.results<-ifelse(fitted.results>0.5,1,0)
misClassificError<-mean(fitted.results != salary2.df$sex)
print(paste('Accuracy',1-misClassificError))




