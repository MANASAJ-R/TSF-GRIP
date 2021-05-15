##THE SPARKS FOUNDATION##
#GRIPMAY21#
#TASK1-predicting scores using supervised learning#
#author:JAGGAREDDYGARI MANASA REDDY#


#load required libraries & packages


library(caTools)

library(ggplot2)

library(dplyr)

#load given data set from remote source

student_data<-read.csv("https://raw.githubusercontent.com/AdiPersonalWorks/Random/master/student_scores%20-%20student_scores.csv")
View(student_data)
summary(student_data)

#plot graph of hours vs scores

plot(student_data$Hours,student_data$Scores,xlab = "no. of hours studied",ylab = "percentagae of score")

#given_data shows positive linear  relation---> check correlation

cor(student_data$Hours,student_data$Scores)

#cor.value = 0.97.. (very  strong correlation)


#split the given_data into "train" and "test"

set.seed(2)
split<-sample.split(student_data$Hours,SplitRatio = 0.6)
train<-subset(student_data,split=="TRUE")
test<-subset(student_data,split=="FALSE")
train
test

#build LINEAR REGRESSION MODEL using train data set

model<-lm(Scores~Hours,data=train)
model
summary(model)

#plot 
plot(student_data$Hours,student_data$Scores, xlab = 'hours studies', ylab = 'percentage score')
abline(model,col='red')


#prediction of final score

predict_scores<-predict(model,test)
predict_scores

#COMPARISON of actual and predicted values

df<-cbind(Actual=test$Scores,prediction=predict_scores)
DT::datatable(df)

#finding final score for 9.25 hours#

final_score<-data.frame(Hours=c(9.25))
predict_final_score<-predict(lm(Scores~Hours,data = test),newdata =final_score)
predict_final_score 



