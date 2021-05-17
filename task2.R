   
                                    #THE SPARKS FOUNDATION#
                                        #GRIP MAY 2021#
                             #TASK-2,DATA SCENCE& BUSINES ANALYTICS#
                            #AUTHOR  :.MANASA REDDY J#


#INSTALL& LOAD REQUIRED PACKAGES#


install.packages("ggfortify")
library(stats)
library(dplyr)
library(ggplot2)
library(ggfortify)



#loading IRIS data#
#given iris data is default data in R#

given_data<-(iris)
View(given_data)

# as it is un supervised learning, we need to use unlabeled data here.so, remove variable "SPECIES".
required_data<-select(given_data,c(1,2,3,4))
required_data

#Plot of required_data#

Data<-required_data[2:4]
str(Data)
plot(Data,main="wd.& lt of sepal and petal",pch=20,cex=2)


#get wws plot function(not default) from Rpubs,for finding  k=?

wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
  wss
}

#wwsplot of required_data#

wssplot(required_data)

#knit is observed at 2 in wwsplot,, optimum value of k 2 #
#lets find k means#

km<-kmeans(required_data,2)
km

#visualization#

cluster_plot<-autoplot(km,required_data,frame = TRUE)
cluster_plot

#Evaluation#
km$centers

#above evaluation method is depicting clear distinctiveness so, optimum values of clusters ="2"#
