#To Read a large CSV file we need ff library
library("ff")
#linear regression model for dead patients
xa<-read.csv.ffdf(file='F:corona data/time_series_covid_19_deaths.csv',header=TRUE,VERBOSE=TRUE,first.rows=10000,next.rows=50000,colClasses=NA)
index_country=0
#A function to get the country from user and finds the index of the country from the data set
#country<-readline('Enter the country you want to predict the death')
#the input function is not working properly so we are manually giving the country here 
country='India'
for(i in seq(1,length(xa$Country.Region),by=1)){
  if(xa$Country.Region[i]==country){
    index_country=i
  }
}

index_country
#library to manipulate user entered date into a count from the starting of the pandemic
library(tidyverse)
library(lubridate)
library(nycflights13)

new.date_calculator <- function(){
  sdod <- mdy("01/22/2020")
  dod <- mdy(readline("Enter the date : "))
  diff_dod <- capture.output(dod-sdod)
  new.seperate(diff_dod) 
}

new.seperate <- function(diff_dod){
  x <- strsplit(diff_dod, " ")[[1]]
  x <- as.numeric(x[4])
  print(x)
}

ndays=new.date_calculator()


#total number of days of the pandemic in the data set
n=length(xa)
n
#y number of deaths
#x number of days
#xlist contains the list of days happened
xlist<-c()
for(i in seq(1,n-4,by=1)) {
  xlist[i]=i
}

j=1
ylist<-c()
#xa[1,5]
#ylist contains the list of deaths happened over each day in the given country
for(i in seq(5,n,by=1)){
  ylist[j]=(xa[index_country,i])
  j=j+1
}
ylist
#linear regression:
#summation of xlist will be the total number of days
sigmax=n
#summation of ylist will be the total death occured in the country 
sigmay=ylist[n-4]
xlist
length(xlist)
ylist
length(ylist)
#sigmaxy=sum(xlist*ylist) 
sigmaxy=n*ylist[n-4]
#sigmax2=sum(xlist*xlist)
sigmax2=n*n
b=(n*sigmaxy-sigmax*sigmay)/(n*sigmax2-(sigmax*sigmax))
b
a=(sigmay-b*(sigmax))/n
a
#y=ax+b
#ploting the graph for visualisation
x<-seq(1,1000,1)
plot(x,a*x+b,xlab='')

x=ndays
x
#substituting the value of the number of days to predict we find the number of deaths happened on that day
y=a*x+b
predicted_death=y
#if(y<0){
#  print('Zero deaths')
#}else{
#  y
#}

#Similar model for RECOVERED Patients
xa<-read.csv.ffdf(file='F:/corona data/time_series_covid_19_recovered.csv',header=TRUE,VERBOSE=TRUE,first.rows=10000,next.rows=50000,colClasses=NA)


#total number of days of the pandemic in the data set
n=length(xa)
n
#y number of deaths
#x number of days
#xlist contains the list of days happened
xlist<-c()
for(i in seq(1,n-4,by=1)) {
  xlist[i]=i
}

j=1
ylist<-c()
#xa[1,5]
#ylist contains the list of deaths happened over each day in the given country
for(i in seq(5,n,by=1)){
  ylist[j]=(xa[index_country,i])
  j=j+1
}
ylist
#linear regression:
#summation of xlist will be the total number of days
sigmax=n
#summation of ylist will be the total death occured in the country 
sigmay=ylist[n-4]
xlist
length(xlist)
ylist
length(ylist)
#sigmaxy=sum(xlist*ylist) 
sigmaxy=n*ylist[n-4]
#sigmax2=sum(xlist*xlist)
sigmax2=n*n
b=(n*sigmaxy-sigmax*sigmay)/(n*sigmax2-(sigmax*sigmax))
b
a=(sigmay-b*(sigmax))/n
a
#y=ax+b
#ploting the graph for visualisation
x<-seq(1,1000,1)
plot(x,a*x+b,xlab='')

x=ndays
x
#substituting the value of the number of days to predict we find the number of deaths happened on that day
y=a*x+b
predicted_recover=y
#if(y<0){
#  print('Zero deaths')
#}else{
#  y
#}


#And regression model for confirmed model
xa<-read.csv.ffdf(file='F:/corona data/time_series_covid_19_confirmed.csv',header=TRUE,VERBOSE=TRUE,first.rows=10000,next.rows=50000,colClasses=NA)

#total number of days of the pandemic in the data set
n=length(xa)
n
#y number of deaths
#x number of days
#xlist contains the list of days happened
xlist<-c()
for(i in seq(1,n-4,by=1)) {
  xlist[i]=i
}

j=1
ylist<-c()
#xa[1,5]
#ylist contains the list of deaths happened over each day in the given country
for(i in seq(5,n,by=1)){
  ylist[j]=(xa[index_country,i])
  j=j+1
}
ylist
#linear regression:
#summation of xlist will be the total number of days
sigmax=n
#summation of ylist will be the total death occured in the country 
sigmay=ylist[n-4]
xlist
length(xlist)
ylist
length(ylist)
#sigmaxy=sum(xlist*ylist) 
sigmaxy=n*ylist[n-4]
#sigmax2=sum(xlist*xlist)
sigmax2=n*n
b=(n*sigmaxy-sigmax*sigmay)/(n*sigmax2-(sigmax*sigmax))
b
a=(sigmay-b*(sigmax))/n
a
#y=ax+b
#ploting the graph for visualisation
x<-seq(1,1000,1)
plot(x,a*x+b,xlab='')

x=ndays
x
#substituting the value of the number of days to predict we find the number of deaths happened on that day
y=a*x+b
predicted_confirm=y
#if(y<0){
#  print('Zero deaths')
#}else{
#  y
#}

#conclusion
if(predicted_death<predicted_confirm-predicted_recover){
  print('the total death predicted is')
  predicted_death
}else{
  print('the total death predicted is')
  predicted_confirm-predicted_recover
}