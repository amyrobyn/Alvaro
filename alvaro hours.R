library(dplyr)
library(psych)
library("xlsx")
setwd("C:/Users/amykr/Google Drive/life/alvaro/alvaro hours")
hours <- read.csv("hours and comments.csv")

summary(hours$Age..Hours.)
describe(hours)
names(hours)[9] <- "age"

hours<-hours%>%group_by(Case.Number)%>%mutate(count = n())
hours<-hours[!duplicated(hours$Case.Number), ]
describe(hours$count)

describe(hours$age)
plot(hours$age, hours$count)
plot(hours$age)
#greater than 2.5 sd 440.15*2.5
upper <- 2 + (438.93*2.5)
lower <-2 - (438.93*2.5)

non_outliers <- subset(hours, age <= upper )
summary(non_outliers$age)
describe(non_outliers$age)
describe(hours$age)
1418-1492

plot(non_outliers$age, non_outliers$count)

#remove those where you have to wait on user to repsond via email.
not_waiting_on_user <- subset(hours, Subject !="[REDCap] Request to Move Project to Production" & Subject !="[REDCap] Approve Project Changes")
describe(not_waiting_on_user$age)
plot(not_waiting_on_user$age, not_waiting_on_user$count)

#greater than 2.5 sd 440.15*2.5
upper <- 2 + (451.33*2.5)
lower <-2 - (451.33*2.5)

not_waiting_on_user <- subset(not_waiting_on_user, age <= upper )
not_waiting_on_user <- subset(not_waiting_on_user, count<=30)
#not_waiting_on_user <- subset(not_waiting_on_user, age<=200)

describe(not_waiting_on_user$age)

plot(not_waiting_on_user$age, not_waiting_on_user$count)

hours<-not_waiting_on_user

hours$Date.Closed <- as.numeric(hours$Date.Closed)
hours$Date.Closed <- as.Date(hours$Date.Closed,
                             origin = "1899-12-30")
library("zoo")
hours$month.Closed <- as.yearmon(hours$Date.Closed)
table(hours$month.Closed)
#exclude those closed in may that are over one month old
hours <-subset(hours, !(month.Closed=="May 2017" & age>720))

hours$category<-hours$age

hours<-within(hours, category[hours$age<=24]<-1)
hours<-within(hours, category[hours$age>24]<-2)
hours<-within(hours, category[hours$age<=168]<-3)
attach(hours)

more_than_two_emails_more_than_one_day<-subset(hours, count>2 & age>24)
plot(more_than_two_emails_more_than_one_day$age, more_than_two_emails_more_than_one_day$count)
describe(more_than_two_emails_more_than_one_day$age)
describe(more_than_two_emails_more_than_one_day$count)