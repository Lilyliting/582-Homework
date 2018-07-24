#Problem 1

library("gdata")
library("plyr")
library("ggplot2")
setwd("/Users/apple/Desktop/HWs/582_1/HW1_F16")

bx <- read.xls("rollingsales_bronx.xls", perl = "/usr/bin/perl", pattern="BOROUGH")
bk <- read.xls("rollingsales_brooklyn.xls", perl = "/usr/bin/perl", pattern="BOROUGH")
mh <- read.xls("rollingsales_manhattan.xls", perl = "/usr/bin/perl", pattern="BOROUGH")
qn <- read.xls("rollingsales_queens.xls", perl = "/usr/bin/perl", pattern="BOROUGH")
si <- read.xls("rollingsales_statenisland.xls", perl = "/usr/bin/perl", pattern="BOROUGH")
#Load in data

cleandata <- function(x) {
    names(x) <- tolower(names(x))
    sale.price.n <- as.numeric(gsub("[^[:digit:]]","",x$sale.price))
    #price should be numeric
    x$gross.square.feet <- as.numeric(gsub("[^[:digit:]]","",x$gross.square.feet))
    x$land.square.feet <- as.numeric(gsub("[^[:digit:]]","",x$land.square.feet))
    #gross square feet and land square feet should be numeric
    x$sale.date <- as.Date(x$sale.date)
    #time series
    x$year.built <- as.numeric(as.character(x$year.built))
    x <- data.frame(x, sale.price.n)
    mvc <- length(x$sale.price.n)-count(is.na(x$sale.price.n))[2]
    if (mvc == 0) message("No missing value")
    else message("There exists at least one missing value")
    #to see if there exists missing value
    x <- subset(x, log(x$sale.price.n) > 5)
    #delete data whose sale price is too low
    return(x)
}
bx <- cleandata(bx)
bk <- cleandata(bk)
mh <- cleandata(mh)
qn <- cleandata(qn)
si <- cleandata(si)
#Clean/format the data and remove unreasonable data whose sale prices are too low 

addbuildingtype <- function(x) {
    x$building.type <- "6 Others"
    x$building.type[grepl("ONE FAMILY",x$building.class.category)] <- "1 One family"
    x$building.type[grepl("TWO FAMILY",x$building.class.category)] <- "2 Two family"
    x$building.type[grepl("THREE FAMILY",x$building.class.category)] <- "3 Three family"
    x$building.type[grepl("COOPS",x$building.class.category)] <- "4 Coops"
    x$building.type[grepl("CONDOS",x$building.class.category)] <- "5 Condos"
    x$building.type <- factor(x$building.type)
    return(x)
}

bx <- addbuildingtype(bx)
bk <- addbuildingtype(bk)
mh <- addbuildingtype(mh)
qn <- addbuildingtype(qn)
si <- addbuildingtype(si)
#Building type

aldata <- merge(mh, merge(bk, bx, all = TRUE), all = TRUE)
aldata <- merge(si, merge(qn, aldata, all = TRUE), all = TRUE)
aldata$borough <- as.factor(aldata$borough)

hist(aldata$sale.price.n[aldata$sale.price.n<5000000], xlab = "sale price", main = "Sale price distribution")
hist(log(aldata$sale.price.n[aldata$sale.price.n<5000000]), xlab = "log of sale price", main = "Log sale price distribution")
#price distribution

#attach
ggplot(mh,aes(x=sale.date, y=log(sale.price.n),colour=building.type))+geom_jitter(size=.6)+geom_smooth()
ggplot(bx,aes(x=sale.date, y=log(sale.price.n),colour=building.type))+geom_jitter(size=.6)+geom_smooth()
ggplot(bk,aes(x=sale.date, y=log(sale.price.n),colour=building.type))+geom_jitter(size=.6)+geom_smooth()
ggplot(qn,aes(x=sale.date, y=log(sale.price.n),colour=building.type))+geom_jitter(size=.6)+geom_smooth()
ggplot(si,aes(x=sale.date, y=log(sale.price.n),colour=building.type))+geom_jitter(size=.6)+geom_smooth()
ggplot(aldata,aes(x=sale.date, y=log(sale.price.n),colour=building.type))+geom_jitter(size=.6)+geom_smooth()
#sale.date-price
ggplot(subset(aldata, aldata$year.built >1900), aes(x=year.built, y=log(sale.price.n),colour=borough))+geom_jitter(size=.6)+geom_smooth()
#year.built-price
ggplot(aldata, aes(x=borough, y=log(sale.price.n), fill=borough))+geom_boxplot()
#borough-price
#1-Manhattan, 2-Bronx, 3-Brooklyn, 4-Queen, 5-Statenisland
ggplot(bk, aes(x=building.type, y=log(sale.price.n), fill=building.type))+geom_boxplot()
ggplot(aldata, aes(x=borough, y=log(sale.price.n), fill=building.type))+geom_boxplot()
#building.type-price
ggplot(subset(bk, bk$gross.square.feet >0 & bk$gross.square.feet < 10000), aes(x=gross.square.feet, y=log(sale.price.n),colour=building.type))+geom_jitter(size=.5)
#gross.square.feet-price
ggplot(aldata, aes(x=borough,fill=building.type))+geom_bar(position = "fill")
#borough-building.type

#----------------------------------------------------------
#Problem 2

library("doBy")

data1 <- read.csv("nyt1.csv")
data2 <- read.csv("nyt2.csv")
data3 <- read.csv("nyt3.csv")

data1$age_group <- cut(data1$Age, c(-Inf,0,19,29,39,49,59,69,Inf))
data2$age_group <- cut(data2$Age, c(-Inf,0,19,29,39,49,59,69,Inf))
data3$age_group <- cut(data3$Age, c(-Inf,0,19,29,39,49,59,69,Inf))
#Create age_group

createcate <- function(x) {
    x$scode[x$Impressions==0] <- "NoImps"
    x$scode[x$Impressions >0] <- "Imps"
    x$scode[x$Clicks >0] <- "Clicks"
    x$scode <- factor(x$scode)
    return(x)
}
data1 <- createcate(data1)
data2 <- createcate(data2)
data3 <- createcate(data3)
#Create categories

data1$Day <- "Day 1"
data2$Day <- "Day 2"
data3$Day <- "Day 3"

data123 <- merge(data3, merge(data1, data2, all = T), all = T)

summaryBy(Age~age_group, data=data123, FUN=c(length,min,mean,max))
summaryBy(Gender+Signed_In+Impressions+Clicks~age_group, data = data123)
#Only signed in users have ages and genders

ggplot(data1, aes(x=Impressions, fill=age_group)) +geom_histogram(binwidth=1)
ggplot(data1, aes(x=age_group, y=Impressions, fill=age_group)) +geom_boxplot()

ggplot(data2, aes(x=Impressions, fill=age_group)) +geom_histogram(binwidth=1)
ggplot(data2, aes(x=age_group, y=Impressions, fill=age_group)) +geom_boxplot()

ggplot(data3, aes(x=Impressions, fill=age_group)) +geom_histogram(binwidth=1)
ggplot(data3, aes(x=age_group, y=Impressions, fill=age_group)) +geom_boxplot()
#Distribution of number of impressions

all(data123$Impressions >= data123$Clicks)
#Impressions always come with clicks

data1$hasimp <- cut(data1$Impressions,c(-Inf,0,Inf))
summaryBy(Clicks~hasimp, data=data1, FUN=c(length,min,mean,max))
ggplot(subset(data123, Clicks >0),aes(x=Clicks/Impressions,colour=age_group))+geom_density()
# create click thru rate
# we don't consider clicks if there are no impressions

data123$gender_group <- cut(data123$Gender, c(-Inf,0,Inf), labels = c("female", "male"))
ggplot(subset(data123, Clicks > 0 & Signed_In > 0),aes(x=Clicks/Impressions,colour=gender_group))+geom_density()
ggplot(subset(data123, Clicks > 0 & Signed_In > 0 & Age <= 20),aes(x=Clicks/Impressions,colour=gender_group))+geom_density()
ggplot(subset(data123, Clicks > 0 & Signed_In > 0),aes(x=Impressions,fill=gender_group))+geom_bar(position = "dodge")
ggplot(subset(data123, Clicks > 0 & Signed_In > 0 & Age <= 20),aes(x=Impressions,fill=gender_group))+geom_bar(position = "dodge")
#by gender
data123$sign_group <- cut(data123$Signed_In, c(-Inf,0,Inf), labels = c("not signed in", "signed in"))
ggplot(subset(data123, Clicks >0),aes(x=Clicks/Impressions,colour=sign_group))+geom_density()
#by signed in or not
ggplot(subset(data123, Clicks >0),aes(x=Clicks/Impressions,colour=Day))+geom_density()
#by day