#####################################################################
#---- Include all required libraries----
require(ggplot2)
require(plyr)


theme_nobackground <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
#---- Read Data from csv file ------- 
demographic_data <- read.csv('Demographic data.csv',na.strings = c(""," ","  "))
credit_bureau_data <-  read.csv('Credit Bureau data.csv',na.strings = c(""," ","  "))
library(lme4)
library(MASS)
library(car)
library(e1071)
library(caret)
library(ggplot2)
library(cowplot)
library(caTools)
library(munsell)
library(ModelMetrics)
library(RcppRoll)
library(bindrcpp)
library(glue)
library(pkgconfig)
library(ddalpha)
library(labeling)
library(DEoptimR)
library(robustbase)
library(plotly)
library(gdata) 
library(dummies)
## Get repeating Application.ID
which(count(demographic_data,"Application.ID")[2]>1)
which(count(credit_bureau_data,"Application.ID")[2]>1)
count(demographic_data,"Application.ID")[c(46613,47928,54589),]
count(credit_bureau_data,"Application.ID")[c(46613,47928,54589),]

## Retain only unique Application.Id
demographic_data <- demographic_data[!duplicated(demographic_data[,c('Application.ID')]),]
credit_bureau_data <- credit_bureau_data[!duplicated(credit_bureau_data[,c('Application.ID')]),] 

which(count(demographic_data,"Application.ID")[2]>1)
which(count(credit_bureau_data,"Application.ID")[2]>1)

## Merge Data based on Application.Id
diff_applications <- setdiff(demographic_data$Application.ID,credit_bureau_data$Application.ID)
master_data <- merge(demographic_data,credit_bureau_data,by="Application.ID")

## Count of demographic_data, credit_bureau_data and master_data are same.


#---- View Data columns and data types----
colnames(master_data)
head(master_data,10)
summary(master_data)

#---- Null value analysis and handling ----
## Total Null values 4605
sum(is.na(master_data))
## Columnwise NA value %
column_description_null <- apply(master_data,MARGIN = 2, function(x) sum(is.na(x))/length(x))
barplot(column_description_null[which(column_description_null>0)],las=2)

#---- Performance Tag Analysis----
## No difference in performance tag between two application Id from Bureau Credit Data and demographic data
setdiff(master_data$Performance.Tag.x,master_data$Performance.Tag.y)
master_data <- subset(master_data,select = -c(Performance.Tag.x))
colnames(master_data)[colnames(master_data) == 'Performance.Tag.y'] <- 'Performance.Tag'
master_data[which(master_data$No.of.times.90.DPD.or.worse.in.last.12.months>0),'Performance.Tag'] <- 1
master_data$Performance.Tag <- as.factor(master_data$Performance.Tag)


## Remove NA data
master_data <- na.omit(master_data)
column_description_null <- apply(master_data,MARGIN = 2, function(x) sum(is.na(x))/length(x))

#---- Unique Value Analysis ----
column_description_unique <- apply(demographic_data, 2, function(x) length(unique(x))/length(x))
barplot(column_description_unique,las=2)
## All Application>Id are Unique and Outstanding.Balance have all unique values


##---- Application.ID ----
## All application ID are unique hence removing the column
master_data <- subset(master_data,select = -c(Application.ID))
##----Age----
## Age distribution
ggplot(data=master_data, aes(master_data$Age)) + geom_histogram()+theme_nobackground
boxplot(master_data$Age)
quantile(master_data$Age, probs = seq(0, 1, 0.01), na.rm = FALSE,
         names = TRUE, type = 7)
mean(master_data$Age)
## Replacing outliers with Mean
master_data <- master_data[master_data$Age>0,]

## Age distribution wrt performance
densiti1<- ggplot(master_data, aes(x=scale(Age), colour=Performance.Tag,size=0.8)) + geom_density()+theme_nobackground
ggplotly(densiti1)
## Age with lesser than 40 has more chances of default
##---- Gender----
## Shows no significance influence on performance
unique(master_data$Gender)
master_data$Gender <- as.factor(master_data$Gender)
histo1<-ggplot(data=master_data, aes(master_data$Gender)) + geom_histogram(stat = "count")+theme_nobackground
ggplotly(histo1)
## Graph explaination
# length(which(demographic_data$Performance.Tag==0)) 66778
# length(which(demographic_data$Performance.Tag==1)) 2943
# length(which(demographic_data$Gender=="M"&demographic_data$Performance.Tag==0)) 51016
# length(which(demographic_data$Gender=="M"&demographic_data$Performance.Tag==1)) 2226
# (51016*100)/66778 for male
# (2226*100)/2943 for male

plot1<- ggplot(master_data, aes(x= Gender,  group=Performance.Tag)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="Gender") +
  facet_grid(~Performance.Tag) +theme_nobackground

ggplotly(plot1)
##---- Marital.Status..at.the.time.of.application.----
unique(master_data$Marital.Status..at.the.time.of.application.)
master_data$Marital.Status..at.the.time.of.application.=as.factor(master_data$Marital.Status..at.the.time.of.application.)
plot2<- ggplot(data=master_data, aes(master_data$Marital.Status..at.the.time.of.application.)) +
  geom_histogram(stat = "count")+theme_nobackground

ggplotly(plot2)

## Marital.Status..at.the.time.of.application. distribution wrt performance
plot3<- ggplot(master_data, aes(x= Marital.Status..at.the.time.of.application.,  group=Performance.Tag)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="Marital.Status") +
  facet_grid(~Performance.Tag) +theme_nobackground

ggplotly(plot3)

##---- Income ----
plot4<- ggplot(data=master_data, aes(master_data$Income)) + geom_histogram()+theme_nobackground
ggplotly(plot4)

boxplot(master_data$Income)
quantile(master_data$Income, probs = seq(0, 1, 0.01), na.rm = FALSE,
         names = TRUE, type = 7)
master_data[which(master_data$Age<4.5),'Income']=4.5


## Income distribution wrt performance
plot5<-ggplot(master_data, aes(x=Income, colour=Performance.Tag,size=0.8)) + geom_density()+theme_nobackground
ggplotly(plot5)

##----No.of.dependents----
## No of dependents more than 3 are more likely to default
unique(master_data$No.of.dependents)
master_data$No.of.dependents <- as.factor(master_data$No.of.dependents)
histo2<- ggplot(data=master_data, aes(master_data$No.of.dependents)) + geom_histogram(stat = "count")+theme_nobackground
ggplotly(histo2)
## No.of.dependents distribution wrt performance

plot6<- ggplot(master_data, aes(x= No.of.dependents,  group=Performance.Tag)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="No.of.dependents") +
  facet_grid(~Performance.Tag) +theme_nobackground
ggplotly(plot6)

##----Education----
  unique(master_data$Education)
master_data$Education <- as.factor(master_data$Education)
histo3<- ggplot(data=master_data, aes(master_data$Education)) + geom_histogram(stat = "count")+theme_nobackground
ggplotly(histo3)
## Education distribution wrt performance

plot7<- ggplot(master_data, aes(x= Education,  group=Performance.Tag)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="Education") +
  facet_grid(~Performance.Tag) +theme_nobackground
ggplotly(plot7)

##---- Profession ----
unique(master_data$Profession)
master_data$Profession <- as.factor(master_data$Profession)
histo4<- ggplot(data=master_data, aes(master_data$Profession)) + geom_histogram(stat = "count")+theme_nobackground
ggplotly(histo4)
## Profession distribution wrt performance
plot8<- ggplot(master_data, aes(x= Profession,  group=Performance.Tag)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="Profession") +
  facet_grid(~Performance.Tag) +theme_nobackground
ggplotly(plot8)

##---- Type.of.residence----
unique(master_data$Type.of.residence)
master_data$Type.of.residence <- as.factor(master_data$Type.of.residence)
histo5<- ggplot(data=master_data, aes(master_data$Type.of.residence)) + geom_histogram(stat = "count")+theme_nobackground
ggplotly(histo5)

## Type.of.residence distribution wrt performance
plot9<- ggplot(master_data, aes(x= Type.of.residence,  group=Performance.Tag)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="Type.of.residence") +
  facet_grid(~Performance.Tag) +theme_nobackground
ggplotly(plot8)

##----No.of.months.in.current.residence ----
unique(master_data$No.of.months.in.current.residence)

histo6<- ggplot(data=master_data, aes(master_data$No.of.months.in.current.residence)) + geom_histogram()+theme_nobackground
ggplotly(histo6)
boxplot(master_data$No.of.months.in.current.residence)
quantile(master_data$No.of.months.in.current.residence, probs = seq(0, 1, 0.01), na.rm = FALSE,
         names = TRUE, type = 7)
## years distribution wrt performance
plot10<-ggplot(master_data, aes(x=No.of.months.in.current.residence, colour=Performance.Tag,size=0.8)) + 
  geom_density()+theme_nobackground
ggplotly(plot10)

##----No.of.months.in.current.company----
unique(master_data$No.of.months.in.current.company)
histo7<- ggplot(data=master_data, aes(master_data$No.of.months.in.current.company)) + geom_histogram()+theme_nobackground
ggplotly(histo7)
boxplot(master_data$No.of.months.in.current.company)
quantile(master_data$No.of.months.in.current.company, probs = seq(0, 1, 0.01), na.rm = FALSE,
         names = TRUE, type = 7)
plot11<-ggplot(master_data, aes(x=No.of.months.in.current.company, colour=Performance.Tag,size=.8)) + 
  geom_density()+theme_nobackground

ggplotly(plot11)

##---- Presence.of.open.auto.loan ----
unique(master_data$Presence.of.open.auto.loan)
master_data$Presence.of.open.auto.loan <- as.factor(master_data$Presence.of.open.auto.loan)

histo8<-ggplot(data=master_data, aes(master_data$Presence.of.open.auto.loan)) + 
  geom_histogram(stat = "count")+theme_nobackground
ggplotly(histo8)

## Gender distribution wrt performance
plot12<- ggplot(master_data, aes(x= Presence.of.open.auto.loan,  group=Performance.Tag)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="Presence.of.open.auto.loan") +
  facet_grid(~Performance.Tag) +theme_nobackground

ggplotly(plot12)

##---- Total.No.of.Trades ----
histo9<- ggplot(data=master_data, aes(master_data$Total.No.of.Trades)) + geom_histogram()+theme_nobackground
ggplotly(histo9)
boxplot(master_data$Total.No.of.Trades)
quantile(master_data$Total.No.of.Trades, probs = seq(0, 1, 0.01), na.rm = FALSE,
         names = TRUE, type = 7)

plot13<- ggplot(master_data, aes(x=Total.No.of.Trades, colour=Performance.Tag,size=0.8)) + 
    geom_density()+theme_nobackground
ggplotly(plot13)

##---- Outstanding.Balance ----
histo10<- ggplot(data=master_data, aes(master_data$Outstanding.Balance)) + geom_histogram()+theme_nobackground
ggplotly(histo10)
boxplot(master_data$Outstanding.Balance)
quantile(master_data$Outstanding.Balance, probs = seq(0, 1, 0.01), na.rm = FALSE,
         names = TRUE, type = 7)

## Outstanding.Balance distribution wrt performance
plot14<- ggplot(master_data, aes(x=Outstanding.Balance, colour=Performance.Tag)) + geom_density()+theme_nobackground
ggplotly(plot14)

##---- Presence.of.open.home.loan ----
unique(master_data$Presence.of.open.home.loan)
master_data$Presence.of.open.home.loan <- as.factor(master_data$Presence.of.open.home.loan)
histo11<- ggplot(data=master_data, aes(master_data$Presence.of.open.home.loan)) +
  geom_histogram(stat = "count")+theme_nobackground
ggplotly(histo11)
## Presence.of.open.home.loan distribution wrt performance


plot15<- ggplot(master_data, aes(x= Presence.of.open.home.loan,  group=Performance.Tag)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="Presence.of.open.home.loan") +
  facet_grid(~Performance.Tag) +theme_nobackground
ggplotly(plot15)

##---- No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. ----
histo12<- ggplot(data=master_data, aes(master_data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.)) + geom_histogram()+theme_nobackground
ggplotly(histo12)
boxplot(master_data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.)
quantile(master_data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans., probs = seq(0, 1, 0.01), na.rm = FALSE,
         names = TRUE, type = 7)
plot16<- ggplot(master_data, aes(x=No.of.Inquiries.in.last.12.months..excluding.home...auto.loans., colour=Performance.Tag)) + geom_density()+theme_nobackground
ggplotly(plot16)

##----No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. ----
histo13<- ggplot(data=master_data, aes(master_data$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.)) + geom_histogram()+theme_nobackground
ggplotly(histo13)
boxplot(master_data$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.)
quantile(master_data$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans., probs = seq(0, 1, 0.01), na.rm = FALSE,
         names = TRUE, type = 7)
plot17<- ggplot(master_data, aes(x=No.of.Inquiries.in.last.6.months..excluding.home...auto.loans., colour=Performance.Tag,size=0.8)) + 
  geom_density()+theme_nobackground
ggplotly(plot17)
##----No.of.PL.trades.opened.in.last.12.months----
unique(master_data$No.of.PL.trades.opened.in.last.12.months)
histo14<-ggplot(data=master_data, aes(master_data$No.of.PL.trades.opened.in.last.12.months)) + geom_histogram()+theme_nobackground
ggplotly(histo14)
boxplot(master_data$No.of.PL.trades.opened.in.last.12.months)
quantile(master_data$No.of.PL.trades.opened.in.last.12.months, probs = seq(0, 1, 0.01), na.rm = FALSE,
         names = TRUE, type = 7)
plot18<- ggplot(master_data, aes(x= No.of.PL.trades.opened.in.last.12.months,  group=Performance.Tag)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="No.of.PL.trades.opened.in.last.12.months") +
  facet_grid(~Performance.Tag) +theme_nobackground
ggplotly(plot18)
##----No.of.PL.trades.opened.in.last.6.months----
unique(master_data$No.of.PL.trades.opened.in.last.6.months)
histo15<-ggplot(data=master_data, aes(master_data$No.of.PL.trades.opened.in.last.6.months)) + geom_histogram()+theme_nobackground
ggplotly(histo15)
boxplot(master_data$No.of.PL.trades.opened.in.last.6.months)
quantile(master_data$No.of.PL.trades.opened.in.last.6.months, probs = seq(0, 1, 0.01), na.rm = FALSE,
         names = TRUE, type = 7)
plot19<- ggplot(master_data, aes(x= No.of.PL.trades.opened.in.last.6.months,  group=Performance.Tag)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="No.of.PL.trades.opened.in.last.6.months") +
  facet_grid(~Performance.Tag) +theme_nobackground
ggplotly(plot19)

##---- No.of.trades.opened.in.last.12.months----

histo16<- ggplot(data=master_data, aes(master_data$No.of.trades.opened.in.last.12.months)) + geom_histogram()+theme_nobackground
ggplotly(histo16)
boxplot(master_data$No.of.trades.opened.in.last.12.months)
quantile(master_data$No.of.trades.opened.in.last.12.months, probs = seq(0, 1, 0.01), na.rm = FALSE,
         names = TRUE, type = 7)
plot20<-ggplot(master_data, aes(x=No.of.trades.opened.in.last.12.months, colour=Performance.Tag)) + 
  geom_density()+theme_nobackground
ggplotly(plot20)

##---- No.of.trades.opened.in.last.6.months -----
histo17<- ggplot(data=master_data, aes(master_data$No.of.trades.opened.in.last.6.months)) + geom_histogram()+theme_nobackground
ggplotly(histo17)
boxplot(master_data$No.of.trades.opened.in.last.6.months)
quantile(master_data$No.of.trades.opened.in.last.6.months, probs = seq(0, 1, 0.01), na.rm = FALSE,
         names = TRUE, type = 7)
plot21<- ggplot(master_data, aes(x= No.of.trades.opened.in.last.6.months,  group=Performance.Tag)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="No.of.trades.opened.in.last.6.months") +
  facet_grid(~Performance.Tag) +theme_nobackground
ggplotly(plot21)
##---- Avgas.CC.Utilization.in.last.12.months ----
unique(master_data$Avgas.CC.Utilization.in.last.12.months)
quantile(master_data$Avgas.CC.Utilization.in.last.12.months, probs = seq(0, 1, 0.01), na.rm = FALSE,
         names = TRUE, type = 7)
boxplot(master_data$Avgas.CC.Utilization.in.last.12.months)
master_data <- master_data[master_data$Avgas.CC.Utilization.in.last.12.months!=113,]
plot22<- ggplot(master_data, aes(x=Avgas.CC.Utilization.in.last.12.months, colour=Performance.Tag,size=0.8)) + geom_density()+theme_nobackground
ggplotly(plot22)

##----No.of.times.30.DPD.or.worse.in.last.12.months----

unique(master_data$No.of.times.30.DPD.or.worse.in.last.12.months)
quantile(master_data$No.of.times.30.DPD.or.worse.in.last.12.months, probs = seq(0, 1, 0.01), na.rm = FALSE,
         names = TRUE, type = 7)
plot23<- ggplot(master_data, aes(x= No.of.times.30.DPD.or.worse.in.last.12.months,  group=Performance.Tag)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="No.of.times.30.DPD.or.worse.in.last.12.months") +
  facet_grid(~Performance.Tag) +theme_nobackground

ggplotly(plot23)
##----No.of.times.30.DPD.or.worse.in.last.6.months----
unique(master_data$No.of.times.30.DPD.or.worse.in.last.6.months)
quantile(master_data$No.of.times.30.DPD.or.worse.in.last.6.months, probs = seq(0, 1, 0.01), na.rm = FALSE,
         names = TRUE, type = 7)

plot24<- ggplot(master_data, aes(x= No.of.times.30.DPD.or.worse.in.last.6.months,  group=Performance.Tag)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="No.of.times.30.DPD.or.worse.in.last.6.months") +
  facet_grid(~Performance.Tag) +theme_nobackground

ggplotly(plot24)
##---- No.of.times.60.DPD.or.worse.in.last.12.months ----
unique(master_data$No.of.times.60.DPD.or.worse.in.last.12.months)
quantile(master_data$No.of.times.60.DPD.or.worse.in.last.12.months, probs = seq(0, 1, 0.01), na.rm = FALSE,
         names = TRUE, type = 7)
plot25<- ggplot(master_data, aes(x= No.of.times.60.DPD.or.worse.in.last.12.months,  group=Performance.Tag)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="No.of.times.60.DPD.or.worse.in.last.12.months") +
  facet_grid(~Performance.Tag) +theme_nobackground
ggplotly(plot25)

##---- No.of.times.60.DPD.or.worse.in.last.6.months ----
unique(master_data$No.of.times.60.DPD.or.worse.in.last.6.months)
quantile(master_data$No.of.times.60.DPD.or.worse.in.last.6.months, probs = seq(0, 1, 0.01), na.rm = FALSE,
         names = TRUE, type = 7)
plot26<- ggplot(master_data, aes(x= No.of.times.60.DPD.or.worse.in.last.6.months,  group=Performance.Tag)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="No.of.times.60.DPD.or.worse.in.last.6.months") +
  facet_grid(~Performance.Tag) +theme_nobackground
ggplotly(plot26)

## ----No.of.times.90.DPD.or.worse.in.last.12.months----
unique(master_data$No.of.times.90.DPD.or.worse.in.last.12.months)
quantile(master_data$No.of.times.90.DPD.or.worse.in.last.12.months, probs = seq(0, 1, 0.01), na.rm = FALSE,
         names = TRUE, type = 7)
plot27<- ggplot(master_data, aes(x= No.of.times.90.DPD.or.worse.in.last.12.months,  group=Performance.Tag)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="No.of.times.90.DPD.or.worse.in.last.12.months") +
  facet_grid(~Performance.Tag) +theme_nobackground

ggplotly(plot27)
## ----No.of.times.90.DPD.or.worse.in.last.6.months----
unique(master_data$No.of.times.90.DPD.or.worse.in.last.6.months)
quantile(master_data$No.of.times.90.DPD.or.worse.in.last.6.months, probs = seq(0, 1, 0.01), na.rm = FALSE,
         names = TRUE, type = 7)
plot28<- ggplot(master_data, aes(x= No.of.times.90.DPD.or.worse.in.last.6.months,  group=Performance.Tag)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="No.of.times.90.DPD.or.worse.in.last.6.months") +
  facet_grid(~Performance.Tag) +theme_nobackground
ggplotly(plot28)

##---- Write to a file----
write.csv(master_data, file = "Master_Data.csv",row.names=FALSE)
####################################################################
##----Logistic Reegression ----
install.packages("caret", dependencies = c("Depends", "Suggests"))
credit_card <- read.csv('Master_Data.csv')

##---- Normalising continuous features ----
credit_card$Age<- scale(credit_card$Age) 
credit_card$No.of.dependents<- scale(credit_card$No.of.dependents) 
credit_card$Income<- scale(credit_card$Income)
credit_card$No.of.months.in.current.residence<- scale(credit_card$No.of.months.in.current.residence)
credit_card$No.of.months.in.current.company<- scale(credit_card$No.of.months.in.current.company)
credit_card$No.of.times.30.DPD.or.worse.in.last.12.months<- scale(credit_card$No.of.times.30.DPD.or.worse.in.last.12.months)
credit_card$No.of.times.30.DPD.or.worse.in.last.6.months<- scale(credit_card$No.of.times.30.DPD.or.worse.in.last.6.months)
credit_card$No.of.times.60.DPD.or.worse.in.last.12.months<- scale(credit_card$No.of.times.60.DPD.or.worse.in.last.12.months)
credit_card$No.of.times.60.DPD.or.worse.in.last.6.months<- scale(credit_card$No.of.times.60.DPD.or.worse.in.last.6.months)
credit_card$No.of.times.90.DPD.or.worse.in.last.12.months<- scale(credit_card$No.of.times.90.DPD.or.worse.in.last.12.months)
credit_card$No.of.times.90.DPD.or.worse.in.last.6.months<- scale(credit_card$No.of.times.90.DPD.or.worse.in.last.6.months)
credit_card$No.of.trades.opened.in.last.12.months<- scale(credit_card$No.of.trades.opened.in.last.12.months)
credit_card$No.of.trades.opened.in.last.6.months<- scale(credit_card$No.of.trades.opened.in.last.6.months)
credit_card$No.of.PL.trades.opened.in.last.12.months<- scale(credit_card$No.of.PL.trades.opened.in.last.12.months)
credit_card$No.of.PL.trades.opened.in.last.6.months<- scale(credit_card$No.of.PL.trades.opened.in.last.6.months)
credit_card$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.<- scale(credit_card$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.)
credit_card$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.<- scale(credit_card$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.)
credit_card$Outstanding.Balance<- scale(credit_card$Outstanding.Balance)
credit_card$Total.No.of.Trades<- scale(credit_card$Total.No.of.Trades)
credit_card$Avgas.CC.Utilization.in.last.12.months<- scale(credit_card$Avgas.CC.Utilization.in.last.12.months)


bad <- sum(credit_card$Performance.Tag)/nrow(credit_card)
## 31.1% bad

##---- Categorical Columns ----

colnames(credit_card)
credit_card_chr<- credit_card[,-c(1,4,5,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,25,26)]

dummies<- data.frame(sapply(credit_card_chr, 
                            function(x) data.frame(model.matrix(~x-1,data =credit_card_chr))[,-1]))

credit_card_final<- cbind(credit_card[,c(1,4,5,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28)],dummies) 
write.csv(credit_card_final, file = "Model_Data.csv",row.names=FALSE)
str(credit_card_final) #70021 obs. of  35 variables
##----splitting the data between train and test ----

credit_card_final <- read.csv('Model_Data.csv')
set.seed(100)
library(caTools)
indices = sample.split(credit_card_final$Performance.Tag, SplitRatio = 0.7)

train = credit_card_final[indices,]

test = credit_card_final[!(indices),]
str(train)


##----Logistic Regression Model Building ----

model_1 = glm(Performance.Tag ~ ., data = train, family = "binomial")
summary(model_1)


# Stepwise selection
library("MASS")
model_2<- stepAIC(model_1, direction="both")
summary(model_2)

model_3<- glm(formula = Performance.Tag ~ Income + No.of.months.in.current.company +  
                No.of.times.30.DPD.or.worse.in.last.6.months + No.of.times.30.DPD.or.worse.in.last.12.months + No.of.trades.opened.in.last.12.months + 
                No.of.PL.trades.opened.in.last.12.months + Type.of.residence.xOthers + No.of.months.in.current.residence + 
                No.of.times.60.DPD.or.worse.in.last.6.months + No.of.times.60.DPD.or.worse.in.last.12.months + Avgas.CC.Utilization.in.last.12.months + 
                No.of.PL.trades.opened.in.last.6.months + No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + Total.No.of.Trades + 
                Presence.of.open.auto.loan , family = "binomial", data = train) 

summary(model_3) 

vif(model_3) 


# Excluding No.of.trades.opened.in.last.12.months 
model_4<- glm(formula = Performance.Tag ~ Income + No.of.months.in.current.company +  
                No.of.times.30.DPD.or.worse.in.last.6.months + No.of.times.30.DPD.or.worse.in.last.12.months + 
                No.of.PL.trades.opened.in.last.12.months + Type.of.residence.xOthers + No.of.months.in.current.residence + 
                No.of.times.60.DPD.or.worse.in.last.6.months + No.of.times.60.DPD.or.worse.in.last.12.months + Avgas.CC.Utilization.in.last.12.months + 
                No.of.PL.trades.opened.in.last.6.months + No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + Total.No.of.Trades + 
                Presence.of.open.home.loan, family = "binomial", data = train) 
final_model<- model_4
##---- Model Evaluation ----
test_pred = predict(final_model, type = "response", 
                    newdata = test[,-c(23)])
test_pred_performance.tag.y <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_performance.tag.y <- factor(ifelse(test$Performance.Tag==1,"Yes","No"))
View(test_pred_performance.tag.y)
View(test_actual_performance.tag.y)
test$Predicted_Performance<-test_pred_performance.tag.y
test$Actual_Performance<-test_actual_performance.tag.y


conf_final <- table(test_pred_performance.tag.y, test_actual_performance.tag.y)

accuracy<-sum(diag(conf_final)/sum(conf_final))
sensitivity<-5083/(5083+799)
specificity<-13743/(13743+461)
View(accuracy)
View(sensitivity)
View(specificity)

library(caret)


conf_final<-confusionMatrix(test_pred_performance.tag.y, test_actual_performance.tag.y)
View(conf_final)

specificity(test_actual_performance.tag.y,test_pred_performance.tag.y,cutoff=0.5)

sensitivity(test_actual_performance.tag.y,test_pred_performance.tag.y,cutoff=0.5)

test_cutoff_churn <- ifelse(test_pred_performance.tag.y=="Yes",1,0)
test_actual_churn <- ifelse(test_actual_performance.tag.y=="Yes",1,0)


library(ROCR)
#on testing  data
pred_object_test<- prediction(test_cutoff_churn, test_actual_churn)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)


# Lift & Gain Chart 

# plotting the lift chart

# Loading dplyr package 
require(dplyr)
library(dplyr)

lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

Churn_decile = lift(test_actual_churn, test_pred, groups = 10)

####################################################################
##----Random Forest -----
install.packages("randomForest")
library(randomForest)
credit_card <- read.csv('Model_Data.csv')
##---- Split Data into Test and Train ----
set.seed(100)
library(caTools)
indices = sample.split(credit_card$Performance.Tag, SplitRatio = 0.7)

train = credit_card[indices,]

test = credit_card[!(indices),]
str(train)
##---- Random Forest Model Building ----

random_forest <- randomForest(Performance.Tag ~ .,
                              data=train,
                              ntree=500,
                              importance=T)

## Variable importance plot

varImpPlot(random_forest,
           sort = T,
           main="Variable Importance",
           n.var=5)

# Variable Importance Table
var.imp <- data.frame(importance(random_forest,
                                 type=2))
# make row names as columns
var.imp$Variables <- row.names(var.imp)
var.imp[order(var.imp$IncNodePurity,decreasing = T),]

## Important colums and their influence

# No.of.times.90.DPD.or.worse.in.last.12.months                    4528.5921671
# No.of.times.60.DPD.or.worse.in.last.6.months                     1634.3235572
# No.of.times.90.DPD.or.worse.in.last.6.months                     1331.5532553
# No.of.times.30.DPD.or.worse.in.last.6.months                     1052.3106633
# No.of.times.60.DPD.or.worse.in.last.12.months                     422.0654197
# No.of.times.30.DPD.or.worse.in.last.12.months                     303.4961611
# Avgas.CC.Utilization.in.last.12.months                            206.1707204
# Outstanding.Balance                                               120.0791910
# No.of.PL.trades.opened.in.last.12.months                          101.7240929
# No.of.months.in.current.company                                    80.8825667 
# Income                                                             78.2607339


##---- Model Evaluation ----
test$predicted.response <- predict(random_forest ,test)
saveRDS(random_forest,"Random_Forest.rds")
## random_forest <- readRDS("Random_Forest.rds")
## ROC curve

library(caret)
require(ROCR)

perf<-performance( prediction(test$predicted.response ,test$Performance.Tag) ,"tpr", "fpr")
plot(perf)
auc<- performance(prediction(test$predicted.response ,test$Performance.Tag),"auc")
auc@y.values


opt.cut = function(perf, pred){
  cut.ind = mapply(FUN=function(x, y, p){
    d = (x - 0)^2 + (y-1)^2
    ind = which(d == min(d))
    c(sensitivity = y[[ind]], specificity = 1-x[[ind]], 
      cutoff = p[[ind]])
  }, perf@x.values, perf@y.values, pred@cutoffs)
}
print(opt.cut(perf, prediction(test$predicted.response ,test$Performance.Tag)))

# Accuracy 0.9734193
# sensitivity 0.9333132
# specificity 0.9930449
# cutoff      0.1820667

test$predicted.response <- factor(ifelse(test$predicted.response >= 0.182, 1, 0))
confusionMatrix(test$Performance.Tag, test$predicted.response)

##      Accuracy : 0.9742 
##      Sensitivity : 0.9700         
##      Specificity : 0.9841 


##---- Random Forest Model Without Credit Card Data ----

credit_card_demographic <- credit_card[,c("Age","No.of.dependents","Income",
                                          "No.of.months.in.current.residence","No.of.months.in.current.company",
                                          "Gender","Marital.Status..at.the.time.of.application.",
                                          "Education.xMasters","Education.xOthers","Education.xPhd",
                                          "Education.xProfessional","Profession.xSE","Profession.xSE_PROF",
                                          "Type.of.residence.xLiving.with.Parents","Type.of.residence.xOthers",
                                          "Type.of.residence.xOwned","Type.of.residence.xRented","No.of.months.in.current.residence",
                                          "No.of.months.in.current.company","Performance.Tag")]
##---- Split Data into Test and Train ----
set.seed(100)
library(caTools)
indices = sample.split(credit_card_demographic$Performance.Tag, SplitRatio = 0.7)

train = credit_card_demographic[indices,]

test = credit_card_demographic[!(indices),]
##---- Model Building -----
random_forest <- randomForest(Performance.Tag ~ .,
                              data=train,
                              ntree=500,
                              importance=T)
saveRDS(random_forest,"Random_Forest_Demographic_Model.rds")
test$predicted.response <- predict(random_forest ,test)
test$predicted.response <- factor(ifelse(test$predicted.response >= 0.15, 1, 0))
confusionMatrix(test$Performance.Tag, test$predicted.response)

# Accuracy : 0.6054 
# Sensitivity : 0.9052          
# Specificity : 0.4384




#####################################################################
##---- Decision Tree -----
library(rpart)

credit_card <- read.csv('Model_Data.csv')
##---- Split Data into Test and Train ----
set.seed(100)
library(caTools)
indices = sample.split(credit_card$Performance.Tag, SplitRatio = 0.7)

train = credit_card[indices,]

test = credit_card[!(indices),]
##---- Model Building -----
decision_tree <- rpart(Performance.Tag ~ ., data=train, method="anova") 
printcp(decision_tree) # display the results 
plotcp(decision_tree) # visualize cross-validation results 
summary(decision_tree) # detailed summary of splits

test$predicted.response <- predict(decision_tree,test)

test$predicted.response <- factor(ifelse(test$predicted.response >= 0.182, 1, 0))
confusionMatrix(test$Performance.Tag, test$predicted.response)

##      Accuracy : 0.9742 
##      Sensitivity : 0.9697         
##      Specificity : 1.0000 

##---- Model Building with Demographic Data ----

credit_card_demographic <- credit_card[,c("Age","No.of.dependents","Income",
                                          "No.of.months.in.current.residence","No.of.months.in.current.company",
                                          "Gender","Marital.Status..at.the.time.of.application.",
                                          "Education.xMasters","Education.xOthers","Education.xPhd",
                                          "Education.xProfessional","Profession.xSE","Profession.xSE_PROF",
                                          "Type.of.residence.xLiving.with.Parents","Type.of.residence.xOthers",
                                          "Type.of.residence.xOwned","Type.of.residence.xRented","No.of.months.in.current.residence",
                                          "No.of.months.in.current.company","Performance.Tag")]
set.seed(100)
library(caTools)
indices = sample.split(credit_card_demographic$Performance.Tag, SplitRatio = 0.7)

train = credit_card_demographic[indices,]

test = credit_card_demographic[!(indices),]
decision_tree <- rpart(Performance.Tag ~ ., data=train, method="anova") 

printcp(decision_tree) # display the results 
plotcp(decision_tree) # visualize cross-validation results 
summary(decision_tree) # detailed summary of splits

test$predicted.response <- predict(decision_tree,test)

test$predicted.response <- factor(ifelse(test$predicted.response >= 0.15, 1, 0))
confusionMatrix(test$Performance.Tag, test$predicted.response)



# Accuracy : 0.678
# Sensitivity : 0.8825          
# Specificity : 0.4939
