setwd('C:/Users/CAM/Documents/MSBA/Fall/ST 541/Final Project/Movie_R_Project')
library(readr)
library(leaps)
library("stringr")
library(dplyr)

#USAMOVIES <- read_csv("./USAMOVIES.csv")
ratings <- read_csv("IMDb ratings.csv")
USAMOVIES <- read_csv("USAMOVIES_INFL.csv")

names(ratings)<-str_replace_all(names(ratings), c("_" = "." , "," = "" ))
names(USAMOVIES)<-str_replace_all(names(USAMOVIES), c("_" = "." , "," = "" ))
USAMOVIES["IsItHorror"]<- 0
USAMOVIES["IsItRomance"] <- 0
USAMOVIES["IsItAction"] <- 0
USAMOVIES["IsItComedy"] <- 0
USAMOVIES["IsItDrama"] <- 0
USAMOVIES$IsItHorror[grep("Horror",USAMOVIES$genre)] <- 1
USAMOVIES$IsItRomance[grep("Romance",USAMOVIES$genre)] <- 1
USAMOVIES$IsItAction[grep("Action",USAMOVIES$genre)] <- 1
USAMOVIES$IsItComedy[grep("Comedy",USAMOVIES$genre)] <- 1
USAMOVIES$IsItDrama[grep("Drama",USAMOVIES$genre)] <- 1
USAMOVIES$IsItHorror<-as.factor(USAMOVIES$IsItHorror)
USAMOVIES$IsItRomance<-as.factor(USAMOVIES$IsItRomance)
USAMOVIES$IsItAction<-as.factor(USAMOVIES$IsItAction)
USAMOVIES$IsItComedy<-as.factor(USAMOVIES$IsItComedy)
USAMOVIES$IsItDrama<-as.factor(USAMOVIES$IsItDrama)
#USAMOVIES$budget <- substr(USAMOVIES$budget, start = 2, stop = 15)
#USAMOVIES$budget <- as.numeric(USAMOVIES$budget)
#USAMOVIES$usa.gross.income <- substr(USAMOVIES$usa.gross.income, start = 2, stop = 15)
#USAMOVIES$usa.gross.income <- as.numeric(USAMOVIES$usa.gross.income)
#USAMOVIES$worlwide.gross.income <- substr(USAMOVIES$worlwide.gross.income, start = 2, stop = 15)
#USAMOVIES$worlwide.gross.income <- as.numeric(USAMOVIES$worlwide.gross.income)
#USAMOVIES$profit <-(USAMOVIES$worlwide.gross.income- USAMOVIES$budget)


#USAMOVIES<-USAMOVIES[!is.na(USAMOVIES$reviews.from.users)&!is.na(USAMOVIES$reviews.from.critics),]
USAMOVIES <- select(USAMOVIES,-c("metascore"))
USAMOVIESratings <- merge(USAMOVIES,ratings,by="imdb.title.id")
attach(USAMOVIESratings)
detach(USAMOVIESratings)

colnames(USAMOVIESratings)
str(USAMOVIESratings)
summary(USAMOVIES)
dim(USAMOVIES)

#merge USAMOVIES with ratings
#ratings <- select(ratings,c("imdb.title.id","weighted.average.vote","allgenders.0age.avg.vote",
                           # "allgenders.18age.avg.vote","allgenders.30age.avg.vote","allgenders.45age.avg.vote",
                            #"females.allages.avg.vote","males.allages.avg.vote"))







#What is the best way to predict the profit margin of a movie?--Collins
# removed na from reviews
# excluded metascore because of the # of NA
fullmodprofit<- lm(profit~ year+ duration +avg.vote+votes+reviews.from.users+reviews.from.critics+IsItHorror+IsItRomance+IsItAction+IsItComedy+IsItDrama+weighted.average.vote+total.votes+mean.vote+median.vote+votes.10+votes.9+votes.8+votes.7+votes.6+votes.5+votes.4+votes.3+votes.2+votes.1+allgenders.0age.avg.vote+allgenders.0age.votes+allgenders.18age.avg.vote+allgenders.18age.votes+allgenders.30age.avg.vote+allgenders.30age.votes+allgenders.45age.avg.vote+allgenders.45age.votes+males.allages.avg.vote+males.allages.votes+males.0age.avg.vote+males.0age.votes+males.18age.avg.vote+males.18age.votes+males.30age.avg.vote+males.30age.votes+males.45age.avg.vote+males.45age.votes+ females.allages.avg.vote+females.allages.votes+females.0age.avg.vote+females.0age.votes+females.18age.avg.vote+females.18age.votes+females.30age.avg.vote+females.30age.votes+females.45age.avg.vote+females.45age.votes+top1000.voters.rating+top1000.voters.votes+us.voters.rating+us.voters.votes+non.us.voters.rating+non.us.voters.votes, data=USAMOVIESratings)
summary(fullmodprofit)
fullmodprofitinfl<- lm(profit.infl~ year+ duration +avg.vote+votes+reviews.from.users+reviews.from.critics+IsItHorror+IsItRomance+IsItAction+IsItComedy+IsItDrama+weighted.average.vote+total.votes+mean.vote+median.vote+votes.10+votes.9+votes.8+votes.7+votes.6+votes.5+votes.4+votes.3+votes.2+votes.1+allgenders.0age.avg.vote+allgenders.0age.votes+allgenders.18age.avg.vote+allgenders.18age.votes+allgenders.30age.avg.vote+allgenders.30age.votes+allgenders.45age.avg.vote+allgenders.45age.votes+males.allages.avg.vote+males.allages.votes+males.0age.avg.vote+males.0age.votes+males.18age.avg.vote+males.18age.votes+males.30age.avg.vote+males.30age.votes+males.45age.avg.vote+males.45age.votes+ females.allages.avg.vote+females.allages.votes+females.0age.avg.vote+females.0age.votes+females.18age.avg.vote+females.18age.votes+females.30age.avg.vote+females.30age.votes+females.45age.avg.vote+females.45age.votes+top1000.voters.rating+top1000.voters.votes+us.voters.rating+us.voters.votes+non.us.voters.rating+non.us.voters.votes, data=USAMOVIESratings)
summary(fullmodprofitinfl)
allinputs<-cbind(year,duration,avg.vote,votes,reviews.from.users,reviews.from.critics,IsItHorror,IsItRomance,IsItAction,IsItComedy,IsItDrama,weighted.average.vote,total.votes,mean.vote,median.vote,votes.10,votes.9,votes.8,votes.7,votes.6,votes.5,votes.4,votes.3,votes.2,votes.1,allgenders.0age.avg.vote,allgenders.0age.votes,allgenders.18age.avg.vote+allgenders.18age.votes+allgenders.30age.avg.vote,allgenders.30age.votes,allgenders.45age.avg.vote,allgenders.45age.votes,males.allages.avg.vote,males.allages.votes,males.0age.avg.vote,males.0age.votes,males.18age.avg.vote,males.18age.votes,males.30age.avg.vote,males.30age.votes,males.45age.avg.vote,males.45age.votes,females.allages.avg.vote,females.allages.votes,females.0age.avg.vote,females.0age.votes,females.18age.avg.vote,females.18age.votes,females.30age.avg.vote,females.30age.votes,females.45age.avg.vote,females.45age.votes,top1000.voters.rating,top1000.voters.votes,us.voters.rating,us.voters.votes,non.us.voters.rating,non.us.voters.votes)
best <- regsubsets(as.matrix(allinputs), profit)
summary(best)


###BACK AIC
  ##no inflation
  backAIC <- step(fullmodprofit,direction="backward", data=USAMOVIESratings)
  backAIC$coefficients
  backAICmodel<-lm(profit ~ year + duration + votes + reviews.from.users + IsItHorror + IsItRomance + IsItAction + IsItComedy + IsItDrama + votes.10 + votes.9 + votes.8 + votes.7 + votes.6 + votes.4 + votes.3 + votes.2 + allgenders.0age.votes + allgenders.18age.votes + allgenders.30age.votes + allgenders.45age.votes + males.allages.votes + males.0age.votes + males.18age.votes + males.30age.votes + males.45age.avg.vote + males.45age.votes + females.allages.votes + females.0age.votes + females.30age.votes + females.45age.avg.vote + females.45age.votes + top1000.voters.votes + us.voters.votes + non.us.voters.votes)
  #profit ~ year + duration + votes + reviews.from.users + reviews.from.critics + IsItHorror + IsItComedy + IsItDrama)
  summary(backAICmodel)
  #AIC= 139805
  #R^2= 0.6856 
  
  
  ##inflation
  backAICinfl <- step(fullmodprofitinfl,direction="backward", data=USAMOVIESratings)
  backAIC$coefficients
  #backAICmodelinfl<- lm(profit.infl~)
  summary(backAICmodelinfl)
  #AIC= 
  #R^2=

  anova(backAICmodel,backAICmodelinfl)




###BACK BIC
  n <- length(fullmodprofit$residuals)
  ninfl <- length(fullmodprofitinfl$residuals)
  ##no inflation
  backBIC <- step(fullmodprofit,direction="backward", data=USAMOVIESratings, k=log(n))
  backBIC$coefficients
  backBICmodel<-lm(profit ~ duration + votes + reviews.from.users + IsItHorror + IsItAction + IsItDrama + votes.10 + votes.9 + votes.8 + votes.7 + votes.6 + votes.4 + votes.3 + votes.2 + allgenders.0age.votes + allgenders.18age.votes + allgenders.30age.votes + allgenders.45age.votes + males.allages.votes + males.0age.votes + males.18age.votes + males.30age.votes + males.45age.avg.vote + males.45age.votes + females.allages.votes + females.0age.votes + females.30age.votes + females.45age.avg.vote + us.voters.votes + non.us.voters.votes)
  #lm(profit ~ votes + reviews.from.users + reviews.from.critics + IsItHorror + IsItDrama)
  summary(backBICmodel)
  #AIC= 140006.5
  #R^2= 0.6846 
  
  ##inflation
  backBICinfl <- step(fullmodprofit,direction="backward", data=USAMOVIESratings, k=log(ninfl))
  backBICinfl$coefficients
  #backBICmodelinfl<-lm(profit ~ votes + reviews.from.users + reviews.from.critics + IsItHorror + IsItDrama)
  summary(backBICmodelinfl)
  #AIC= 
  #R^2=  

  anova(backBICmodel,backBICmodelinfl)
  
  
###FORWARD AIC 
  intercept <- lm(profit~1,data=USAMOVIESratings)
  interceptinfl <- lm(profit.infl~1,data=USAMOVIESratings)
  
  forwardAIC <- step(intercept,scope=list(lower=~1,upper=~year+duration+avg.vote+votes+reviews.from.users+reviews.from.critics+IsItHorror+IsItRomance+IsItAction+IsItComedy+IsItDrama+weighted.average.vote+total.votes+mean.vote+median.vote+votes.10+votes.9+votes.8+votes.7+votes.6+votes.5+votes.4+votes.3+votes.2+votes.1+allgenders.0age.avg.vote+allgenders.0age.votes+allgenders.18age.avg.vote+allgenders.18age.votes+allgenders.30age.avg.vote+allgenders.30age.votes+allgenders.45age.avg.vote+allgenders.45age.votes+males.allages.avg.vote+males.allages.votes+males.0age.avg.vote+males.0age.votes+males.18age.avg.vote+males.18age.votes+males.30age.avg.vote+males.30age.votes+males.45age.avg.vote+males.45age.votes+ females.allages.avg.vote+females.allages.votes+females.0age.avg.vote+females.0age.votes+females.18age.avg.vote+females.18age.votes+females.30age.avg.vote+females.30age.votes+females.45age.avg.vote+females.45age.votes+top1000.voters.rating+top1000.voters.votes+us.voters.rating+us.voters.votes+non.us.voters.rating+non.us.voters.votes),direction="forward", data=USAMOVIESratings)
  forwardAICmodel<-lm()
  summary(forwardAICmodel)
  forwardAIC$coefficients  
  #AIC= 
  #R^2=
  
  forwardAICinfl <- step(interceptinfl,scope=list(lower=~1,upper=~year+duration+avg.vote+votes+reviews.from.users+reviews.from.critics+IsItHorror+IsItRomance+IsItAction+IsItComedy+IsItDrama+weighted.average.vote+total.votes+mean.vote+median.vote+votes.10+votes.9+votes.8+votes.7+votes.6+votes.5+votes.4+votes.3+votes.2+votes.1+allgenders.0age.avg.vote+allgenders.0age.votes+allgenders.18age.avg.vote+allgenders.18age.votes+allgenders.30age.avg.vote+allgenders.30age.votes+allgenders.45age.avg.vote+allgenders.45age.votes+males.allages.avg.vote+males.allages.votes+males.0age.avg.vote+males.0age.votes+males.18age.avg.vote+males.18age.votes+males.30age.avg.vote+males.30age.votes+males.45age.avg.vote+males.45age.votes+ females.allages.avg.vote+females.allages.votes+females.0age.avg.vote+females.0age.votes+females.18age.avg.vote+females.18age.votes+females.30age.avg.vote+females.30age.votes+females.45age.avg.vote+females.45age.votes+top1000.voters.rating+top1000.voters.votes+us.voters.rating+us.voters.votes+non.us.voters.rating+non.us.voters.votes),direction="forward", data=USAMOVIESratings)
  forwardAICmodelinfl<-lm()
  summary(forwardAICmodelinfl)
  forwardAICinfl$coefficients
  #AIC= 
  #R^2=
  
  
  anova(forwardAICmodel,forwardAICmodelinfl)
  
  
  
###FORWARD BIC 
  
  forwardBIC <- step(intercept,scope=list(lower=~1,upper=~year+duration+avg.vote+votes+reviews.from.users+reviews.from.critics+IsItHorror+IsItRomance+IsItAction+IsItComedy+IsItDrama+weighted.average.vote+total.votes+mean.vote+median.vote+votes.10+votes.9+votes.8+votes.7+votes.6+votes.5+votes.4+votes.3+votes.2+votes.1+allgenders.0age.avg.vote+allgenders.0age.votes+allgenders.18age.avg.vote+allgenders.18age.votes+allgenders.30age.avg.vote+allgenders.30age.votes+allgenders.45age.avg.vote+allgenders.45age.votes+males.allages.avg.vote+males.allages.votes+males.0age.avg.vote+males.0age.votes+males.18age.avg.vote+males.18age.votes+males.30age.avg.vote+males.30age.votes+males.45age.avg.vote+males.45age.votes+ females.allages.avg.vote+females.allages.votes+females.0age.avg.vote+females.0age.votes+females.18age.avg.vote+females.18age.votes+females.30age.avg.vote+females.30age.votes+females.45age.avg.vote+females.45age.votes+top1000.voters.rating+top1000.voters.votes+us.voters.rating+us.voters.votes+non.us.voters.rating+non.us.voters.votes),direction="forward", data=USAMOVIESratings,k=log(n))
  forwardBICmodel<-lm()
  summary(forwardBICmodel)
  forwardBIC$coefficients
  #AIC= 
  #R^2=
  

  forwardBICinfl <- step(interceptinfl,scope=list(lower=~1,upper=~year+duration+avg.vote+votes+reviews.from.users+reviews.from.critics+IsItHorror+IsItRomance+IsItAction+IsItComedy+IsItDrama+weighted.average.vote+total.votes+mean.vote+median.vote+votes.10+votes.9+votes.8+votes.7+votes.6+votes.5+votes.4+votes.3+votes.2+votes.1+allgenders.0age.avg.vote+allgenders.0age.votes+allgenders.18age.avg.vote+allgenders.18age.votes+allgenders.30age.avg.vote+allgenders.30age.votes+allgenders.45age.avg.vote+allgenders.45age.votes+males.allages.avg.vote+males.allages.votes+males.0age.avg.vote+males.0age.votes+males.18age.avg.vote+males.18age.votes+males.30age.avg.vote+males.30age.votes+males.45age.avg.vote+males.45age.votes+ females.allages.avg.vote+females.allages.votes+females.0age.avg.vote+females.0age.votes+females.18age.avg.vote+females.18age.votes+females.30age.avg.vote+females.30age.votes+females.45age.avg.vote+females.45age.votes+top1000.voters.rating+top1000.voters.votes+us.voters.rating+us.voters.votes+non.us.voters.rating+non.us.voters.votes),direction="forward", data=USAMOVIESratings,k=log(n))
  forwardBICmodelinfl<-lm()
  summary(forwardBICmodelinfl)
  forwardBICinfl$coefficients
  #AIC= 
  #R^2=
  
  
  anova(forwardBICmodel,forwardBICmodelinfl)
  
  

