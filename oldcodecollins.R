setwd('C:/Users/CAM/Documents/MSBA/Fall/ST 541/Final Project/Movie_R_Project')
library(readr)
library(leaps)
library("stringr")
library(dplyr)

#USAMOVIES <- read_csv("./USAMOVIES.csv")
ratings <- read_csv("IMDb ratings.csv")
USAMOVIES <- read_csv("USAMOVIES_INFL.csv")
USAMOVIES <- select(USAMOVIES, -c("X1","Unnamed: 0","X","Unnamed..0"))

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
#detach(USAMOVIESratings)

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
fullmodprofit<- lm(profit~ year+ duration +avg.vote+votes+reviews.from.users+reviews.from.critics+IsItHorror+IsItRomance+IsItAction+IsItComedy+IsItDrama+mean.vote+median.vote+males.allages.avg.vote+males.allages.votes+females.allages.avg.vote+females.allages.votes+top1000.voters.rating+top1000.voters.votes+us.voters.rating+us.voters.votes+non.us.voters.rating+non.us.voters.votes, data=USAMOVIESratings)
summary(fullmodprofit)
fullmodprofitinfl<- lm(profit.infl~ year+ duration +avg.vote+votes+reviews.from.users+reviews.from.critics+IsItHorror+IsItRomance+IsItAction+IsItComedy+IsItDrama+mean.vote+median.vote+males.allages.avg.vote+males.allages.votes+females.allages.avg.vote+females.allages.votes+top1000.voters.rating+top1000.voters.votes+us.voters.rating+us.voters.votes+non.us.voters.rating+non.us.voters.votes, data=USAMOVIESratings)
summary(fullmodprofitinfl)

#####get to work
allinputs<-cbind(year,duration,avg.vote,votes,reviews.from.users,reviews.from.critics,IsItHorror,IsItRomance,IsItAction,IsItComedy,IsItDrama,weighted.average.vote,total.votes,mean.vote,median.vote,males.allages.avg.vote,males.allages.votes,females.allages.avg.vote,females.allages.votes,top1000.voters.rating,top1000.voters.votes,us.voters.rating,us.voters.votes,non.us.voters.rating,non.us.voters.votes, data=USAMOVIESratings)
best <- regsubsets(as.matrix(allinputs), profit)
summary(best)


###BACK AIC
##no inflation
backAIC <- step(fullmodprofit,direction="backward", data=USAMOVIESratings)
backAIC$coefficients
backAICmodel<-lm(profit ~ year + duration + avg.vote + votes + reviews.from.users + 
                   reviews.from.critics + IsItHorror + IsItRomance + IsItAction + 
                   IsItDrama + mean.vote + males.allages.votes + females.allages.avg.vote + 
                   top1000.voters.rating + top1000.voters.votes + us.voters.rating + 
                   us.voters.votes + non.us.voters.rating + non.us.voters.votes, data=USAMOVIESratings)
#not taking ratings into the model #profit ~ year + duration + votes + reviews.from.users + reviews.from.critics + IsItHorror + IsItComedy + IsItDrama)
summary(backAICmodel)
#AIC= 204280.2
#R^2= 0.5195  


##inflation
backAICinfl <- step(fullmodprofitinfl,direction="backward", data=USAMOVIESratings)
backAIC$coefficients
backAICmodelinfl<- lm(profit.infl ~ year + duration + avg.vote + votes + reviews.from.users + 
                        reviews.from.critics + IsItHorror + IsItAction + IsItComedy + 
                        IsItDrama + mean.vote + males.allages.votes + females.allages.avg.vote + 
                        females.allages.votes + top1000.voters.rating + top1000.voters.votes + 
                        us.voters.rating + us.voters.votes + non.us.voters.rating, data=USAMOVIESratings)
summary(backAICmodelinfl)
#AIC= 210847.5
#R^2= 0.3937 




###BACK BIC
n <- length(fullmodprofit$residuals)
ninfl <- length(fullmodprofitinfl$residuals)
##no inflation
backBIC <- step(fullmodprofit,direction="backward", data=USAMOVIESratings, k=log(n))
backBIC$coefficients
backBICmodel<-lm(profit ~ avg.vote + votes + reviews.from.users + reviews.from.critics + 
                   IsItHorror + IsItDrama + mean.vote + males.allages.votes + 
                   females.allages.avg.vote + top1000.voters.votes + us.voters.votes + 
                   non.us.voters.votes, data=USAMOVIESratings)
#not taking ratings into the model#lm(profit ~ votes + reviews.from.users + reviews.from.critics + IsItHorror + IsItDrama)
summary(backBICmodel)
#AIC= 204379.1
#R^2= 0.5178  

##inflation
backBICinfl <- step(fullmodprofitinfl,direction="backward", data=USAMOVIESratings, k=log(ninfl))
backBICinfl$coefficients
backBICmodelinfl<-lm(profit.infl ~ year + duration + avg.vote + votes + reviews.from.users + 
                       reviews.from.critics + IsItHorror + IsItDrama + mean.vote + 
                       males.allages.votes + females.allages.avg.vote + females.allages.votes + 
                       top1000.voters.votes + us.voters.votes, data=USAMOVIESratings)
summary(backBICmodelinfl)
#AIC= 210951.8
#R^2=  0.3927  


###FORWARD AIC 
intercept <- lm(profit~1,data=USAMOVIESratings)
interceptinfl <- lm(profit.infl~1,data=USAMOVIESratings)

forwardAIC <- step(intercept,scope=list(lower=~1,upper=~year+ duration +avg.vote+votes+reviews.from.users+reviews.from.critics+IsItHorror+IsItRomance+IsItAction+IsItComedy+IsItDrama+mean.vote+median.vote+males.allages.avg.vote+males.allages.votes+females.allages.avg.vote+females.allages.votes+top1000.voters.rating+top1000.voters.votes+us.voters.rating+us.voters.votes+non.us.voters.rating+non.us.voters.votes),direction="forward", data=USAMOVIESratings)
forwardAIC$coefficients
forwardAICmodel<-lm(profit ~ votes + reviews.from.users + males.allages.votes + reviews.from.critics + 
                      IsItDrama + IsItHorror + IsItAction + top1000.voters.votes + 
                      us.voters.votes + non.us.voters.votes + year + duration + 
                      non.us.voters.rating + females.allages.avg.vote + us.voters.rating + 
                      mean.vote + top1000.voters.rating + avg.vote + IsItRomance,data=USAMOVIESratings)
summary(forwardAICmodel)

#AIC= 204280.2
#R^2=0.5195 

forwardAICinfl <- step(interceptinfl,scope=list(lower=~1,upper=~year+ duration +avg.vote+votes+reviews.from.users+reviews.from.critics+IsItHorror+IsItRomance+IsItAction+IsItComedy+IsItDrama+mean.vote+median.vote+males.allages.avg.vote+males.allages.votes+females.allages.avg.vote+females.allages.votes+top1000.voters.rating+top1000.voters.votes+us.voters.rating+us.voters.votes+non.us.voters.rating+non.us.voters.votes),direction="forward", data=USAMOVIESratings)
forwardAICinfl$coefficients
forwardAICmodelinfl<-lm(profit.infl ~ votes + males.allages.votes + top1000.voters.votes + 
                          non.us.voters.votes + IsItDrama + duration + year + reviews.from.users + 
                          IsItHorror + reviews.from.critics + us.voters.votes + non.us.voters.rating + 
                          females.allages.avg.vote + females.allages.votes + IsItComedy + 
                          avg.vote + mean.vote + top1000.voters.rating + IsItAction + 
                          us.voters.rating,data=USAMOVIESratings)
summary(forwardAICmodelinfl)

#AIC= 210849
#R^2=0.3937 


###FORWARD BIC 

forwardBIC <- step(intercept,scope=list(lower=~1,upper=~year+ duration +avg.vote+votes+reviews.from.users+reviews.from.critics+IsItHorror+IsItRomance+IsItAction+IsItComedy+IsItDrama+mean.vote+median.vote+males.allages.avg.vote+males.allages.votes+females.allages.avg.vote+females.allages.votes+top1000.voters.rating+top1000.voters.votes+us.voters.rating+us.voters.votes+non.us.voters.rating+non.us.voters.votes),direction="forward", data=USAMOVIESratings,k=log(n))
forwardBIC$coefficients
forwardBICmodel<-lm(profit ~ votes + reviews.from.users + males.allages.votes + reviews.from.critics + 
                      IsItDrama + IsItHorror + IsItAction + top1000.voters.votes + 
                      us.voters.votes + non.us.voters.votes + year,data=USAMOVIESratings)
summary(forwardBICmodel)

#AIC= 204451.7
#R^2=0.5107 


forwardBICinfl <- step(interceptinfl,scope=list(lower=~1,upper=~year+ duration +avg.vote+votes+reviews.from.users+reviews.from.critics+IsItHorror+IsItRomance+IsItAction+IsItComedy+IsItDrama+mean.vote+median.vote+males.allages.avg.vote+males.allages.votes+females.allages.avg.vote+females.allages.votes+top1000.voters.rating+top1000.voters.votes+us.voters.rating+us.voters.votes+non.us.voters.rating+non.us.voters.votes),direction="forward", data=USAMOVIESratings,k=log(n))
forwardBICinfl$coefficients
forwardBICmodelinfl<-lm(profit.infl ~ votes + males.allages.votes + top1000.voters.votes + 
                          non.us.voters.votes + IsItDrama + duration + year + reviews.from.users + 
                          IsItHorror + reviews.from.critics + us.voters.votes + non.us.voters.rating + 
                          females.allages.avg.vote + females.allages.votes,data=USAMOVIESratings)
summary(forwardBICmodelinfl)

#AIC= 210970.3
#R^2= 0.3906 





