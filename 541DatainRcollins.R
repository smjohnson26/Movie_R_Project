setwd('C:/Users/CAM/Documents/MSBA/Fall/ST 541/Final Project/Movie_R_Project')
library(readr)
library(leaps)
library("stringr")
library(dplyr)
library(ggplot2)
library(ggplot)
library(car)
library(moments)



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
#merge USAMOVIES with ratings
ratings <- select(ratings,c("imdb.title.id","weighted.average.vote","mean.vote",
                            "median.vote","males.allages.avg.vote","males.allages.votes",
                            "females.allages.avg.vote","females.allages.votes",
                            "top1000.voters.rating","top1000.voters.votes","us.voters.rating",
                            "us.voters.votes","non.us.voters.rating","non.us.voters.votes"))
USAMOVIESratings <- merge(USAMOVIES,ratings,by="imdb.title.id")



attach(USAMOVIESratings)
detach(USAMOVIESratings)

year21<-subset(USAMOVIESratings, year>=2000)
posprof<- subset(year21, profit.infl>0)
negprofit<- subset(year21, profit.infl<=0)
detach(year)
attach(posprof)
detach(posprof)
attach(negprofit)
detach(negprofit)
p<-ggplot(negprofit, aes(x=logprofit)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") + 
  scale_x_continuous(breaks=seq(0, 10, 0.5)+scale_y_continuous(label = scales::comma))
p+ geom_vline(aes(xintercept=mean(logprofit)),
              color="blue", linetype="dashed", size=1) + ggtitle("Spread of Movie Budget")
hist(trans)


#What is the best way to predict the profit margin of a movie?--Collins

minprof<- abs(min(profit.infl))
normprofit <-profit.infl+minprof+1
sqrt<- sqrt(normprofit)
trans<-1/((minprof+1)-profit.infl)
skewness(trans, na.rm = TRUE)
logprofit<- log(normprofit)
logprofit<- log(profit.infl)

library(MASS)
par(mfrow=c(1,2))
boxcox(profit.infl)
boxcox(m1,lambda=seq(0.325,0.34,length=20))

fullmodprofitinfl<- lm(logprofit~ year+ duration +avg.vote+votes+reviews.from.users+reviews.from.critics+IsItHorror+IsItRomance+IsItAction+IsItComedy+IsItDrama+mean.vote+median.vote+males.allages.avg.vote+males.allages.votes+females.allages.avg.vote+females.allages.votes+top1000.voters.rating+top1000.voters.votes+us.voters.rating+us.voters.votes+non.us.voters.rating+non.us.voters.votes, data=negprofit)
summary(fullmodprofitinfl)

#####get to work
allinputs<-cbind(year,duration,avg.vote,votes,reviews.from.users,reviews.from.critics,IsItHorror,IsItRomance,IsItAction,IsItComedy,IsItDrama,weighted.average.vote,mean.vote,median.vote,males.allages.avg.vote,males.allages.votes,females.allages.avg.vote,females.allages.votes,top1000.voters.rating,top1000.voters.votes,us.voters.rating,us.voters.votes,non.us.voters.rating,non.us.voters.votes, data=posprof)
best <- regsubsets(as.matrix(allinputs), logprofit)
summary(best)


###BACK AIC
  
  ##inflation
  backAICinfl <- step(fullmodprofitinfl,direction="backward", data=posprof)
  backAICinfl$coefficients
  backAICmodelinfllog<-lm(logprofit ~ votes + reviews.from.users + reviews.from.critics + 
    IsItHorror + IsItDrama + mean.vote + males.allages.avg.vote + 
    males.allages.votes + females.allages.avg.vote + females.allages.votes + 
    top1000.voters.rating + top1000.voters.votes + non.us.voters.rating, data=USAMOVIESratings)
  summary(backAICmodelinfllog)

  #AIC= -9701.66
  #R^2= 0.406  
  
  trial<-lm(logprofit ~ duration + reviews.from.critics + IsItAction + top1000.voters.votes + 
              us.voters.rating + non.us.voters.votes, data=negprofit)
  summary(trial)



###BACK BIC
  ninfl <- length(fullmodprofitinfl$residuals)
  ##inflation
  backBICinfl <- step(fullmodprofitinfl,direction="backward", data=USAMOVIESratings, k=log(ninfl))
  backBICinfl$coefficients
  backBICmodelinfllog<-lm(logprofit ~ votes + reviews.from.critics + IsItHorror + IsItDrama + 
                         mean.vote + males.allages.votes + females.allages.avg.vote + 
                         females.allages.votes + top1000.voters.votes + non.us.voters.rating, data=USAMOVIESratings)
  summary(backBICmodelinfllog)
  #BIC= -9619.72
  #R^2=  0.4047 

  
###FORWARD AIC 
  interceptinfl <- lm(logprofit~1,data=USAMOVIESratings)
  
  forwardAICinfl <- step(interceptinfl,scope=list(lower=~1,upper=~year+ duration +avg.vote+votes+reviews.from.users+reviews.from.critics+IsItHorror+IsItRomance+IsItAction+IsItComedy+IsItDrama+mean.vote+median.vote+males.allages.avg.vote+males.allages.votes+females.allages.avg.vote+females.allages.votes+top1000.voters.rating+top1000.voters.votes+us.voters.rating+us.voters.votes+non.us.voters.rating+non.us.voters.votes),direction="forward", data=USAMOVIESratings)
  forwardAICinfl$coefficients
  forwardAICmodelinfl<-lm(logprofit ~ top1000.voters.votes + females.allages.votes + IsItDrama + 
                            reviews.from.users + non.us.voters.votes + votes + males.allages.votes + 
                            IsItHorror + reviews.from.critics + non.us.voters.rating + 
                            females.allages.avg.vote + mean.vote + top1000.voters.rating + 
                            males.allages.avg.vote,data=USAMOVIESratings)
  summary(forwardAICmodelinfl)
  
  #AIC= -9699.8
  #R^2=0.4059
  
  
###FORWARD BIC 

  forwardBICinfl <- step(interceptinfl,scope=list(lower=~1,upper=~year+ duration +avg.vote+votes+reviews.from.users+reviews.from.critics+IsItHorror+IsItRomance+IsItAction+IsItComedy+IsItDrama+mean.vote+median.vote+males.allages.avg.vote+males.allages.votes+females.allages.avg.vote+females.allages.votes+top1000.voters.rating+top1000.voters.votes+us.voters.rating+us.voters.votes+non.us.voters.rating+non.us.voters.votes),direction="forward", data=USAMOVIESratings,k=log(ninfl))
  forwardBICinfl$coefficients
  forwardBICmodelinfllog<-lm(logprofit ~ top1000.voters.votes + females.allages.votes + IsItDrama + 
                            reviews.from.users + non.us.voters.votes + votes + males.allages.votes + 
                            IsItHorror + reviews.from.critics,data=USAMOVIESratings)
  summary(forwardBICmodelinfllog)
  
  #BIC= -9531.61
  #R^2= 0.3943 
  
###STEP
  #AIC
  StepAIC <- step(fullmodprofitinfl,direction="both", data=negprofit)
  StepAICmodelinfl<-lm(logprofit ~ votes + reviews.from.users + reviews.from.critics + 
                         IsItHorror + IsItDrama + mean.vote + males.allages.avg.vote + 
                         males.allages.votes + females.allages.avg.vote + females.allages.votes + 
                         top1000.voters.rating + top1000.voters.votes + non.us.voters.rating,data=USAMOVIESratings)
  summary(StepAICmodelinfl)
    #AIC= -9701.66
    #R^2= 0.406 
  
  #BIC
  StepBIC <- step(fullmodprofitinfl,direction="both", data=posprof, k=log(ninfl))
  StepBICmodelinfl<-lm(logprofit ~ votes + reviews.from.critics + IsItHorror + IsItDrama + 
                         mean.vote + males.allages.votes + females.allages.avg.vote + 
                         females.allages.votes + top1000.voters.votes + non.us.voters.rating,data=USAMOVIESratings)
  summary(StepBICmodelinfl)
    #BIC= -9619.72
    #R^2= 0.4047   
    
 
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
#Original Code/Models without Log  
  
  backAICmodelinfl<- lm(profit.infl ~ year + duration + avg.vote + votes + reviews.from.users + 
                          reviews.from.critics + IsItHorror + IsItAction + IsItComedy + 
                          IsItDrama + mean.vote + males.allages.votes + females.allages.avg.vote + 
                          females.allages.votes + top1000.voters.rating + top1000.voters.votes + 
                          us.voters.rating + us.voters.votes + non.us.voters.rating, data=USAMOVIESratings)
  #AIC= 210847.5
  #R^2= 0.3937 
  backBICmodelinfl<-lm(profit.infl ~ year + duration + avg.vote + votes + reviews.from.users + 
                         reviews.from.critics + IsItHorror + IsItDrama + mean.vote + 
                         males.allages.votes + females.allages.avg.vote + females.allages.votes + 
                         top1000.voters.votes + us.voters.votes, data=USAMOVIESratings)
  summary(backBICmodelinfl)
  #BIC= 210951.8
  #R^2=  0.3927  
  
  forwardAICmodelinfl<-lm(profit.infl ~ votes + males.allages.votes + top1000.voters.votes + 
                            non.us.voters.votes + IsItDrama + duration + year + reviews.from.users + 
                            IsItHorror + reviews.from.critics + us.voters.votes + non.us.voters.rating + 
                            females.allages.avg.vote + females.allages.votes + IsItComedy + 
                            avg.vote + mean.vote + top1000.voters.rating + IsItAction + 
                            us.voters.rating,data=USAMOVIESratings)
  summary(forwardAICmodelinfl)
  forwardBICmodelinfl<-lm(profit.infl ~ votes + males.allages.votes + top1000.voters.votes + 
                            non.us.voters.votes + IsItDrama + duration + year + reviews.from.users + 
                            IsItHorror + reviews.from.critics + us.voters.votes + non.us.voters.rating + 
                            females.allages.avg.vote + females.allages.votes,data=USAMOVIESratings)
  summary(forwardBICmodelinfl)
  
  #BIC= 210970.3
  #R^2= 0.3906 
  
  #AIC= 210849
  #R^2=0.3937
  StepAICmodelinfl<-lm(profit.infl ~ year + duration + avg.vote + votes + reviews.from.users + 
                         reviews.from.critics + IsItHorror + IsItAction + IsItComedy + 
                         IsItDrama + mean.vote + males.allages.votes + females.allages.avg.vote + 
                         females.allages.votes + top1000.voters.rating + top1000.voters.votes + 
                         us.voters.rating + us.voters.votes + non.us.voters.rating,data=USAMOVIESratings)
  summary(StepAICmodelinfl)
  #AIC= 210847.5
  #R^2= 0.3937 
  StepBICmodelinfl<-lm(profit.infl ~ year + duration + avg.vote + votes + reviews.from.users + 
                         reviews.from.critics + IsItHorror + IsItDrama + mean.vote + 
                         males.allages.votes + females.allages.avg.vote + females.allages.votes + 
                         top1000.voters.votes + us.voters.votes,data=USAMOVIESratings)
  summary(StepBICmodelinfl)
  #BIC= 210951.8
  #R^2= 0.3927  
  
  
  

