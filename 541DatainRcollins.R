setwd('C:/Users/CAM/Documents/MSBA/Fall/ST 541/Final Project/Movie_R_Project')
library(readr)
library(leaps)
library("stringr")
library(dplyr)
library(ggplot2)
library(car)



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
# Data Visualizations


colnames(USAMOVIESratings)
str(USAMOVIESratings)
g.
summary(USAMOVIESratings[, c("year", "duration", "production.company","budget.infl","worldwide.gross.infl","usa.gross.infl","profit.infl",            
                            "weighted.average.vote","males.allages.avg.vote","females.allages.avg.vote")])

table(IsItHorror)
table(IsItRomance)
table(IsItAction)
table(IsItComedy)
table(IsItDrama)

par(mfrow = c(1, 1))
hist(budget.infl, main = "Distribution of Budget", xlab = "budget", las= 1, xaxt="n")
#boxplot(budget.infl, data = USAMOVIESratings, main = "Box Plot of Budget", yaxt="n")
#axis(2, at=axTicks(2), labels=sprintf("$%s", axTicks(2)), las = 1)
axis(side = 1, at = seq(1000,400000000,100000), labels = seq(1000,400000000,100000))
# scale_y_continuous(label = scales::comma))
hist(profit.infl, main = "Distribution of Profit",
     xlab = "profit", las= 1)
     #xlim = c(-174531755,7462749659)), breaks=c(-20000000, -10000000, 0, 100000000, 200000000, 300000000, 400000000, 500000000)
hist(avg.vote, main = "Distribution of Average Vote", xlab = "Average Vote", xlim = c(0,10), breaks = c(0,1,2,3,4,5,6,7,8,9,10), las= 1)
summary(USAMOVIES)
dim(USAMOVIES)

ggplot(USAMOVIESratings, aes(x=budget.infl)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") + scale_x_continuous(breaks=seq(0, 10, 0.5)+scale_y_continuous(label = scales::comma))
  p+ geom_vline(aes(xintercept=mean(budget.infl)),
              color="blue", linetype="dashed", size=1) + ggtitle("Spread of Movie Budget")
  
  
ggplot(USAMOVIESratings, aes(x=profit.infl)) + 
    geom_histogram(aes(y=..density..), colour="black", fill="white")+
    geom_density(alpha=.2, fill="#FF6666") + scale_x_continuous(breaks=seq(0, 10, 0.5)+scale_y_continuous(label = scales::comma))
    p+ geom_vline(aes(xintercept=mean(profit.infl)),
                color="blue", linetype="dashed", size=1) + ggtitle("Spread of Movie Profit")







#What is the best way to predict the profit margin of a movie?--Collins
fullmodprofitinfl<- lm(profit.infl~ year+ duration +avg.vote+votes+reviews.from.users+reviews.from.critics+IsItHorror+IsItRomance+IsItAction+IsItComedy+IsItDrama+mean.vote+median.vote+males.allages.avg.vote+males.allages.votes+females.allages.avg.vote+females.allages.votes+top1000.voters.rating+top1000.voters.votes+us.voters.rating+us.voters.votes+non.us.voters.rating+non.us.voters.votes, data=USAMOVIESratings)
summary(fullmodprofitinfl)

#####get to work
allinputs<-cbind(year,duration,avg.vote,votes,reviews.from.users,reviews.from.critics,IsItHorror,IsItRomance,IsItAction,IsItComedy,IsItDrama,weighted.average.vote,mean.vote,median.vote,males.allages.avg.vote,males.allages.votes,females.allages.avg.vote,females.allages.votes,top1000.voters.rating,top1000.voters.votes,us.voters.rating,us.voters.votes,non.us.voters.rating,non.us.voters.votes, data=USAMOVIESratings)
best <- regsubsets(as.matrix(allinputs), profit)
summary(best)


###BACK AIC
  
  ##inflation
  backAICinfl <- step(fullmodprofitinfl,direction="backward", data=USAMOVIESratings)
  backAICinfl$coefficients
  backAICmodelinfl<- lm(profit.infl ~ year + duration + avg.vote + votes + reviews.from.users + 
   reviews.from.critics + IsItHorror + IsItAction + IsItComedy + 
    IsItDrama + mean.vote + males.allages.votes + females.allages.avg.vote + 
    females.allages.votes + top1000.voters.rating + top1000.voters.votes + 
    us.voters.rating + us.voters.votes + non.us.voters.rating, data=USAMOVIESratings)
  summary(backAICmodelinfl)
  #AIC= 210847.5
  #R^2= 0.3937 




###BACK BIC
  ninfl <- length(fullmodprofitinfl$residuals)
  ##inflation
  backBICinfl <- step(fullmodprofitinfl,direction="backward", data=USAMOVIESratings, k=log(ninfl))
  backBICinfl$coefficients
  backBICmodelinfl<-lm(profit.infl ~ year + duration + avg.vote + votes + reviews.from.users + 
                         reviews.from.critics + IsItHorror + IsItDrama + mean.vote + 
                         males.allages.votes + females.allages.avg.vote + females.allages.votes + 
                         top1000.voters.votes + us.voters.votes, data=USAMOVIESratings)
  summary(backBICmodelinfl)
  #BIC= 210951.8
  #R^2=  0.3927  

  
###FORWARD AIC 
  interceptinfl <- lm(profit.infl~1,data=USAMOVIESratings)
  
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

  forwardBICinfl <- step(interceptinfl,scope=list(lower=~1,upper=~year+ duration +avg.vote+votes+reviews.from.users+reviews.from.critics+IsItHorror+IsItRomance+IsItAction+IsItComedy+IsItDrama+mean.vote+median.vote+males.allages.avg.vote+males.allages.votes+females.allages.avg.vote+females.allages.votes+top1000.voters.rating+top1000.voters.votes+us.voters.rating+us.voters.votes+non.us.voters.rating+non.us.voters.votes),direction="forward", data=USAMOVIESratings,k=log(ninfl))
  forwardBICinfl$coefficients
  forwardBICmodelinfl<-lm(profit.infl ~ votes + males.allages.votes + top1000.voters.votes + 
                            non.us.voters.votes + IsItDrama + duration + year + reviews.from.users + 
                            IsItHorror + reviews.from.critics + us.voters.votes + non.us.voters.rating + 
                            females.allages.avg.vote + females.allages.votes,data=USAMOVIESratings)
  summary(forwardBICmodelinfl)
  
  #BIC= 210970.3
  #R^2= 0.3906 
  
###STEP
  #AIC
  StepAIC <- step(fullmodprofitinfl,direction="both", data=USAMOVIESratings)
  StepAICmodelinfl<-lm(profit.infl ~ year + duration + avg.vote + votes + reviews.from.users + 
                         reviews.from.critics + IsItHorror + IsItAction + IsItComedy + 
                         IsItDrama + mean.vote + males.allages.votes + females.allages.avg.vote + 
                         females.allages.votes + top1000.voters.rating + top1000.voters.votes + 
                         us.voters.rating + us.voters.votes + non.us.voters.rating,data=USAMOVIESratings)
  summary(StepAICmodelinfl)
    #AIC= 210847.5
    #R^2= 0.3937 
  
  #BIC
  StepBIC <- step(fullmodprofitinfl,direction="both", data=USAMOVIESratings, k=log(ninfl))
  StepBICmodelinfl<-lm(profit.infl ~ year + duration + avg.vote + votes + reviews.from.users + 
                         reviews.from.critics + IsItHorror + IsItDrama + mean.vote + 
                         males.allages.votes + females.allages.avg.vote + females.allages.votes + 
                         top1000.voters.votes + us.voters.votes,data=USAMOVIESratings)
  summary(StepBICmodelinfl)
    #BIC= 210951.8
    #R^2= 0.3927  
    
  
  
  

