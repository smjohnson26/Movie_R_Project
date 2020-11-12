library(readr)
library(leaps)
library("stringr")

USAMOVIES <- read_csv("MSBA/Fall/ST 541/Final Project/Movie_R_Project/USAMOVIES.csv")


names(USAMOVIES)<-str_replace_all(names(USAMOVIES), c("_" = "." , "," = "" ))
USAMOVIES$budget <- substr(USAMOVIES$budget, start = 2, stop = 15)
USAMOVIES$budget <- as.numeric(USAMOVIES$budget)
USAMOVIES$usa.gross.income <- substr(USAMOVIES$usa.gross.income, start = 2, stop = 15)
USAMOVIES$usa.gross.income <- as.numeric(USAMOVIES$usa.gross.income)
USAMOVIES$worlwide.gross.income <- substr(USAMOVIES$worlwide.gross.income, start = 2, stop = 15)
USAMOVIES$worlwide.gross.income <- as.numeric(USAMOVIES$worlwide.gross.income)
USAMOVIES$profit <-(USAMOVIES$worlwide.gross.income- USAMOVIES$budget)


length(grep("Drama",USAMOVIES$genre))
length(grep("Comedy",USAMOVIES$genre))
length(grep("Action",USAMOVIES$genre))
length(grep("Horror",USAMOVIES$genre))
length(grep("Romance",USAMOVIES$genre))


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
str(USAMOVIES)
summary(USAMOVIES)


attach(USAMOVIES)
detach(USAMOVIES)



#What is the best way to predict the profit margin of a movie?--Collins
#fill metascore with 0? just not include it. removed for now.
#input genre and gender later after steven pushed
fullmodel<- lm(profit~ year+ duration +avg.vote+votes+reviews.from.users+reviews.from.critics+IsItHorror+IsItRomance+IsItAction+IsItComedy+IsItDrama, data=USAMOVIES)
summary(fullmodel)
allinputs<-cbind(year,duration,avg.vote,votes,reviews.from.users,reviews.from.critics,IsItHorror,IsItRomance,IsItAction,IsItComedy,IsItDrama)
best <- regsubsets(as.matrix(allinputs), profit)
summary(best)

backAIC <- step(fullmodel,direction="backward", data=USAMOVIES)

backAIC$coefficients
backAICmodel<-lm(profit ~ year + duration + votes + reviews.from.users + reviews.from.critics + 
                   IsItHorror + IsItComedy + IsItDrama)
#AIC= 204936.6
summary(backAICmodel)
#R^2= 0.4579

n <- length(fullmodel$residuals)
backBIC <- step(fullmodel,direction="backward", data=USAMOVIES, k=log(n))
backBIC$coefficients
backBICmodel<-lm(profit ~ votes + reviews.from.users + reviews.from.critics + 
                   IsItHorror + IsItDrama)
#AIC= 204985.3
summary(backBICmodel)
#R^2= 0.4568 

intercept <- lm(profit~1,data=USAMOVIES)
forwardBIC <- step(intercept,scope=list(lower=~1,upper=~year+duration+avg.vote+votes+reviews.from.users+reviews.from.critics+IsItHorror+IsItRomance+IsItAction+IsItComedy+IsItDrama),direction="forward", data=USAMOVIES,k=log(n))
forwardBICmodel<-lm()
summary(forwardBICmodel)
forwardBIC$coefficients

intercept <- lm(profit~1,data=USAMOVIES)
forwardAIC <- step(intercept,scope=list(lower=~1,upper=~year+duration+avg.vote+votes+reviews.from.users+reviews.from.critics+IsItHorror+IsItRomance+IsItAction+IsItComedy+IsItDrama),direction="forward", data=USAMOVIES)
forwardAICmodel<-lm()
summary(forwardAICmodel)
forwardAIC$coefficients

