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

str(USAMOVIES)
summary(USAMOVIES)

#Does the budget have an effect on the profit margins, total gross, and user ratings of the movie?
#How are ratings affected by movie genre, movie duration, gender, or age group?
#Are production companies a significant variable when discussing the success of a movie?
#What is the best way to predict the profit margin of a movie?
#What is the comparison of international movies to movies made in America by market share and has that trend evolved over the years?

attach(USAMOVIES)
detach(USAMOVIES)



#What is the best way to predict the profit margin of a movie?--Collins
#fill metascore with 0? just not include it. removed for now.
#input genre later after steven pushed
fullmodel<- lm(profit~ year+ duration +avg.vote+votes+reviews.from.users+reviews.from.critics, data=USAMOVIES)
summary(fullmodel)
allinputs<-c(year,duration,avg.vote,votes,reviews.from.users,reviews.from.critics)
best <- regsubsets(as.matrix(allinputs), profit)
summary(best)
backAIC <- step(fullmodel,direction="backward", data=USAMOVIES)

backAIC$coefficients
backAICmodel<-lm(profit ~ avg.vote + votes + reviews.from.users + reviews.from.critics)
summary(backAICmodel)

n <- length(fullmodel$residuals)
backBIC <- step(fullmodel,direction="backward", data=USAMOVIES, k=log(n))
backBIC$coefficients
backBICmodel<-lm(profit ~ avg.vote + votes + reviews.from.users + reviews.from.critics)
summary(backBICmodel)

intercept <- lm(profit~1,data=USAMOVIES)
forwardBIC <- step(intercept,scope=list(lower=~1,upper=~year,duration,avg.vote,votes,reviews.from.users,reviews.from.critics),direction="forward", data=USAMOVIES,k=log(n))
forwardBICmodel<-lm()
summary(forwardBICmodel)
forwardBIC$coefficients
>>>>>>> Stashed changes
