library(readr)
<<<<<<< Updated upstream
IMDb_movies <- read_csv("./IMDb movies.csv", budget = col_double(), usa_gross_income = col_double(), worlwide_gross_income = col_double(), metascore = col_double())
USAMOVIES <- read_csv("USAMOVIES.csv")
=======
library("stringr")
library(leaps)
>>>>>>> Stashed changes

str(USAMOVIES)
summary(USAMOVIES)

<<<<<<< Updated upstream
#Does the budget have an effect on the profit margins, total gross, and user ratings of the movie?
#How are ratings affected by movie genre, movie duration, gender, or age group?
#Are production companies a significant variable when discussing the success of a movie?
#What is the best way to predict the profit margin of a movie?
#What is the comparison of international movies to movies made in America by market share and has that trend evolved over the years?
=======
attach(USAMOVIES)
#detach(USAMOVIES)



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
