setwd('C:/Users/CAM/Documents/MSBA/Fall/ST 541/Final Project/Movie_R_Project')
library(readr)
library(leaps)
library("stringr")
library(ggplot2)
library(car)
library(dplyr)

IMDb_movies <- read_csv("IMDb movies.csv")
ratings <- read_csv("IMDb ratings.csv")
cpi_rates <- read.csv("cpi_table.csv")
names(ratings)<-str_replace_all(names(ratings), c("_" = "." , "," = "" ))
names(IMDb_movies)<-str_replace_all(names(IMDb_movies), c("_" = "." , "," = "" ))
#rename 'worlwide' to 'worldwide'
colnames(IMDb_movies)[19] <- "worldwide.gross.income"

IMDb_movies["IsItHorror"]<- 0
IMDb_movies["IsItRomance"] <- 0
IMDb_movies["IsItAction"] <- 0
IMDb_movies["IsItComedy"] <- 0
IMDb_movies["IsItDrama"] <- 0
IMDb_movies$IsItHorror[grep("Horror",IMDb_movies$genre)] <- 1
IMDb_movies$IsItRomance[grep("Romance",IMDb_movies$genre)] <- 1
IMDb_movies$IsItAction[grep("Action",IMDb_movies$genre)] <- 1
IMDb_movies$IsItComedy[grep("Comedy",IMDb_movies$genre)] <- 1
IMDb_movies$IsItDrama[grep("Drama",IMDb_movies$genre)] <- 1
IMDb_movies$IsItHorror<-as.factor(IMDb_movies$IsItHorror)
IMDb_movies$IsItRomance<-as.factor(IMDb_movies$IsItRomance)
IMDb_movies$IsItAction<-as.factor(IMDb_movies$IsItAction)
IMDb_movies$IsItComedy<-as.factor(IMDb_movies$IsItComedy)
IMDb_movies$IsItDrama<-as.factor(IMDb_movies$IsItDrama)

budget <-IMDb_movies[!is.na(IMDb_movies$budget),]
usamovies<-budget[!is.na(budget$usa.gross.income),]
usamovies1<-usamovies[!is.na(usamovies$country),]
usamovies2<-usamovies1[grep("USA",usamovies1$country),]
usamovies2$Currency <- substr(usamovies2$budget, start = 1, stop = 3)
for(value in 1:6605){
  if(substr(usamovies2$Currency[value], start = 1, stop = 1)=="$"){
    usamovies2$Currency[value] = "USD"
  }
}

USAMOVIES<-subset(usamovies2, usamovies2$Currency== "USD")
USAMOVIES$budget <- substr(USAMOVIES$budget, start = 2, stop = 15)
USAMOVIES$usa.gross.income <- substr(USAMOVIES$usa.gross.income, start = 2, stop = 15)
USAMOVIES$worldwide.gross.income <- substr(USAMOVIES$worldwide.gross.income, start = 2, stop = 15)
USAMOVIES$budget <- as.numeric(USAMOVIES$budget)
USAMOVIES$usa.gross.income <- as.numeric(USAMOVIES$usa.gross.income)
USAMOVIES$worldwide.gross.income <- as.numeric(USAMOVIES$worldwide.gross.income)
USAMOVIES$profit <-(USAMOVIES$worldwide.gross.income- USAMOVIES$budget)
summary(USAMOVIES)





#make year column into the index for cpi_rates dataframe
row.names(cpi_rates) <- cpi_rates$Year
cpi_rates[1] <- NULL
cpi_2020 <-  cpi_rates["2020", 1]


#Formula for inflated values: (original value * 2020 Consumer Price Index Value) / Consumer Price Index From Year of Release
for(val in 1:nrow(USAMOVIES)){
  USAMOVIES[val, "budg_infl"] <- (USAMOVIES[val, "budget"] * cpi_2020) / cpi_rates[toString(USAMOVIES[val, "year"]), "Avg"]
  USAMOVIES[val, "worldwide_gross_infl"] <- (USAMOVIES[val, "worldwide.gross.income"] * cpi_2020) / cpi_rates[toString(USAMOVIES[val, "year"]), "Avg"]
  USAMOVIES[val, "usa_gross_infl"] <- (USAMOVIES[val, "usa.gross.income"] * cpi_2020) / cpi_rates[toString(USAMOVIES[val, "year"]), "Avg"]
  USAMOVIES[val, "profit_infl"] <- (USAMOVIES[val, "profit"] * cpi_2020) / cpi_rates[toString(USAMOVIES[val, "year"]), "Avg"]
}
names(USAMOVIES)<-str_replace_all(names(USAMOVIES), c("_" = "." , "," = "" ))
#removing metascore instead of rows with NA's because of the number of NA's that cause problems in the models.
USAMOVIES <- select(USAMOVIES,-c("metascore"))
#merging dataset to include ratings dataset to incorporate more information that might explain profit
ratings<- select(ratings,c("imdb.title.id","mean.vote","median.vote","males.allages.avg.vote","males.allages.votes","females.allages.avg.vote","females.allages.votes","top1000.voters.rating","top1000.voters.votes","us.voters.rating","us.voters.votes","non.us.voters.rating","non.us.voters.votes"))
USAMOVIESratings <- merge(USAMOVIES,ratings,by="imdb.title.id")
USAMOVIESratings<- USAMOVIESratings[!is.na(USAMOVIESratings$reviews.from.users),]
USAMOVIESratings<- USAMOVIESratings[!is.na(USAMOVIESratings$reviews.from.critics),]
summary(USAMOVIESratings)

#subsetting dataset to only account for movies from 2000 on.
year21<-subset(USAMOVIESratings, year>=2000)
#subsetting dataset to separate movies where profit as positive or negative
posprof<- subset(year21, year21$profit.infl>0)
negprof<- subset(year21, year21$profit.infl<=0)




#What is the best way to predict the profit margin of a movie?--Collins

#initially did not transform profit but after analysis reran with log of profit. 

logprofitpos<- log(posprof$profit.infl)
absprofit<- abs(negprof$profit.infl)
logprofitneg<- (absprofit)^(1/3)
hist(logprofitneg)
#
fullmodpos<- lm(logprofitpos~ year+ duration +avg.vote+votes+reviews.from.users+reviews.from.critics+IsItHorror+IsItRomance+IsItAction+IsItComedy+IsItDrama+mean.vote+median.vote+males.allages.avg.vote+males.allages.votes+females.allages.avg.vote+females.allages.votes+top1000.voters.rating+top1000.voters.votes+us.voters.rating+us.voters.votes+non.us.voters.rating+non.us.voters.votes, data=posprof)
summary(fullmodpos)

fullmodneg<- lm(logprofitneg~ year+ duration +avg.vote+votes+reviews.from.users+reviews.from.critics+IsItHorror+IsItRomance+IsItAction+IsItComedy+IsItDrama+mean.vote+median.vote+males.allages.avg.vote+males.allages.votes+females.allages.avg.vote+females.allages.votes+top1000.voters.rating+top1000.voters.votes+us.voters.rating+us.voters.votes+non.us.voters.rating+non.us.voters.votes, data=negprof)
summary(fullmodneg)
summary(negprof)




  
###STEP
  #AIC
  StepAICpos <- step(fullmodpos,direction="both", data=posprof)
  StepAICmodpos<-lm(logprofitpos ~ year + duration + avg.vote + votes + reviews.from.users + 
                      reviews.from.critics + IsItRomance + IsItAction + IsItComedy + 
                      IsItDrama + males.allages.avg.vote + females.allages.avg.vote + 
                      females.allages.votes + top1000.voters.rating + top1000.voters.votes + 
                      us.voters.rating + non.us.voters.votes,data=posprof)
  summary(StepAICmodpos)
    #AIC= 929.51
    #R^2= 0.597  
  
  StepAICneg <- step(fullmodneg,direction="both", data=negprof)
  StepAICmodneg<-lm(logprofitneg ~ year + duration + reviews.from.critics + IsItHorror + 
                      IsItAction + IsItComedy + IsItDrama + mean.vote + males.allages.avg.vote + 
                      males.allages.votes + females.allages.avg.vote + females.allages.votes + 
                      top1000.voters.votes + us.voters.rating + us.voters.votes + 
                      non.us.voters.votes,data=negprof)
  summary(StepAICmodneg)
  #AIC= 12824.73
  #R^2= 0.3681   
  
  
  
  
  
  #BIC
  #
  npos <- length(fullmodpos$residuals)
  nneg <- length(fullmodneg$residuals)
  
  StepBIC <- step(fullmodpos,direction="both", data=posprof, k=log(npos))
  StepBICmodelpos<-lm(logprofitpos ~ year + duration + avg.vote + votes + reviews.from.users + 
                         reviews.from.critics + IsItComedy + IsItDrama + males.allages.avg.vote + 
                         females.allages.avg.vote + females.allages.votes + top1000.voters.votes + 
                         us.voters.rating + non.us.voters.votes,data=posprof)
  summary(StepBICmodelpos)
    #BIC= 1022.99
    #R^2= 0.5955   
  
  StepBIC <- step(fullmodneg,direction="both", data=negprof, k=log(nneg))
  StepBICmodelneg<-lm(logprofitneg ~ duration + reviews.from.critics + IsItHorror + 
                        IsItAction + mean.vote + males.allages.avg.vote + males.allages.votes + 
                        females.allages.avg.vote + females.allages.votes + top1000.voters.votes + 
                        us.voters.rating + us.voters.votes + non.us.voters.votes,data=negprof)
  summary(StepBICmodelneg)
  #BIC= 12905.24
  #R^2= 0.3643  
  
  summary(posprof)
 
  