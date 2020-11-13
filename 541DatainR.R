library(readr)
library(leaps)
library("stringr")
library(dplyr)

USAMOVIES <- read_csv("./USAMOVIES.csv")
ratings <- read_csv("IMDb ratings.csv")

names(ratings)<-str_replace_all(names(ratings), c("_" = "." , "," = "" ))
names(USAMOVIES)<-str_replace_all(names(USAMOVIES), c("_" = "." , "," = "" ))
USAMOVIES$budget <- substr(USAMOVIES$budget, start = 2, stop = 15)
USAMOVIES$budget <- as.numeric(USAMOVIES$budget)
USAMOVIES$usa.gross.income <- substr(USAMOVIES$usa.gross.income, start = 2, stop = 15)
USAMOVIES$usa.gross.income <- as.numeric(USAMOVIES$usa.gross.income)
USAMOVIES$worlwide.gross.income <- substr(USAMOVIES$worlwide.gross.income, start = 2, stop = 15)
USAMOVIES$worlwide.gross.income <- as.numeric(USAMOVIES$worlwide.gross.income)
USAMOVIES$profit <-(USAMOVIES$worlwide.gross.income- USAMOVIES$budget)
USAMOVIES<-USAMOVIES[!is.na(USAMOVIES$reviews.from.users)&!is.na(USAMOVIES$reviews.from.critics),]
USAMOVIES <- select(USAMOVIES,-c("metascore"))
USAMOVIESratings <- merge(USAMOVIES,ratings,by="imdb.title.id")

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

attach(USAMOVIESratings)
detach(USAMOVIESratings)



#What is the best way to predict the profit margin of a movie?--Collins

