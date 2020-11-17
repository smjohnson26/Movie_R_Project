options(max.print=10000)

library(readr)
library(leaps)
library("stringr")
library(dplyr)

USAMOVIES <- read_csv("./USAMOVIES_INFL.csv")
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

# Creating a time series analysis

MOVIES.TIME.SERIES <- select(USAMOVIESratings,-c("X1", "Unnamed: 0","X","Unnamed..0","title","actors","description","writer","budget",
                                                 "usa.gross.income", "worlwide.gross.income", "votes.10", "votes.9", "votes.8", "votes.7", 
                                                 "votes.6","votes.5","votes.4","votes.3", "votes.2", "votes.1"))

MOVIES.TIME.SERIES <- MOVIES.TIME.SERIES[order(MOVIES.TIME.SERIES$date.published),]                             
attach(MOVIES.TIME.SERIES)

# Counts the number of unique dates of publication
unique.dates <- unique(MOVIES.TIME.SERIES$date.published)
length(unique.dates)   

date.published <- as.Date(date.published)

qplot(date.published,profit.infl, 
      main="Profit Gained over time")

ggplot(data = MOVIES.TIME.SERIES, aes(x = date.published, y = profit.infl)) +
  geom_bar(stat = "identity", fill = "green") +
  labs(title = "Profit gained over time",
       subtitle = "1921 - 2020",
       x = "Date", y = "Profit")
