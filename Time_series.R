options(max.print=10000)

library(readr)
library(leaps)
library("stringr")
library(dplyr)

setwd("C:/Users/Mary/Documents/Grad School/ST 541/Final Project")
IMDb_movies <- read_csv("IMDb movies.csv")
ratings <- read_csv("IMDb ratings.csv")
cpi_rates <- read.csv("cpi_table.csv")
names(ratings)<-str_replace_all(names(ratings), c("_" = "." , "," = "" ))
names(IMDb_movies)<-str_replace_all(names(IMDb_movies), c("_" = "." , "," = "" ))

colnames(IMDb_movies)[19] <- "worldwide.gross.income"

budget <-IMDb_movies[!is.na(IMDb_movies$budget),]
usamovies<-budget[!is.na(budget$usa.gross.income),]#worldwide
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

USAMOVIESratings <- merge(USAMOVIES,ratings,by="imdb.title.id")
names(USAMOVIESratings)<-str_replace_all(names(USAMOVIESratings), c("_" = "." , "," = "" ))


# Creating a time series analysis

MOVIES.TIME.SERIES <- USAMOVIESratings[c("year","date.published","profit.infl")]

MOVIES.TIME.SERIES <- MOVIES.TIME.SERIES[order(MOVIES.TIME.SERIES$date.published),]                             
attach(MOVIES.TIME.SERIES)

# Counts the number of unique dates of publication
unique.dates <- unique(MOVIES.TIME.SERIES$date.published)
length(unique.dates)   

# Initial Plot

qplot(date.published,profit.infl, 
      main="Profits for Movies for the past Century", xlab = "Date Movie was Published", ylab = "Profit Made (including inflation)")

# Found inconsistencies in the year and date.published so create a new column to extract the year from date.published
MOVIES.TIME.SERIES$year.published<- as.integer(substring(MOVIES.TIME.SERIES$date.published,1,4))
head(MOVIES.TIME.SERIES$year.published)

# Group movies by year and investigate that time series
years <- group_by(MOVIES.TIME.SERIES,year.published)

avg.profit.by.year <- summarize(years,avg.profit.infl = mean(profit.infl))
avg.profit.by.year

ggplot(data = avg.profit.by.year, aes(x = year.published, y = avg.profit.infl)) +
  geom_line(stat = "identity", fill = "blue") +
  labs(title = "Avgerage Profit gained per Year",
       subtitle = "1921 - 2020",
       x = "Date", y = "Profit")

# Data still looked skewed so created a new column that created the decade the movie was created.
MOVIES.TIME.SERIES$decade.published <- as.integer(substring(MOVIES.TIME.SERIES$date.published,1,3))*10
head(MOVIES.TIME.SERIES)

decade <- group_by(MOVIES.TIME.SERIES,decade.published)

avg.profit.by.decade <- summarize(decade,avg.profit.infl = mean(profit.infl))
avg.profit.by.decade

ggplot(data = avg.profit.by.decade, aes(x = decade.published, y = avg.profit.infl)) +
  geom_line(stat = "identity", fill = "blue") +
  xlim(1920,2020) +
  labs(title = "Avgerage Profit gained over Decades",
       subtitle = "1920 - 2020",
       x = "Date", y = "Profit")

# Distribution of movies by profit over every decade
ggplot(data=decade, aes(x = profit.infl)) +
  geom_histogram() + 
  ylim(0,10) +
  facet_wrap(~decade.published)
  


#Checking for correlation between any given date and the one before.
plot(profit.infl[1:6529], profit.infl[2:6530], pch=20, col=4,
     main="Profit for Individual Movies by Date Published", xlab="Profit(t-1)",
     ylab = "Profit(t)")
text(x=6, y=45, col=2, cex=1.5,
     labels=paste("Corr =", round(cor(profit.infl[1:6529],
                                      profit.infl[2:6530]),2)))

# Autocorrelation between each date
print(acf(profit.infl))


#Checking for correlation between any given year and the one before.
plot(avg.profit.by.year$avg.profit.infl[1:78], avg.profit.by.year$avg.profit.infl[2:79], pch=20, col=4,
     main="Yearly Profit", xlab="Profit(t-1)",
     ylab = "Profit(t)")
text(x=6, y=45, col=2, cex=1.5,
     labels=paste("Corr =", round(cor(avg.profit.by.year$avg.profit.infl[1:78],
                                      avg.profit.by.year$avg.profit.infl[2:79]),2)))

# Autocorrelation between each year
print(acf(avg.profit.by.year$avg.profit.infl))

*Every 5 years have a significant correlation


