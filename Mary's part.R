options(max.print=10000)

library(readr)
library(leaps)
library("stringr")
library(dplyr)

USAMOVIES <- read_csv("USAMOVIES_INFL.csv")
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

prod.co.analysis <- USAMOVIESratings[c(7,16, 19, 27, 31)]
attach(prod.co.analysis)

# Testing the significance of each production company compared to profit 
prod_co_mod <- lm(profit~production.company)
summary(prod_co_mod)

# Model Summary
p-value = 1
Adjusted R^2 = -0.2588

# Collecting the significant companies
significant_production_cos_profit <- subset(prod.co.analysis,summary(prod_co_mod)$coeff[-1,4] < 0.05)
num_sig_comps_profit <- count(significant_production_cos_profit, vars = production.company)
ordered_run_profit <- num_sig_comps_profit[order(-num_sig_comps_profit$n),]
head(ordered_run_profit)

# Testing the significance of each production company compared to profit (including inflation)
prod_infl_co_mod <- lm(profit.infl~production.company)
summary(prod_infl_co_mod)

# Model Summary
p-value = 0.0001483 
Adjusted R^2 = 0.04933

# Collecting the significant companies
significant_production_cos_profit_infl <- subset(prod.co.analysis,summary(prod_infl_co_mod)$coeff[-1,4] < 0.05)
num_sig_comps_profit_infl <- count(significant_production_cos_profit_infl, vars = production.company)
ordered_run_profit_infl <- num_sig_comps_profit_infl[order(-num_sig_comps_profit_infl$n),]
head(ordered_run_profit_infl)

# Production Company vs Ratings
prod_ratings <- lm(prod.co.analysis$avg.vote ~ prod.co.analysis$production.company)
summary(prod_ratings)

# Model Summary
p-value = <2.2e^-16
Adjusted R^2 = 0.1912

# Collecting the significant companies
significant_production_cos_ratings <- subset(prod.co.analysis,summary(prod_ratings)$coeff[-1,4] < 0.05)
num_sig_comps_ratings <- count(significant_production_cos_ratings, vars = production.company)
ordered_run_ratings <- num_sig_comps_ratings[order(-num_sig_comps_ratings$n),]
head(ordered_run_ratings)

