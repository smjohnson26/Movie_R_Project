options(max.print=10000)
setwd("C:/Users/Mary/Documents/Grad School/ST 541/Final Project")

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

# Data Visualizations
colnames(USAMOVIESratings)
str(USAMOVIESratings)
summary(USAMOVIESratings[, c("production.company","profit", "profit.infl", "weighted.average.vote", "mean.vote")])


production.comp.analysis <- USAMOVIESratings[c(3,12, 24, 28, 29, 31)]

# Finding the production companies with the most movies
prod_comp_groups <- count(group_by(production.comp.analysis,production.company))
prod_co_groups_num_movies <- arrange(prod_comp_groups,-n)

top_companies_list <- prod_co_groups_num_movies[prod_co_groups_num_movies$n > 50,]

#top_quarter_prod_cos <- num_prod_co_groups[ num_prod_co_groups$n > quantile(num_prod_co_groups$n , 0.75 ) , ]
#others <- num_prod_co_groups[ num_prod_co_groups$n <= quantile(num_prod_co_groups$n , 0.75 ) , ]

movies_by_top_companies <- production.comp.analysis[production.company %in% top_companies_list$production.company,]

summary(movies_by_top_companies)

attach(movies_by_top_companies)

# Testing the significance of each production company compared to profit 
prod.comp.profit.model <- lm(profit~production.company)
summary(prod.comp.profit.model)

# Model Summary
p-value = <2.2e^-16
Adjusted R^2 = 0.04233

#Taking the column name out of the model summary
summ <- summary(prod.comp.profit.model)
profit.csumm <- coef(summ)
rownames(profit.csumm) <- sub("^production.company", "", rownames(profit.csumm))
profit.csumm

# Collecting the significant companies
significant_production_comps_profit <- subset(movies_by_top_companies,profit.csumm[-1,4] < 0.05)
num_sig_comps_profit <- count(significant_production_comps_profit, vars = production.company)
ordered_run_profit <- num_sig_comps_profit[order(-num_sig_comps_profit$n),]
head(ordered_run_profit,n=5)

#Testing the transformation of profit to adjust for the 
detach(movies_by_top_companies)

# What if there is a change in significance from the positive to negative profits?

#Separate movies with positive and negative profits
pos.profit.companies <- subset(movies_by_top_companies,movies_by_top_companies$profit > 0)
neg.profit.companies <- subset(movies_by_top_companies,movies_by_top_companies$profit < 0)

#view the summaries of each subset
summary(pos.profit.companies)
summary(neg.profit.companies)

attach(pos.profit.companies)
# Testing the significance of each production company compared to positive profit 

prod.comp.pos.profit.model <- lm(profit~production.company)
summary(prod.comp.pos.profit.model)

# Model Summary
p-value = 7.731e^-14
Adjusted R^2 = 0.04158
#Model decreased slightly in reliability

#Taking the column name out of the model summary
summary2 <- summary(prod.comp.pos.profit.model)
pos.profit.csummary <- coef(summary2)
rownames(pos.profit.csummary) <- sub("^production.company", "", rownames(pos.profit.csummary))
pos.profit.csummary

# Collecting the significant companies
significant_production_comps_pos_profit <- subset(pos.profit.companies,pos.profit.csummary[-1,4] < 0.05)
num_sig_comps_pos_profit <- count(significant_production_comps_pos_profit, vars = production.company)
ordered_run_pos_profit <- num_sig_comps_pos_profit[order(-num_sig_comps_pos_profit$n),]
head(ordered_run_pos_profit,n=5)

#Top production companies are the same, but in a different order.

detach(pos.profit.companies)
attach(neg.profit.companies)

# Testing the significance of each production company compared to negative profit 
prod.comp.neg.profit.model <- lm(profit~production.company)
summary(prod.comp.neg.profit.model)

# Model Summary
p-value = 0.03962
Adjusted R^2 = 0.01947
#Model is no longer significant

#Taking the column name out of the model summary
summary3 <- summary(prod.comp.neg.profit.model)
neg.profit.csummary <- coef(summary3)
rownames(neg.profit.csummary) <- sub("^production.company", "", rownames(neg.profit.csummary))
neg.profit.csummary

# Collecting the significant companies
significant_production_comps_neg_profit <- subset(neg.profit.companies,neg.profit.csummary[-1,4] < 0.05)
num_sig_comps_neg_profit <- count(significant_production_comps_neg_profit, vars = production.company)
ordered_run_neg_profit <- num_sig_comps_neg_profit[order(-num_sig_comps_neg_profit$n),]
head(ordered_run_neg_profit,n=5)

#Almost all of the significant production companies in this model are different than the positive profit except
# Universal Pictures => This implies that even the movies from Universal that have negative profit still have a 
# good following?

detach(neg.profit.companies)





# Testing the significance of each production company compared to profit (including inflation)
attach(movies_by_top_companies)

prod.comp.infl.profit.model <- lm(profit.infl~production.company)
summary(prod.comp.infl.profit.model)

# Model Summary
p-value = <2.2e^-16
Adjusted R^2 = 0.03826
#Model Comparison from profit without inflation
p-value = <2.2e^-16
Adjusted R^2 = 0.04233

#Taking the column name out of the model summary
summary4 <- summary(prod.comp.infl.profit.model)
profit.infl.csumm <- coef(summary4)
rownames(profit.infl.csumm) <- sub("^production.company", "", rownames(profit.infl.csumm))
profit.infl.csumm

# Collecting the significant companies
significant_production_comps_profit_infl <- subset(movies_by_top_companies,profit.infl.csumm[-1,4] < 0.05)
num_sig_comps_profit_infl <- count(significant_production_comps_profit_infl, vars = production.company)
ordered_run_profit_infl <- num_sig_comps_profit_infl[order(-num_sig_comps_profit_infl$n),]
head(ordered_run_profit_infl,n=5)

detach(movies_by_top_companies)

# What if there is a change in significance from the positive to negative profits (with influence)?

attach(pos.profit.companies)
# Testing the significance of each production company compared to positive profit 

prod.comp.pos.profit.infl.model <- lm(profit.infl~production.company)
summary(prod.comp.pos.profit.infl.model)

# Model Summary
p-value = 3.921e^-13
Adjusted R^2 = 0.03971
#Model comparison from profit without influence: 
p-value = 7.731e^-14
Adjusted R^2 = 0.04158

#Taking the column name out of the model summary
summary5 <- summary(prod.comp.pos.profit.infl.model)
pos.profit.infl.csummary <- coef(summary5)
rownames(pos.profit.infl.csummary) <- sub("^production.company", "", rownames(pos.profit.infl.csummary))
pos.profit.infl.csummary

# Collecting the significant companies
significant_production_comps_pos_profit_infl <- subset(pos.profit.companies,pos.profit.infl.csummary[-1,4] < 0.05)
num_sig_comps_pos_profit_infl <- count(significant_production_comps_pos_profit_infl, vars = production.company)
ordered_run_pos_profit_infl <- num_sig_comps_pos_profit_infl[order(-num_sig_comps_pos_profit_infl$n),]
head(ordered_run_pos_profit_infl,n=5)

#This model shows the exact same significant companies as the profit without inflation.

detach(pos.profit.companies)
attach(neg.profit.companies)

# Testing the significance of each production company compared to negative profit with inflation
prod.comp.neg.profit.infl.model <- lm(profit.infl~production.company)
summary(prod.comp.neg.profit.infl.model)

#Model Summary
p-value = 0.01407
Adjusted R^2 = 0.02573

# Model Comparison of profit without inflation
p-value = 0.03962
Adjusted R^2 = 0.01947
#Inflation helps the significance of the companies with negative profit values.

#Taking the column name out of the model summary
summary6 <- summary(prod.comp.neg.profit.infl.model)
neg.profit.infl.csummary <- coef(summary6)
rownames(neg.profit.infl.csummary) <- sub("^production.company", "", rownames(neg.profit.infl.csummary))
neg.profit.infl.csummary

# Collecting the significant companies
significant_production_comps_neg_profit_infl <- subset(neg.profit.companies,neg.profit.infl.csummary[-1,4] < 0.05)
num_sig_comps_neg_profit_infl <- count(significant_production_comps_neg_profit_infl, vars = production.company)
ordered_run_neg_profit_infl <- num_sig_comps_neg_profit_infl[order(-num_sig_comps_neg_profit_infl$n),]
head(ordered_run_neg_profit_infl,n=5)

#Compared to the model without inflation, this model trades Touchstome Pictures for Warner Bros. 

detach(neg.profit.companies)



attach(movies_by_top_companies)

# Production Company vs Ratings - Mean.Vote
prod.comp.ratings.model <- lm(mean.vote ~ production.company)
summary(prod.comp.ratings.model)

# Model Summary
p-value = 2.209e^-12
Adjusted R^2 = 0.02905

#Taking the column name out of the model summary
summary7 <- summary(prod.comp.ratings.model)
mean.vote.csummary <- coef(summary7)
rownames(mean.vote.csummary) <- sub("^production.company", "", rownames(mean.vote.csummary))
mean.vote.csummary

# Collecting the significant companies
significant_production_cos_ratings <- subset(movies_by_top_companies,mean.vote.csummary[-1,4] < 0.05)
num_sig_comps_ratings <- count(significant_production_cos_ratings, vars = production.company)
ordered_run_ratings <- num_sig_comps_ratings[order(-num_sig_comps_ratings$n),]
head(ordered_run_ratings)


# Production Company vs Ratings - Weighted.Average.Vote
prod.comp.ratings.model.2 <- lm(weighted.average.vote ~ production.company)
summary(prod.comp.ratings.model.2)

# Model Summary
p-value = <2.2e^-16
Adjusted R^2 = 0.2018
#118 observations compared to the 838 obervations from mean.vote 
#This model is significantly better than using the mean.vote variable.

#Taking the column name out of the model summary
summary8 <- summary(prod.comp.ratings.model.2)
weighted.average.vote.csummary <- coef(summary8)
rownames(weighted.average.vote.csummary) <- sub("^production.company", "", rownames(weighted.average.vote.csummary))
weighted.average.vote.csummary

# Collecting the significant companies
significant_production_cos_ratings_2 <- subset(movies_by_top_companies,weighted.average.vote.csummary[-1,4] < 0.05)
num_sig_comps_ratings_2 <- count(significant_production_cos_ratings_2, vars = production.company)
ordered_run_ratings_2 <- num_sig_comps_ratings_2[order(-num_sig_comps_ratings_2$n),]
head(ordered_run_ratings_2)
