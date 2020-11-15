options(max.print=250)

library(dplyr)
# Loading and subsetting the data
USAMOVIES_INFL <- read.csv("~/Desktop/Mary's Stuff/GitHub/Movie_R_Project/USAMOVIES_INFL.csv")
prod.co.analysis <- USAMOVIES_INFL[c(7,16, 19, 32)]
attach(prod.co.analysis)

# Testing the significance of each production company compared to profit (including inflation)
prod_co_mod <- lm(profit_infl~production.company)
summary(prod_co_mod)
# Model Summary
p-value = 0.0001483 Adjusted R^2 = 0.04933

# Collecting the significant companies
significant_production_cos <- subset(prod.co.analysis,summary(prod_co_mod)$coeff[-1,4] < 0.05)



num_movies_by_prod_co <- group_by(significant_production_cos, aggregation = 'sum')

first_run <- count(significant_production_cos, vars = production.company)
ordered_run <- first_run[order(-first_run$n),]
  
 
# Rename values from one column given a condition
df[which(df[,2]=="red"),1] <- "yellow"

# Combining Rows in a dataset
library(data.table)
DT <- data.table(DF)  # DF is your original data
DT[, lapply(.SD, sum), by=list(season, lic, id, vessel)]