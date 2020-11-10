#change to whatever your wd is
movies <- read.csv("IMDb_movies.csv")
#library that adds sql-like functionality
library(dplyr)

#create Currency column from first 3 characters in budget column
movies$Currency <- substr(movies$budget, start = 1, stop = 3)
#line displaying currency values before cleaning
distinct(substr(movies$budget, start = 1, stop = 3))

#international movies begin their budget with three characters for their currency
#US films begin with '$ '


#change Currency column for US Films to indicate the currency is USD
for(value in 1:85855){
  if(substr(movies$Currency[value], start = 1, stop = 1)=="$"){
      movies$Currency[value] = "USD"
    }
}

#CSV with historical conversion rates for global currencies
conversion_rates <- read.csv('conversion_rates.csv')

#smaller conversion table if we want to just use monthly historic records
cv_month <- conversion_rates[, grep("01$", colnames(conversion_rates))]