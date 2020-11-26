options(max.print=10000)

library(readr)
library(leaps)
library("stringr")
library(dplyr)

#Use the USAMOVIES file from the Original Code file.

production.comp.analysis <- USAMOVIES[c(3,12, 15, 29, 33)]
attach(production.comp.analysis)
#detach(production.comp.analysis)

# Finding the production companies with the most movies
num_prod_co_groups <- count(group_by(production.comp.analysis,production.company))
num_prod_co_groups <- arrange(num_prod_co_groups,-n)

top_companies_list <- num_prod_co_groups[num_prod_co_groups$n > 50,]

top_quarter_prod_cos <- num_prod_co_groups[ num_prod_co_groups$n > quantile(num_prod_co_groups$n , 0.75 ) , ]
others <- num_prod_co_groups[ num_prod_co_groups$n <= quantile(num_prod_co_groups$n , 0.75 ) , ]

top_companies <- production.comp.analysis[production.company %in% top_companies_list$production.company,]

attach(top_companies)

# Testing the significance of each production company compared to profit 
prod_co_mod <- lm(profit~production.company)
summary(prod_co_mod)

# Model Summary
p-value = 2.2e^-16
Adjusted R^2 = 0.04233

# Collecting the significant companies
significant_production_cos_profit <- subset(production.comp.analysis,summary(prod_co_mod)$coeff[-1,4] < 0.05)
vars <- rownames(summary(prod_co_mod)$coefficients)[which(summary(prod_co_mod)$coefficients[-1, 4] < 0.05)]
significant.productions <- production.comp.analysis[production.company %in% vars,]

num_sig_comps_profit <- count(vars, production.company)
ordered_run_profit <- num_sig_comps_profit[order(-num_sig_comps_profit$n),]
head(ordered_run_profit)
  
# Testing the significance of each production company compared to profit (including inflation)
prod_infl_co_mod <- lm(profit.infl~production.company)
summary(prod_infl_co_mod)

# Model Summary
p-value = 3.359e^-16 
Adjusted R^2 = 0.04246

# Collecting the significant companies
significant_production_cos_profit_infl <- subset(prod.co.analysis,summary(prod_infl_co_mod)$coeff[-1,4] < 0.05)
num_sig_comps_profit_infl <- count(significant_production_cos_profit_infl, vars = production.company)
ordered_run_profit_infl <- num_sig_comps_profit_infl[order(-num_sig_comps_profit_infl$n),]
head(ordered_run_profit_infl)


# Taking out negative profits for comparison
pos.profit <- top_companies[top_companies$profit > 0,]
# Model for profit
profit.model <- lm(pos.profit$profit~pos.profit$production.company)
summary(profit.model)

p-value = 0.001809
R^2 Adjusted = 0.025

# Model for profit with inflation
profit.infl.model <- lm(pos.profit$profit.infl~pos.profit$production.company)
summary(profit.infl.model)

p-value = 5.568e^-05
R^2 Adjusted = 0.3676

# Production Company vs Ratings
prod_ratings <- lm(top_companies$avg.vote ~ top_companies$production.company)
summary(prod_ratings)

# Model Summary
p-value = <2.2e^-16
Adjusted R^2 = 0.1912

# Collecting the significant companies




