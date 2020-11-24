setwd('C:/Users/CAM/Documents/MSBA/Fall/ST 541/Final Project/Movie_R_Project')
library(readr)
library(leaps)
library("stringr")
library(ggplot2)
library(car)

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
usamovies<-budget[!is.na(budget$usa.gross.income),]#worldwide
usamovies1<-usamovies[!is.na(usamovies$country),]
#usamovies2<-subset(usamovies1, usamovies1$country== "USA")
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


year21<-subset(USAMOVIESratings, year>=2000)
posprof<- subset(year21, profit.infl>0)
negprof<- subset(year21, profit.infl<=0)


hist(trans)


#What is the best way to predict the profit margin of a movie?--Collins

logprofitpos<- log(posprof$profit.infl)
absprofit<- abs(negprof$profit.infl)
logprofitneg<- log(absprofit)
hist(logprofitneg)

fullmodpos<- lm(logprofitpos~ year+ duration +avg.vote+votes+reviews.from.users+reviews.from.critics+IsItHorror+IsItRomance+IsItAction+IsItComedy+IsItDrama+mean.vote+median.vote+males.allages.avg.vote+males.allages.votes+females.allages.avg.vote+females.allages.votes+top1000.voters.rating+top1000.voters.votes+us.voters.rating+us.voters.votes+non.us.voters.rating+non.us.voters.votes, data=negprofit)
summary(fullmodprofitinfl)
fullmodneg<- lm(logprofitneg~ year+ duration +avg.vote+votes+reviews.from.users+reviews.from.critics+IsItHorror+IsItRomance+IsItAction+IsItComedy+IsItDrama+mean.vote+median.vote+males.allages.avg.vote+males.allages.votes+females.allages.avg.vote+females.allages.votes+top1000.voters.rating+top1000.voters.votes+us.voters.rating+us.voters.votes+non.us.voters.rating+non.us.voters.votes, data=negprofit)
summary(fullmodprofitinfl)

#####get to work
allinputs<-cbind(year,duration,avg.vote,votes,reviews.from.users,reviews.from.critics,IsItHorror,IsItRomance,IsItAction,IsItComedy,IsItDrama,weighted.average.vote,mean.vote,median.vote,males.allages.avg.vote,males.allages.votes,females.allages.avg.vote,females.allages.votes,top1000.voters.rating,top1000.voters.votes,us.voters.rating,us.voters.votes,non.us.voters.rating,non.us.voters.votes, data=posprof)
best <- regsubsets(as.matrix(allinputs), logprofit)
summary(best)


###BACK AIC
  
  ##inflation
  backAICinfl <- step(fullmodprofitinfl,direction="backward", data=posprof)
  backAICinfl$coefficients
  backAICmodelinfllog<-lm(logprofit ~ votes + reviews.from.users + reviews.from.critics + 
    IsItHorror + IsItDrama + mean.vote + males.allages.avg.vote + 
    males.allages.votes + females.allages.avg.vote + females.allages.votes + 
    top1000.voters.rating + top1000.voters.votes + non.us.voters.rating, data=USAMOVIESratings)
  summary(backAICmodelinfllog)

  #AIC= -9701.66
  #R^2= 0.406  
  
  trial<-lm(logprofit ~ duration + reviews.from.critics + IsItAction + top1000.voters.votes + 
              us.voters.rating + non.us.voters.votes, data=negprofit)
  summary(trial)



###BACK BIC
  ninfl <- length(fullmodprofitinfl$residuals)
  ##inflation
  backBICinfl <- step(fullmodprofitinfl,direction="backward", data=USAMOVIESratings, k=log(ninfl))
  backBICinfl$coefficients
  backBICmodelinfllog<-lm(logprofit ~ votes + reviews.from.critics + IsItHorror + IsItDrama + 
                         mean.vote + males.allages.votes + females.allages.avg.vote + 
                         females.allages.votes + top1000.voters.votes + non.us.voters.rating, data=USAMOVIESratings)
  summary(backBICmodelinfllog)
  #BIC= -9619.72
  #R^2=  0.4047 

  
###FORWARD AIC 
  interceptinfl <- lm(logprofit~1,data=USAMOVIESratings)
  
  forwardAICinfl <- step(interceptinfl,scope=list(lower=~1,upper=~year+ duration +avg.vote+votes+reviews.from.users+reviews.from.critics+IsItHorror+IsItRomance+IsItAction+IsItComedy+IsItDrama+mean.vote+median.vote+males.allages.avg.vote+males.allages.votes+females.allages.avg.vote+females.allages.votes+top1000.voters.rating+top1000.voters.votes+us.voters.rating+us.voters.votes+non.us.voters.rating+non.us.voters.votes),direction="forward", data=USAMOVIESratings)
  forwardAICinfl$coefficients
  forwardAICmodelinfl<-lm(logprofit ~ top1000.voters.votes + females.allages.votes + IsItDrama + 
                            reviews.from.users + non.us.voters.votes + votes + males.allages.votes + 
                            IsItHorror + reviews.from.critics + non.us.voters.rating + 
                            females.allages.avg.vote + mean.vote + top1000.voters.rating + 
                            males.allages.avg.vote,data=USAMOVIESratings)
  summary(forwardAICmodelinfl)
  
  #AIC= -9699.8
  #R^2=0.4059
  
  
###FORWARD BIC 

  forwardBICinfl <- step(interceptinfl,scope=list(lower=~1,upper=~year+ duration +avg.vote+votes+reviews.from.users+reviews.from.critics+IsItHorror+IsItRomance+IsItAction+IsItComedy+IsItDrama+mean.vote+median.vote+males.allages.avg.vote+males.allages.votes+females.allages.avg.vote+females.allages.votes+top1000.voters.rating+top1000.voters.votes+us.voters.rating+us.voters.votes+non.us.voters.rating+non.us.voters.votes),direction="forward", data=USAMOVIESratings,k=log(ninfl))
  forwardBICinfl$coefficients
  forwardBICmodelinfllog<-lm(logprofit ~ top1000.voters.votes + females.allages.votes + IsItDrama + 
                            reviews.from.users + non.us.voters.votes + votes + males.allages.votes + 
                            IsItHorror + reviews.from.critics,data=USAMOVIESratings)
  summary(forwardBICmodelinfllog)
  
  #BIC= -9531.61
  #R^2= 0.3943 
  
###STEP
  #AIC
  StepAIC <- step(fullmodprofitinfl,direction="both", data=negprofit)
  StepAICmodelinfl<-lm(logprofit ~ votes + reviews.from.users + reviews.from.critics + 
                         IsItHorror + IsItDrama + mean.vote + males.allages.avg.vote + 
                         males.allages.votes + females.allages.avg.vote + females.allages.votes + 
                         top1000.voters.rating + top1000.voters.votes + non.us.voters.rating,data=USAMOVIESratings)
  summary(StepAICmodelinfl)
    #AIC= -9701.66
    #R^2= 0.406 
  
  #BIC
  StepBIC <- step(fullmodprofitinfl,direction="both", data=posprof, k=log(ninfl))
  StepBICmodelinfl<-lm(logprofit ~ votes + reviews.from.critics + IsItHorror + IsItDrama + 
                         mean.vote + males.allages.votes + females.allages.avg.vote + 
                         females.allages.votes + top1000.voters.votes + non.us.voters.rating,data=USAMOVIESratings)
  summary(StepBICmodelinfl)
    #BIC= -9619.72
    #R^2= 0.4047   
    
 
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
#Original Code/Models without Log  
  
  backAICmodelinfl<- lm(profit.infl ~ year + duration + avg.vote + votes + reviews.from.users + 
                          reviews.from.critics + IsItHorror + IsItAction + IsItComedy + 
                          IsItDrama + mean.vote + males.allages.votes + females.allages.avg.vote + 
                          females.allages.votes + top1000.voters.rating + top1000.voters.votes + 
                          us.voters.rating + us.voters.votes + non.us.voters.rating, data=USAMOVIESratings)
  #AIC= 210847.5
  #R^2= 0.3937 
  backBICmodelinfl<-lm(profit.infl ~ year + duration + avg.vote + votes + reviews.from.users + 
                         reviews.from.critics + IsItHorror + IsItDrama + mean.vote + 
                         males.allages.votes + females.allages.avg.vote + females.allages.votes + 
                         top1000.voters.votes + us.voters.votes, data=USAMOVIESratings)
  summary(backBICmodelinfl)
  #BIC= 210951.8
  #R^2=  0.3927  
  
  forwardAICmodelinfl<-lm(profit.infl ~ votes + males.allages.votes + top1000.voters.votes + 
                            non.us.voters.votes + IsItDrama + duration + year + reviews.from.users + 
                            IsItHorror + reviews.from.critics + us.voters.votes + non.us.voters.rating + 
                            females.allages.avg.vote + females.allages.votes + IsItComedy + 
                            avg.vote + mean.vote + top1000.voters.rating + IsItAction + 
                            us.voters.rating,data=USAMOVIESratings)
  summary(forwardAICmodelinfl)
  forwardBICmodelinfl<-lm(profit.infl ~ votes + males.allages.votes + top1000.voters.votes + 
                            non.us.voters.votes + IsItDrama + duration + year + reviews.from.users + 
                            IsItHorror + reviews.from.critics + us.voters.votes + non.us.voters.rating + 
                            females.allages.avg.vote + females.allages.votes,data=USAMOVIESratings)
  summary(forwardBICmodelinfl)
  
  #BIC= 210970.3
  #R^2= 0.3906 
  
  #AIC= 210849
  #R^2=0.3937
  StepAICmodelinfl<-lm(profit.infl ~ year + duration + avg.vote + votes + reviews.from.users + 
                         reviews.from.critics + IsItHorror + IsItAction + IsItComedy + 
                         IsItDrama + mean.vote + males.allages.votes + females.allages.avg.vote + 
                         females.allages.votes + top1000.voters.rating + top1000.voters.votes + 
                         us.voters.rating + us.voters.votes + non.us.voters.rating,data=USAMOVIESratings)
  summary(StepAICmodelinfl)
  #AIC= 210847.5
  #R^2= 0.3937 
  StepBICmodelinfl<-lm(profit.infl ~ year + duration + avg.vote + votes + reviews.from.users + 
                         reviews.from.critics + IsItHorror + IsItDrama + mean.vote + 
                         males.allages.votes + females.allages.avg.vote + females.allages.votes + 
                         top1000.voters.votes + us.voters.votes,data=USAMOVIESratings)
  summary(StepBICmodelinfl)
  #BIC= 210951.8
  #R^2= 0.3927  
  
  
  

