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




# Data Visualizations
colnames(USAMOVIESratings)
str(USAMOVIESratings)
g.
summary(USAMOVIESratings[, c("year", "duration", "production.company","budget.infl","worldwide.gross.infl","usa.gross.infl","profit.infl",            
                             "weighted.average.vote","males.allages.avg.vote","females.allages.avg.vote")])

table(IsItHorror)
table(IsItRomance)
table(IsItAction)
table(IsItComedy)
table(IsItDrama)

par(mfrow = c(1, 1))
hist(budget.infl, main = "Distribution of Budget", xlab = "budget", las= 1, xaxt="n")
#boxplot(budget.infl, data = USAMOVIESratings, main = "Box Plot of Budget", yaxt="n")
#axis(2, at=axTicks(2), labels=sprintf("$%s", axTicks(2)), las = 1)
axis(side = 1, at = seq(1000,400000000,100000), labels = seq(1000,400000000,100000))
# scale_y_continuous(label = scales::comma))
hist(profit.infl, main = "Distribution of Profit",
     xlab = "profit", las= 1)
#xlim = c(-174531755,7462749659)), breaks=c(-20000000, -10000000, 0, 100000000, 200000000, 300000000, 400000000, 500000000)
hist(avg.vote, main = "Distribution of Average Vote", xlab = "Average Vote", xlim = c(0,10), breaks = c(0,1,2,3,4,5,6,7,8,9,10), las= 1)
summary(USAMOVIES)
dim(USAMOVIES)

ggplot( data= USAMOVIESratings, aes(budget.infl))+ geom_histogram(col='blue', alpha=.2)+ labs(title="Spread of Movie Budget", x="Budget", y="Frequency")
#xlim(c(--111000000,256000000))


qplot(budget.infl, geom="histogram",main = "Histogram for Movie Budget", xlab="Budget", alpha = I(.2))

p<-ggplot(USAMOVIESratings, aes(x=budget.infl)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") + 
  scale_x_continuous(breaks=seq(0, 10, 0.5)+scale_y_continuous(label = scales::comma))
p+ geom_vline(aes(xintercept=mean(budget.infl)),
              color="blue", linetype="dashed", size=1) + ggtitle("Spread of Movie Budget")


ggplot(USAMOVIESratings, aes(x=profit.infl)) + 
  geom_histogram(colour="black", fill="white")+
  #geom_density(alpha=.2, fill="#FF6666") +
  scale_x_continuous(breaks=seq(0, 10, 0.5))
p+ geom_vline(aes(xintercept=mean(profit.infl)),
              color="blue", linetype="dashed", size=1) + ggtitle("Spread of Movie Profit")+ xlab("Budget")
#+ylab("Frequency")
#+scale_y_continuous(label = scales::comma)


