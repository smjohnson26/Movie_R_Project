USAMOVIES <- read.csv("C:/Users/balli/Documents/Movie_R_Project/USAMOVIES.csv")
attach(USAMOVIES)
How are ratings affected by movie genre, movie duration, gender, or age group?
dim(USAMOVIES)
length(grep("Drama",USAMOVIES$genre))
length(grep("Comedy",USAMOVIES$genre))
length(grep("Action",USAMOVIES$genre))
length(grep("Horror",USAMOVIES$genre))
length(grep("Romance",USAMOVIES$genre))
USAMOVIES1 <- USAMOVIES
detach(USAMOVIES)
attach(USAMOVIES1)





USAMOVIES1["IsItHorror"]<- 0
USAMOVIES1["IsItRomance"] <- 0
USAMOVIES1["IsItAction"] <- 0
USAMOVIES1["IsItComedy"] <- 0
USAMOVIES1["IsItDrama"] <- 0
USAMOVIES1$IsItHorror[grep("Horror",USAMOVIES1$genre)] <- 1
USAMOVIES1$IsItRomance[grep("Romance",USAMOVIES1$genre)] <- 1
USAMOVIES1$IsItAction[grep("Action",USAMOVIES1$genre)] <- 1
USAMOVIES1$IsItComedy[grep("Comedy",USAMOVIES1$genre)] <- 1
USAMOVIES1$IsItDrama[grep("Drama",USAMOVIES1$genre)] <- 1


