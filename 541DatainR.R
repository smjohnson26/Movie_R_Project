library(readr)
IMDb_movies <- read_csv("./IMDb movies.csv", budget = col_double(), usa_gross_income = col_double(), worlwide_gross_income = col_double(), metascore = col_double())
USAMOVIES <- read_csv("USAMOVIES.csv")

str(USAMOVIES)
summary(USAMOVIES)

#Does the budget have an effect on the profit margins, total gross, and user ratings of the movie?
#How are ratings affected by movie genre, movie duration, gender, or age group?
#Are production companies a significant variable when discussing the success of a movie?
#What is the best way to predict the profit margin of a movie?
#What is the comparison of international movies to movies made in America by market share and has that trend evolved over the years?