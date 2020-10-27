library(readr)
IMDb_movies <- read_csv("./IMDb movies.csv", budget = col_double(), usa_gross_income = col_double(), worlwide_gross_income = col_double(), metascore = col_double())
