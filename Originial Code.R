setwd('C:/Users/CAM/Documents/MSBA/Fall/ST 541/Final Project/Movie_R_Project')
library(readr)
library(leaps)
library("stringr")
library(dplyr)
library(ggplot2)
library(ggplot)
library(car)

IMDb_movies <- read_csv("IMDb movies.csv")
ratings <- read_csv("IMDb ratings.csv")
names(ratings)<-str_replace_all(names(ratings), c("_" = "." , "," = "" ))
names(IMDb_movies)<-str_replace_all(names(IMDb_movies), c("_" = "." , "," = "" ))

budget <-IMDb_movies[!is.na(IMDb_movies$budget),]

dim