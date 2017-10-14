require(data.table)
library(arules)
library(dplyr)
library(stringr)

rm(list=ls())
library(splitstackshape)

rating.fullpath <- "ratings.csv"
ratings = read.csv(rating.fullpath,
                   colClasses = c("integer","integer","NULL","NULL"),
                   sep=",",
                   stringsAsFactors = FALSE)
ratings <- na.omit(ratings)
rating.table <- data.table(ratings, key = "movieId")
remove(ratings)

movie.fullpath <- "movies.csv"
movie.table <- read.csv(movie.fullpath, sep=",",colClasses = c("integer","character","NULL"))
movie.table <- data.table(movie.table, key = "movieId")

ratings.merged <- merge(x = rating.table, y = movie.table, by= "movieId", all.x=TRUE)
rating.df <- as.data.frame(ratings.merged)

rating.df$movieId <- NULL
head(rating.df)

movie_transactions <- as(split(rating.df[,"title"], rating.df[,"userId"]), "transactions")

rules <- apriori(movie_transactions, parameter = list(support = 0.001,
                              confidence = 0.9, maxlen=2))
assoc_rules = as(rules,"data.frame")

head(assoc_rules)

rules = as.character(assoc_rules$rules)
rules = gsub("=>",",",rules)
rules = gsub("\\{","",rules)
rules = gsub("\\}","",rules)
head(rules)

rules = str_split(rules,",")
assoc_rules$lhs_movie = sapply( rules,  head, n = 1)
assoc_rules$rhs_movie = sapply( rules ,  tail, n = 1)
head(assoc_rules)
assoc_rules$rules = NULL

# http://stackoverflow.com/a/24821141/5916727
filter(assoc_rules, grepl('Cargo', lhs_movie))
