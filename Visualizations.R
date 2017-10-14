require(data.table)
library("arules")

rm(list=ls())
rating.fullpath <- "/home/niraj/Desktop/R/project_data/ratings.csv"
rating.table <- fread(rating.fullpath)
rating.table <- na.omit(rating.table)
rating.table <- unique(rating.table)
user_item_matrix <- as(split(rating.table[,"movieId"], rating.table[,"userId"]), "transactions")

# Build ratings distribution
require('ggplot2')
require(scales)
library(RColorBrewer)

# with faceting  

ratings2 = read.csv(rating.fullpath,
                    colClasses = c("NULL","NULL","double","NULL"),
                    sep=",",
                    stringsAsFactors = TRUE)


ggplot(ratings2, aes(x = rating, fill = rating) ) +   
  geom_bar( aes(y = ..count..*100/sum(..count..) ) ) + 
  xlab("Rating") +
  ylab("Percentage")+
  labs( title = '\tMovies ratings distribution per ratings\n')

unique(movie.table$genres)

require(data.table)
require('ggplot2')
require(scales)
library(splitstackshape)

movie.fullpath <- "/home/niraj/Desktop/R/project_data/movies.csv"
movie.table <- fread(movie.fullpath)

# Don't care about movie title while merging it just takes more space
movie.table$title <- NULL
movie.table <- cSplit(movie.table, "genres", "|", "long")
movie.table <- movie.table[movie.table$genres != '(no genres listed)', ]

# with faceting  
ggplot(movie.table, aes(x = genres, fill = genres) ) +   
  geom_bar( aes(y = ..count..*100/sum(..count..) ) ) +  
  theme_minimal()+
  xlab("Genre") +
  ylab("Percentage")+
  labs( title = '\tMovies Distribution according to Genre (%)\n')

require(data.table)

rm(list=ls())
library(splitstackshape)

rating.fullpath <- "/home/niraj/Desktop/R/project_data/ratings.csv"
ratings = read.csv(rating.fullpath,
                   colClasses = c("integer","integer","NULL","NULL"),
                   sep=",",
                   stringsAsFactors = FALSE)
ratings <- na.omit(ratings)
rating.table <- data.table(ratings, key = "movieId")
remove(ratings)

movie.fullpath <- "/home/niraj/Desktop/R/project_data/movies.csv"
movie.table <- read.csv(movie.fullpath, sep=",",colClasses = c("integer","character","NULL"))
movie.table <- data.table(movie.table, key = "movieId")

ratings.merged <- merge(x = rating.table, y = movie.table, by= "movieId", all.x=TRUE)
rating.df <- as.data.frame(ratings.merged)

rating.df$movieId <- NULL
head(rating.df)

line.plot <- ggplot(agg.data.2013,aes(mo,amount))+
  geom_line(aes(color="2013 Sales"))+
  xlab('Months')+
  ylab('Amount')+
  labs(color="Sales", title = '\tSales Line Chart for 2013 and 2014\n')
line.plot+theme(panel.background = element_rect(fill = alpha ('green', 0.3), 
                                                colour = 'black'),
                axis.text=element_text(size=12),
                axis.title=element_text(size=15),
                legend.text=element_text(size=12),
                legend.title=element_text(size=12),
                plot.title=element_text(size=17,face="bold")
)


require(data.table)
require('ggplot2')
require(scales)
library(splitstackshape)

movie.fullpath <- "/home/niraj/Desktop/R/project_data/movies.csv"
movie.table <- fread(movie.fullpath)

# Don't care about movie title while merging it just takes more space
movie.table$title <- NULL
movie.table <- cSplit(movie.table, "genres", "|", "long")
movie.table <- movie.table[movie.table$genres != '(no genres listed)', ]

# with faceting  
ggplot(movie.table, aes(x = genres, fill = genres) ) +   
  geom_bar( aes(y = ..count..*100/sum(..count..) ) ) +  
  theme_minimal()+
  xlab("Genre") +
  ylab("Percentage")+
  labs( title = '\tMovies Distribution according to Genre (%)\n')

require(data.table)
require('ggplot2')

rm(list=ls())
library(splitstackshape)

rating.fullpath <- "/home/niraj/Desktop/R/project_data/ratings.csv"
ratings = read.csv(rating.fullpath,
                   colClasses = c("integer","integer","NULL","NULL"),
                   sep=",",
                   stringsAsFactors = FALSE)
ratings <- na.omit(ratings)
rating.table <- data.table(ratings, key = "movieId")
remove(ratings)

movie.fullpath <- "/home/niraj/Desktop/R/project_data/movies.csv"
movie.table <- read.csv(movie.fullpath, sep=",",colClasses = c("integer","NULL","character"))
movie.table <- data.table(movie.table, key = "movieId")
movie.table <- cSplit(movie.table, "genres", "|", "long")
ratings.merged <- rating.table[movie.table, on = "movieId", allow.cartesian = TRUE ]


ratings.df <- as.data.frame(ratings.merged)
head(ratings.df)
ratings.df$movieId <- NULL 
# with faceting  
ggplot(ratings.df, aes(x = genres, fill = userId) ) +   
  geom_bar( aes(y = ..count..*100/sum(..count..) ) ) +  
  theme_minimal()+
  xlab("Genre") +
  ylab("Percentage")+
  labs( title = '\tMovies Distribution according to Genre (%)\n')
# bubblechart
# Ref: http://t-redactyl.io/blog/2016/02/creating-plots-in-r-using-ggplot2-part-6-weighted-scatterplots.html

bubble.chart.data <- aggregate(number~movieId+userId, ratings.df, FUN=sum)

bubble.chart <- ggplot(ratings.df, aes(x = movieId, y = userId, size = userId, fill = movieId)) +
  geom_point(shape = 21) +
  ggtitle("Sales Data in different states for 2014") +
  labs(x = "State", y = "Amount")+
  scale_size(range = c(1, 17))
bubble.chart + theme(panel.background = element_rect(fill = alpha ('green', 0.3), 
                                                     colour = 'black'),
                     axis.text=element_text(size=12),
                     axis.title=element_text(size=15),
                     legend.text=element_text(size=12),
                     legend.title=element_text(size=12),
                     plot.title=element_text(size=17,face="bold")
)
