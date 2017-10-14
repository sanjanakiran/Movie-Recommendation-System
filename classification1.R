# Instead of using data frame we use data.table which is similar and advanced form of dataframe 
# The purpose here is we will run out of memory using dataframe
# For more information: https://www.analyticsvidhya.com/blog/2016/05/data-table-data-frame-work-large-data-sets/
  
  require(data.table)

# This library is used to split column values into different rows
# If you have not installed you may need to install it

# install.packages("splitstackshape")

library(splitstackshape)

# filepath for movies.csv and ratings.csv
rating.fullpath <- "ratings.csv"
movie.fullpath <- "movies.csv"

# Now the approach can be divided into two parts:
#     1. Classify each user to genre depending on majority of movies of certain genre user watched
#        Also track user's watched movie list, it belongs to genre or not so that we do not 
#        refer user same movie user had watched already
#     2. Group user each genre movies, sort them by average score given by users and these movies are picked to 
#        recommend each user by comparing top-k movies that do not belong to the user movie list

# ****************** Start of PART 1 ***********************************************************************
# read movies data and rating data fread is fast or else we run into trouble
rating.table <- fread(rating.fullpath)
movie.table <- fread(movie.fullpath)

# Don't care about movie title while merging it just takes more space
movie.table$title <- NULL
# Don't care about movie title while merging it just takes more space
rating.table$timestamp <- NULL

# now split on the genre to see user's majority likes
movie.table <- cSplit(movie.table, "genres", "|", "long")
movie.table <- movie.table[movie.table$genres != '(no genres listed)', ]

# merge movie and ratings on movieId
# Important point to note is we have split movie genre into multiple rows in movie.table
# So, we need to allow cartesian because movieId one to one mapping will cause lost of movies
classification.table <- rating.table[movie.table, on = "movieId", allow.cartesian = TRUE ]

# free up some memory since we do not need those tables anymore
remove(rating.table)
remove(movie.table)  


# This extra column is track the count of movies in each genre which is later used to observe
# the majority of the movies user watched For e.g. 
# userId    genre      counter
#   1       drama        1
#   1       action       1
#   1       action       1

# Now if we sum counter depending on genre we get
# userId    genre      counter
#   1       action        2
#   1       drama         1

# Note: This might be redundant and there may be better way to track it, this is just for easiness
# This will take some time 
classification.table$counter <- 1

# need to store movie ids for each user to track list of movies user saw in one column
# For all user with specific genre I want to merge to avg score and preserve # number of movies
# can be useful to count or threshold
classification.table <- classification.table[, list(avg_score=mean(rating), num_genre_movies = sum(counter), movie_list = list(movieId)), by = c('genres','userId')]

# prune less than 2 count, user must have atleast watch 2 movies from category using support logic
# this might help to some extent to lower possible records
classification.table <- classification.table[classification.table$num_genre_movies>1,]

# label user with majority of movie genre watched
classification.table <- classification.table[ , .SD[which.max(num_genre_movies)], by = c('userId')]

# Just viewing few contents to see how it looks
classification.table[10, movie_list]
# Now that should satisfy the part 1 requirements
# Note: Need to figure out how to separate training and test data 

# ------------------ End of PART 1 ----------------------------------------------------------------------

# ****************** Start of PART 2 ***********************************************************************

# Quick Reminder: # Group movies in 19 different genre and sort them in average score decreasing order
#                   this decreasing score is used to recommend movies for the user
#                   use k movies that does not exist in user list
# read movies data and rating data fread is fast or else we run into trouble
rating.table <- fread(rating.fullpath)
movie.table <- fread(movie.fullpath)

# Don't care about movie title while merging it just takes more space
rating.table$timestamp <- NULL
# Don't care about userId as well
rating.table$userId <- NULL

# now split on the genre to pure form
movie.table <- cSplit(movie.table, "genres", "|", "long")
movie.table <- movie.table[movie.table$genres != '(no genres listed)', ]

# merge movie and ratings on movieId
movie.group.table <- rating.table[movie.table, on = "movieId", allow.cartesian = TRUE ]
# free up some memory
remove(rating.table)
remove(movie.table)  

# For all movies we need movieId, genre, movieTitle, averageScore
movie.group.table <- movie.group.table[, list(avg_score=mean(rating)), by = c('genres', 'movieId', 'title')]

# view all possible genre possible
# unique(movie.group.table$genres)

# Adventure   
# Animation   
# Children    
# Comedy      
# Fantasy     
# Romance     
# Drama       
# Action      
# Crime      
# Thriller    
# Horror      
# Mystery     
# Sci-Fi      
# IMAX        
# Documentary War         
# Musical     
# Western    
# Film-Noir 

# These text are created for easiness purpose only since being checked multiple times
text.Adventure <- "Adventure"  
text.Animation <- "Animation" 
text.Children <- "Children"    
text.Comedy <- "Comedy"      
text.Fantasy <- "Fantasy"     
text.Romance <- "Romance"     
text.Drama <- "Drama"       
text.Action <- "Action"      
text.Crime <- "Crime"      
text.Thriller <- "Thriller"    
text.Horror <- "Horror"      
text.Mystery <- "Mystery"     
text.SciFi <- "Sci-Fi"      
text.IMAX <- "IMAX"        
text.Documentary <- "Documentary"
text.War <- "War"         
text.Musical <- "Musical"     
text.Western <- "Western"    
text.FilmNoir <- "Film-Noir" 

#i<-as.factor
#for (i  in g)
#{
 # assign(paste("ddat",i,sep="_"), i)
#}







# Also removing movies whose score are missing in rating found not all movies have rating
# One way to handle NA value is to remove them using na.omit() function
# There is so much redundant code, try creating function and pass appropiate paramater

# Create dataframe sorted by score for each genre 
movie.Adventure <- movie.group.table[movie.group.table$genres == text.Adventure,]
movie.Adventure <- na.omit(movie.Adventure)
movie.Adventure <- movie.Adventure[order(movie.Adventure$avg_score, decreasing = TRUE),]
movie.Adventure <- as.data.frame.matrix(movie.Adventure)


#for (i  in unique(movie.group.table$genres))
#{
 # nam <- paste("Text", i, sep = ".")
 #assign(nam,i)
 # nam1 <- paste("Movie", i, sep = ".")
 #x<-na.omit(assign(nam1,movie.group.table[movie.group.table$genres == assign(nam,i),]))
 # x<- x[order(x$avg_score, decreasing = TRUE),]
 #x<-as.data.frame.matrix(movie.Adventure)
  #assign(nam,x)
#}


# Create dataframe sorted by score for each genre 
movie.Animation <- movie.group.table[movie.group.table$genres == text.Animation,]
movie.Animation <- na.omit(movie.Animation)
movie.Animation <- movie.Animation[order(movie.Animation$avg_score, decreasing = TRUE),]
movie.Animation <- as.data.frame.matrix(movie.Animation)

# Create dataframe sorted by score for each genre 
movie.Children <- movie.group.table[movie.group.table$genres == text.Children,]
movie.Children <- na.omit(movie.Children)
movie.Children <- movie.Children[order(movie.Children$avg_score, decreasing = TRUE),]
movie.Children <- as.data.frame.matrix(movie.Children)


# Create dataframe sorted by score for each genre 
movie.Comedy <- movie.group.table[movie.group.table$genres == text.Comedy,]
movie.Comedy <- na.omit(movie.Comedy)
movie.Comedy <- movie.Comedy[order(movie.Comedy$avg_score, decreasing = TRUE),]
movie.Comedy <- as.data.frame.matrix(movie.Comedy)



# Create dataframe sorted by score for each genre 
movie.Fantasy <- movie.group.table[movie.group.table$genres == text.Fantasy,]
movie.Fantasy <- na.omit(movie.Fantasy)
movie.Fantasy <- movie.Fantasy[order(movie.Fantasy$avg_score, decreasing = TRUE),]
movie.Fantasy <- as.data.frame.matrix(movie.Fantasy)

# Create dataframe sorted by score for each genre 
movie.Romance <- movie.group.table[movie.group.table$genres == text.Romance,]
movie.Romance <- na.omit(movie.Romance)
movie.Romance <- movie.Romance[order(movie.Romance$avg_score, decreasing = TRUE),]
movie.Romance <- as.data.frame.matrix(movie.Romance)

# Create dataframe sorted by score for each genre 
movie.Drama <- movie.group.table[movie.group.table$genres == text.Drama,]
movie.Drama <- na.omit(movie.Drama)
movie.Drama <- movie.Drama[order(movie.Drama$avg_score, decreasing = TRUE),]
movie.Drama <- as.data.frame.matrix(movie.Drama)

# Create dataframe sorted by score for each genre 
movie.Action <- movie.group.table[movie.group.table$genres == text.Action,]
movie.Action <- na.omit(movie.Action)
movie.Action <- movie.Action[order(movie.Action$avg_score, decreasing = TRUE),]
movie.Action <- as.data.frame.matrix(movie.Action)

# Create dataframe sorted by score for each genre 
movie.Crime <- movie.group.table[movie.group.table$genres == text.Crime,]
movie.Crime <- na.omit(movie.Crime)
movie.Crime <- movie.Crime[order(movie.Crime$avg_score, decreasing = TRUE),]
movie.Crime <- as.data.frame.matrix(movie.Crime)

# Create dataframe sorted by score for each genre 
movie.Thriller <- movie.group.table[movie.group.table$genres == text.Thriller,]
movie.Thriller <- na.omit(movie.Thriller)
movie.Thriller <- movie.Thriller[order(movie.Thriller$avg_score, decreasing = TRUE),]
movie.Thriller <- as.data.frame.matrix(movie.Thriller)

# Create dataframe sorted by score for each genre 
movie.Horror <- movie.group.table[movie.group.table$genres == text.Horror,]
movie.Horror <- na.omit(movie.Horror)
movie.Horror <- movie.Horror[order(movie.Horror$avg_score, decreasing = TRUE),]
movie.Horror <- as.data.frame.matrix(movie.Horror)



# Create dataframe sorted by score for each genre 
movie.Mystery <- movie.group.table[movie.group.table$genres == text.Mystery,]
movie.Mystery <- na.omit(movie.Mystery)
movie.Mystery <- movie.Mystery[order(movie.Mystery$avg_score, decreasing = TRUE),]
movie.Mystery <- as.data.frame.matrix(movie.Mystery)

# Create dataframe sorted by score for each genre 
movie.SciFi <- movie.group.table[movie.group.table$genres == text.SciFi,]
movie.SciFi <- na.omit(movie.SciFi)
movie.SciFi <- movie.SciFi[order(movie.SciFi$avg_score, decreasing = TRUE),]
movie.SciFi <- as.data.frame.matrix(movie.SciFi)

# Create dataframe sorted by score for each genre 
movie.IMAX <- movie.group.table[movie.group.table$genres == text.IMAX,]
movie.IMAX <- na.omit(movie.IMAX)
movie.IMAX <- movie.IMAX[order(movie.IMAX$avg_score, decreasing = TRUE),]
movie.IMAX <- as.data.frame.matrix(movie.IMAX)

# Create dataframe sorted by score for each genre 
movie.Documentary <- movie.group.table[movie.group.table$genres == text.Documentary,]
movie.Documentary <- na.omit(movie.Documentary)
movie.Documentary <- movie.Documentary[order(movie.Documentary$avg_score, decreasing = TRUE),]
movie.Documentary <- as.data.frame.matrix(movie.Documentary)

# Create dataframe sorted by score for each genre 
movie.War <- movie.group.table[movie.group.table$genres == text.War,]
movie.War <- na.omit(movie.War)
movie.War <- movie.War[order(movie.War$avg_score, decreasing = TRUE),]
movie.War <- as.data.frame.matrix(movie.War)

# Create dataframe sorted by score for each genre 
movie.Musical <- movie.group.table[movie.group.table$genres == text.Musical,]
movie.Musical <- na.omit(movie.Musical)
movie.Musical <- movie.Musical[order(movie.Musical$avg_score, decreasing = TRUE),]
movie.Musical <- as.data.frame.matrix(movie.Musical)


# Create dataframe sorted by score for each genre 
movie.Western <- movie.group.table[movie.group.table$genres == text.Western,]
movie.Western <- na.omit(movie.Western)
movie.Western <- movie.Western[order(movie.Western$avg_score, decreasing = TRUE),]
movie.Western <- as.data.frame.matrix(movie.Western)


# Create dataframe sorted by score for each genre 
movie.FilmNoir <- movie.group.table[movie.group.table$genres == text.FilmNoir,]
movie.FilmNoir <- na.omit(movie.FilmNoir)
movie.FilmNoir <- movie.FilmNoir[order(movie.FilmNoir$avg_score, decreasing = TRUE),]
movie.FilmNoir <- as.data.frame.matrix(movie.FilmNoir)


# ------------------ End of PART 2 ----------------------------------------------------------------------
# ****************** Simple Testing, Let's see what we can recommend ************************************

# Given userId, genre user likes, movie list user has watched find top-k movies from the genre 
# which user has not watched and show titles and average score for the user to recommend

test_user <- classification.table[10,c("userId", "genres", "movie_list")]
user.genre.label <- test_user$genres
k <-  2
# Using if else loop depending on genre user has been labelled we need to check respective dataframe

recommendations <- data.frame(Title=character(),MovieId=numeric(),Score=double()) 

if (user.genre.label == text.Adventure)
{
  
  for (id in movie.Adventure$movieId)
  {
    if (!(id %in% test_user$movie_list)) 
    { 
      record <- movie.Adventure [movie.Adventure$movieId == id, ]
      newRow <- data.frame(Title = record$title, MovieId = record$movieId, Score =record$avg_score)
      recommendations <- rbind(recommendations, newRow)
    }
    if (nrow(recommendations) >= k) break
  }
} else if (user.genre.label == text.Animation) {
  for (id in movie.Animation$movieId)
  {
    if (!(id %in% test_user$movie_list)) 
    { 
      record <- movie.Animation[movie.Animation$movieId == id, ]
      newRow <- data.frame(Title = record$title, MovieId = record$movieId, Score =record$avg_score)
      recommendations <- rbind(recommendations, newRow)
    }
    if (nrow(recommendations) >= k) break
  }
  
} else if (user.genre.label == text.Children) {
  
  for (id in movie.Children$movieId)
  {
    if (!(id %in% test_user$movie_list)) 
    { 
      record <- movie.Children[movie.Adventure$movieId == id, ]
      newRow <- data.frame(Title = record$title, MovieId = record$movieId, Score =record$avg_score)
      recommendations <- rbind(recommendations, newRow)
    }
    if (nrow(recommendations) >= k) break
  }
  
} else if (user.genre.label == text.Comedy)   {
  for (id in movie.Comedy$movieId)
  {
    if (!(id %in% test_user$movie_list)) 
    { 
      record <- movie.Comedy[movie.Comedy$movieId == id, ]
      newRow <- data.frame(Title = record$title, MovieId = record$movieId, Score =record$avg_score)
      recommendations <- rbind(recommendations, newRow)
    }
    if (nrow(recommendations) >= k) break
    
  }
} else if (user.genre.label == text.Fantasy) {
  for (id in movie.Fantasy$movieId)
  {
    if (!(id %in% test_user$movie_list)) 
    { 
      record <- movie.Fantasy[movie.Fantasy$movieId == id, ]
      newRow <- data.frame(Title = record$title, MovieId = record$movieId, Score =record$avg_score)
      recommendations <- rbind(recommendations, newRow)
    }
    if (nrow(recommendations) >= k) break
    
  }
} else if (user.genre.label == text.Romance) {
  for (id in movie.Romance$movieId)
  {
    if (!(id %in% test_user$movie_list)) 
    { 
      record <- movie.Romance[movie.Romance$movieId == id, ]
      newRow <- data.frame(Title = record$title, MovieId = record$movieId, Score =record$avg_score)
      recommendations <- rbind(recommendations, newRow)
    }
    if (nrow(recommendations) >= k) break
    
  }
  
} else if (user.genre.label == text.Drama) {
  for (id in movie.Drama$movieId)
  {
    if (!(id %in% test_user$movie_list)) 
    { 
      record <- movie.Drama[movie.Drama$movieId == id, ]
      newRow <- data.frame(Title = record$title, MovieId = record$movieId, Score =record$avg_score)
      recommendations <- rbind(recommendations, newRow)
    }
    if (nrow(recommendations) >= k) break
    
  }  
} else if (user.genre.label == text.Action) {
  for (id in movie.Action$movieId)
  {
    if (!(id %in% test_user$movie_list)) 
    { 
      record <- movie.Action[movie.Action$movieId == id, ]
      newRow <- data.frame(Title = record$title, MovieId = record$movieId, Score =record$avg_score)
      recommendations <- rbind(recommendations, newRow)
    }
    if (nrow(recommendations) >= k) break
    
  }  
  
} else if (user.genre.label == text.Crime) {
  for (id in movie.Crime$movieId)
  {
    if (!(id %in% test_user$movie_list)) 
    { 
      record <- movie.Crime[movie.Crime$movieId == id, ]
      newRow <- data.frame(Title = record$title, MovieId = record$movieId, Score =record$avg_score)
      recommendations <- rbind(recommendations, newRow)
    }
    if (nrow(recommendations) >= k) break
  }  
} else if (user.genre.label == text.Thriller) {
  # Code for Thriller genre
  for (id in movie.Thriller$movieId)
  {
    if (!(id %in% test_user$movie_list))
    { 
      record <- movie.Thriller[movie.Thriller$movieId == id, ]
      newRow <- data.frame(Title = record$title, MovieId = record$movieId, Score = record$avg_score)
      recommendations <- rbind(recommendations, newRow)
    }
    if (nrow(recommendations) >= k) break
  }
  
} else if (user.genre.label == text.Horror) {
  for (id in movie.Horror$movieId)
  {
    if (!(id %in% test_user$movie_list)) 
    { 
      record <- movie.Horror[movie.Horror$movieId == id, ]
      newRow <- data.frame(Title = record$title, MovieId = record$movieId, Score =record$avg_score)
      recommendations <- rbind(recommendations, newRow)
    }
    if (nrow(recommendations) >= k) break
  }  
} else if (user.genre.label == text.Mystery) {
  for (id in movie.Mystery$movieId)
  {
    if (!(id %in% test_user$movie_list)) 
    { 
      record <- movie.Mystery[movie.Mystery$movieId == id, ]
      newRow <- data.frame(Title = record$title, MovieId = record$movieId, Score =record$avg_score)
      recommendations <- rbind(recommendations, newRow)
    }
    if (nrow(recommendations) >= k) break
  }  
} else if (user.genre.label == text.SciFi) {
  for (id in movie.SciFi$movieId)
  {
    if (!(id %in% test_user$movie_list)) 
    { 
      record <- movie.SciFi[movie.SciFi$movieId == id, ]
      newRow <- data.frame(Title = record$title, MovieId = record$movieId, Score =record$avg_score)
      recommendations <- rbind(recommendations, newRow)
    }
    if (nrow(recommendations) >= k) break
  } 
} else if (user.genre.label == text.IMAX) {
  for (id in movie.IMAX$movieId)
  {
    if (!(id %in% test_user$movie_list)) 
    { 
      record <- movie.IMAX[movie.IMAX$movieId == id, ]
      newRow <- data.frame(Title = record$title, MovieId = record$movieId, Score =record$avg_score)
      recommendations <- rbind(recommendations, newRow)
    }
    if (nrow(recommendations) >= k) break
  } 
} else if (user.genre.label == text.Documentary) {
  for (id in movie.Documentary$movieId)
  {
    if (!(id %in% test_user$movie_list)) 
    { 
      record <- movie.Documentary[movie.Documentary$movieId == id, ]
      newRow <- data.frame(Title = record$title, MovieId = record$movieId, Score =record$avg_score)
      recommendations <- rbind(recommendations, newRow)
    }
    if (nrow(recommendations) >= k) break
  } 
} else if (user.genre.label == text.War) {
  for (id in movie.War$movieId)
  {
    if (!(id %in% test_user$movie_list)) 
    { 
      record <- movie.War[movie.War$movieId == id, ]
      newRow <- data.frame(Title = record$title, MovieId = record$movieId, Score =record$avg_score)
      recommendations <- rbind(recommendations, newRow)
    }
    if (nrow(recommendations) >= k) break
  } 
} else if (user.genre.label == text.Musical){
  for (id in movie.Musical$movieId)
  {
    if (!(id %in% test_user$movie_list)) 
    { 
      record <- movie.Musical[movie.Musical$movieId == id, ]
      newRow <- data.frame(Title = record$title, MovieId = record$movieId, Score =record$avg_score)
      recommendations <- rbind(recommendations, newRow)
    }
    if (nrow(recommendations) >= k) break
  } 
} else if (user.genre.label == text.Western){
  for (id in movie.Western$movieId)
  {
    if (!(id %in% test_user$movie_list)) 
    { 
      record <- movie.Western[movie.Western$movieId == id, ]
      newRow <- data.frame(Title = record$title, MovieId = record$movieId, Score =record$avg_score)
      recommendations <- rbind(recommendations, newRow)
    }
    if (nrow(recommendations) >= k) break
  } 
}else if (user.genre.label == text.FilmNoir){
  for (id in movie.FilmNoir$movieId)
  {
    if (!(id %in% test_user$movie_list)) 
    { 
      record <- movie.FilmNoir[movie.FilmNoir$movieId == id, ]
      newRow <- data.frame(Title = record$title, MovieId = record$movieId, Score =record$avg_score)
      recommendations <- rbind(recommendations, newRow)
    }
    if (nrow(recommendations) >= k) break
  }   
}else{
  print("Unknown Genre or no Genre")
}
recommendations
test_user$movie_list
movie.Thriller[movie.Thriller$movieId == 780,]
