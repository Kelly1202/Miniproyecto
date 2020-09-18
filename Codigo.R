#---
  #title: '**Sistemas de recomendación como medida para la retención y fidelización de
 # clientes**'
#author: "Francisco Pautt, Danisse Carrascal, Kelly Carrascal, Natalia Pérez, Gabriela Muñoz"
#date: "18/09/2020"
#output:
  #pdf_document:
 # toc: yes
#word_document:
 # toc: yes
#csl: science.csl
#---
install.packages(recommenderlab)
library(recommenderlab)

movie_url <- "http://files.grouplens.org/datasets/movielens/ml-100k/u.item"
users_url <- "http://files.grouplens.org/datasets/movielens/ml-100k/u.data"
movie_title_df <- read.table(movie_url,header = F,sep="|",quote = "\"")
users_df <- read.table(users_url,header = F,sep="\t",quote = "\"")
names(movie_title_df) <- c("MovieID", "Title","ReleaseDate",
                           "video_release_date",
                           "IMDb_URL", "unknown", "Action", "Adventure",
                           "Animation", "Childrens", "Comedy", "Crime",
                           "Documentary", "Drama", "Fantasy",
                           "FilmNoir", "Horror", "Musical",
                           "Mystery", "Romance", "SciFi",
                           "Thriller", "War", "Western")

movie_title_df<-unique(movie_title_df)
names(users_df)<-c("UserID", "MovieID", "rating", "timestamp")
users_df$timestamp<-NULL
movie_title_df$video_release_date<-NULL
movie_title_df$ReleaseDate<-NULL
movie_title_df$IMDb_URL<-NULL
movie_title_df<-unique(movie_title_df)

clusterMovies <- function(df, kclust = 10){
  df <- df[,c(-1,-2)]
  mclust <- kmeans(df, centers = kclust, nstart = 20)
  return(mclust)
}
getUserInfo <- function(df, id){
  myUser <- subset(df, UserID == id, select = c(MovieID, rating))
  cluster <- 0
  activeUser <- data.frame(myUser[order(myUser$MovieID),], cluster)
  return(activeUser)
}

setUserMovieCluster <- function(m_title_df, mclust, activeUser){
  df_aux <- data.frame(cbind(m_title_df$MovieID,
                             clustNum = mclust$cluster))
  names(df_aux) <- c("MovieID", "Cluster")
  activeUser$cluster <- df_aux[match(activeUser$MovieID, df_aux$MovieID),2]
  return(activeUser)
}

getAverageClusterRating <- function(mclust, activeUser, minRating = 3){
  like <- aggregate(activeUser$rating,
                    by=list(cluster=activeUser$cluster),
                    mean)
  if(max(like$x)<minRating){
    like <- as.vector(0)
  } else {
    like <- as.vector(t(max(subset(like, x>=minRating, select=cluster))))
  }
  return(like)
}


getRecommendedMovies <- function(like, mclust, m_title_df){
  df_aux <- data.frame(cbind(m_title_df$MovieID,
                             clusterNum = mclust$cluster)
  )
  names(df_aux) = c("MovieID", "Cluster")
  if(like==0){
    recommend <- m_title_df[sample.int(n = nrow(m_title_df),
                                       size = 100),1]
  } else {
    recommend <- as.vector(
      t(subset(df_aux,
               Cluster == like,
               select = MovieID
      )
      )
    )
  }
}
getRecommendations <- function(movie_df, user_df, userID){
  mclust <- clusterMovies(movie_df)
  activeUser <- getUserInfo(user_df, userID)
  activeUser <- setUserMovieCluster(movie_df, mclust, activeUser)
  like <- getAverageClusterRating(mclust, activeUser)
  recomendation <- getRecommendedMovies(like, mclust, movie_df)
  recomendation <- recomendation[-activeUser$MovieID]
  movieTitle <- movie_df[match(recomendation, movie_df$MovieID),2]
  recomendation <- data.frame(recomendation, movieTitle)
  return(recomendation)
}

suggestMovies <- function(movie_df, user_df, user_id, num_movies){
  suggestions <- getRecommendations(movie_df, user_df, user_id)
  suggestions <- suggestions[1:num_movies,]
  writeLines("Tal vez te gustaría ver también las siguientes películas:")
  write.table(suggestions[2], row.names = F, col.names = F)
}

suggestMovies(movie_title_df, users_df, 308, 5)
