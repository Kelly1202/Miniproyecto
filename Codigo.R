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
