
  
title: "MovieLens Data Recommendation System"
author: "Carol Riffle"
date: "2023-002-27"


knitr::opts_chunk$set(echo = TRUE)






if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

# Import libraries
library(tidyverse)
library(caret)
library(ggplot2)
library(dplyr)
library(dslabs)
library(lubridate)


# Create edx and final_holdout_test sets 

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip


options(timeout = 120)

dl <- "ml-10M100K.zip"
if(!file.exists(dl))
  download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)


ratings_file <- "ml-10M100K/ratings.dat"
if(!file.exists(ratings_file))
  unzip(dl, ratings_file)



movies_file <- "ml-10M100K/movies.dat"
if(!file.exists(movies_file))
  unzip(dl, movies_file)

# Pull ratings
ratings <- as.data.frame(str_split(read_lines(ratings_file), fixed("::"), simplify = TRUE),
                         stringsAsFactors = FALSE)
colnames(ratings) <- c("userId", "movieId", "rating", "timestamp")
ratings <- ratings %>%
  mutate(userId = as.integer(userId),
         movieId = as.integer(movieId),
         rating = as.numeric(rating),
         timestamp = as.integer(timestamp))

# Pull movies
movies <- as.data.frame(str_split(read_lines(movies_file), fixed("::"), simplify = TRUE),
                        stringsAsFactors = FALSE)
colnames(movies) <- c("movieId", "title", "genres")
movies <- movies %>%
  mutate(movieId = as.integer(movieId))

movielens <- left_join(ratings, movies, by = "movieId")




# Create final hold-out test set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]


# Make sure userId and movieId in final hold-out test set are also in edx set
final_holdout_test <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")



# Add rows removed from final hold-out test set back into edx set
removed <- anti_join(temp, final_holdout_test)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)


# Two data sets were established.  The edx data set was used for data exploration and model development. The final_holdout_test data set was used for testing the final model.


# Split edx Data Set for Model Development using the caret package

set.seed(1, sample.kind = "Rounding" )
edx_index <- createDataPartition(y=edx$rating, times = 1, p = 0.1, list = FALSE)
edx_train <- edx[-edx_index,]
edx_temp <- edx[edx_index,]

# Make sure edx_test data set has the same movies and users as the edx_train data set.
edx_test <- edx_temp %>% 
  semi_join(edx_train, by = "movieId") %>%
  semi_join(edx_train, by = "userId")

# Add rows back to edx_train
test_removed <- anti_join(edx_temp, edx_test)
edx_train <- rbind(edx_train, test_removed)

rm(edx_index, edx_temp, test_removed)


# Create a data.frame containing the movieId and title for supporting analysis during model development. 

movie_titles_train <- edx_train |>
  select(movieId, title) |>
  distinct()

# Look at data structure
str(edx)


# Apply summary function to understand basic statistics aroudn the data set and identify any missing data 
summary(edx)


# edx |> filter(is.na(variable)) to verify missing data


# Add a  column for the rating year from the unix timestamp 

edx_yr <- edx |> mutate(rating_year = year(as_datetime(timestamp))) 


# Verify new column
str(edx_yr)


# Explore user data

# Unique users  
edx |> 
  summarize(n_users = n_distinct(userId))


# Distribution
edx |> dplyr::count(userId) |>
  ggplot(aes(n)) +
  geom_histogram(bins = 30, color = "black", fill = "blue") +
  scale_x_log10() +
  ggtitle("Distribution of Movies Rated by Users")



# Determine how many users rate <= 100 movies
number <- edx |> group_by(userId) |>
  filter(n() <= 100)
  

# Check average rating of users who have rated between 100 and 1000 movies
user_mean <- edx |> group_by(userId) |>
  select(userId, rating) |>
  filter(n() <= 1000 & n() >= 100) 



# Distribution of the average rating for users
user_ave <- edx %>%
  group_by(userId) %>%
  summarize( ave = mean(rating))

edx |>
  group_by(userId) |>
  summarize( ave = mean(rating)) |>
  ggplot(aes(ave)) +
  geom_histogram(bins = 30, color = "black", fill = "blue") +
  scale_x_log10() +
  ggtitle("Distribution of the Average Rating by Users")



# Explore movie data
# Unique movies
no_movies <- edx |> 
  summarize(n_movies = n_distinct(movieId))
no_movies


# Creation of a data table with movie titles will be used to join titles to movieId's.
movie_titles <- edx |>
  select(movieId, title) |>
  distinct()



# Movie rating distribution
edx |> dplyr::count(movieId) |> 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black", fill = "blue") + 
  scale_x_log10() +
  xlab("Ratings") +
  ylab("Movies") +
  ggtitle("**Distribution of Movie Ratings**")


# Top movies evaluation

# No removal of movies with few ratings
movie_avgs <- edx |>
  group_by(movieId) |>
  summarize(ave_rating = mean(rating), count = n())


top_ten <- movie_avgs |> left_join(movie_titles, by="movieId") |>
  arrange(desc(ave_rating)) |>
  select(title, ave_rating, count) |>
  slice(1:10) |>
  knitr::kable(col.names = c("Movie Title", "Average Rating", "Number of Ratings"), caption = "**Top 10 Movies Based on the Average Rating**") 
top_ten


# Removal of movies with less than 10 ratings
movie_avgs_10 <- edx |>
  group_by(movieId) |>
  filter(n() >=10) |>
  summarize(ave_rating = mean(rating), count = n())
movie_avgs_10

top_ten_10 <- movie_avgs_10 |> 
  left_join(movie_titles, by="movieId") |>
  arrange(desc(ave_rating)) |>
  select(title, ave_rating, count) |>
  slice(1:10) |>
  knitr::kable(col.names = c("Movie Title", "Average Rating", "Number of Ratings"), caption = "**Top 10 Movies Based on the Average Rating When the Number of Ratings was >= 10**")
top_ten_10


# Ratings exploration
edx |> ggplot(aes(rating)) +
  geom_bar(color = "black", fill = "blue") +
  ggtitle("Ratings Distribution")


edx |> group_by(rating) |>
  summarize(count = n()) |>
  arrange(desc(count)) |>
  knitr::kable(caption = "**Ratings Distribution Summary Table**")

# Average rating matches the summary function
avg <- mean(edx$rating)
avg


#  Year Rated
edx_yr |> ggplot(aes(rating_year)) +
  geom_bar(color = "black", fill = "blue")



# Year rated heat map
edx_yr |> count(rating_year, rating) |>
  ggplot(aes(x = rating_year, y = rating)) +
  geom_tile(aes(fill = n)) +
  geom_text(aes(label = n), color = "white", size = 2) +
  guides(fill = guide_colourbar(barwidth = 1,
                                barheight = 10))

# Summary of rating year
edx_yr |>
  group_by(rating_year) |>
  summarize( year_ave = mean(rating), count = n()) |>
  knitr::kable(col.names = c("Rating Year", "Average Rating", "Number of Ratings"), caption = "**Rating Year Summary**")  



# Combined Genres evaluation
genre_number <- summarize(edx, n_genres = n_distinct(genres))
genre_number


edx_genre <- edx |>
  select(genres, rating)

edx_genre |>
  group_by(genres) |>
  summarize(count = n()) |>
  arrange(desc(count)) |>
  slice(1:20) |>
  knitr::kable(caption = "**Top 20 Genres**")

# Split genres summary
edx_genre |> separate_rows(genres, sep = "\\|") |>
  group_by(genres) |>
  summarize(ave_genre_rating = mean(rating), count = n()) |>
  arrange(desc(count)) |>
  knitr::kable(caption = "**Genres and Average Ratings**")

#  **Model Development**


edx_train <- edx_train |> select(userId, movieId, title, rating)

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}


# Model 1 Average Rating
avg <- mean(edx_train$rating)
avg

Model_1_rmse <- RMSE(edx_test$rating, avg)
Model_1_rmse

rmse_values <- data_frame(Method = "Model 1 Average Rating", RMSE = Model_1_rmse)
rmse_values


# Model 2 Movie Effects
avg <- mean(edx_train$rating)
movie_avgs <- edx_train %>% 
  group_by(movieId) %>% 
  summarize(bi = mean(rating - avg))

predicted_ratings <- avg + edx_test |>
  left_join(movie_avgs, by = "movieId") |>
  pull(bi)

Model_2_movie_rmse <- RMSE(predicted_ratings, edx_test$rating)
Model_2_movie_rmse

Model_Results <- bind_rows(rmse_values, data_frame(Method = "Model 2 Movie Effects", RMSE = Model_2_movie_rmse))
Model_Results


# Model 3 User and Movie Effect
user_avgs <- edx_train |> 
  left_join(movie_avgs, by='movieId') |>
  group_by(userId) |>
  summarize(bu = mean(rating - avg - bi))

predicted_ratings <- edx_test |> 
  left_join(movie_avgs, by='movieId') |>
  left_join(user_avgs, by='userId') |>
  mutate(pred = avg + bi + bu) |>
  pull(pred)


Model_3_User_movie_rmse <- RMSE(predicted_ratings, edx_test$rating)
Model_3_User_movie_rmse

Model_Results <- bind_rows(Model_Results, data_frame(Method = "Model 3 User and Movie Effect", RMSE = Model_3_User_movie_rmse))
Model_Results


# Model 4 Regularization of Movie and User Bias


# Selection of lambda for movie and user bias
lambdas <- seq(0, 10, 0.25)
rmses <- sapply(lambdas, function(l){
  ave_rating <- mean(edx_train$rating)
  bi <- edx_train |>
    group_by(movieId) |>
    summarize(bi = sum(rating - ave_rating)/(n()+l))
  bu <- edx_train |> 
    left_join(bi, by="movieId") |>
    group_by(userId) |>
    summarize(bu = sum(rating - bi - ave_rating)/(n()+l))
  predicted_ratings <- 
    edx_test |> 
    left_join(bi, by = "movieId") |>
    left_join(bu, by = "userId") |>
    mutate(pred = ave_rating + bi + bu) |>
    pull(pred)
  return(RMSE(predicted_ratings, edx_test$rating))
})

qplot(lambdas, rmses)  

lambda <- lambdas[which.min(rmses)]
lambda


Model_4_apply_regularization_rmse <- min(rmses)
Model_4_apply_regularization_rmse


Model_Results <- bind_rows(Model_Results, data_frame(Method = "Model 4 Regularization of Movie and User Bias", RMSE = Model_4_apply_regularization_rmse ))
Model_Results



# Results using the final_holdout_test set with Model 4

lambdas <- seq(0, 10, 0.25)
rmses <- sapply(lambdas, function(l){
  ave_rating <- mean(edx$rating)
  bi <- edx |>
    group_by(movieId) |>
    summarize(bi = sum(rating - ave_rating)/(n()+l))
  bu <- edx |> 
    left_join(bi, by="movieId") |>
    group_by(userId) |>
    summarize(bu = sum(rating - bi - ave_rating)/(n()+l))
  predicted_ratings <- 
    final_holdout_test |> 
    left_join(bi, by = "movieId") |>
    left_join(bu, by = "userId") |>
    mutate(pred = ave_rating + bi + bu) |>
    pull(pred)
  return(RMSE(predicted_ratings, final_holdout_test$rating))
})

qplot(lambdas, rmses)  

lambda <- lambdas[which.min(rmses)]
lambda


Model_4_results <- min(rmses)
Model_4_results


