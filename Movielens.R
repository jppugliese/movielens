################################
# Create edx set, validation set
################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data

set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set

validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set

removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

############################################################################################################################################
# Exploring datasets
######## Exercises #########################################################################################################################
#Number of rating having 0 and 3 respectively
str(edx)
sum(edx$rating=="0")
sum(edx$rating=="3")

#Unique number of Movies and users respectively
unique(edx$movieId)
n_distinct(edx$movieId)
n_distinct(edx$userId)

#Counting the numbers of most rated movie's genres
edx %>% separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>%
  summarize(avrg = mean(rating),count = n()) %>%
  arrange(desc(count))
#We can see from there that movie's rating and their counts are different depending of the genre

#Most rated movies
edx %>% group_by(movieId, title) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

#Ranked Rating provided
edx %>% group_by(rating) %>% summarize(count = n()) %>% arrange(desc(count))  %>% plot(count)
#We can see from here that the most common ratings are 4 and 3, with 4 being the highest and then 3.



#Looking at how user rate movies. This shows that not every users are rating as often as others:
edx %>% count(userId) %>% ggplot(aes(n)) + geom_histogram() + scale_x_log10()

#A plot will show the skewing of user's feedback in the ratings.
edx %>% group_by(userId) %>% 
  summarize(b1 = mean(rating - mu)) %>% ggplot(aes(b1)) + geom_histogram()
#It can be seen that users are affecting the rating, we could try to add this into our model.


#Looking at the rating per movie, showing that movies are not rated in the same way. Some are rated more times than others:
edx %>% count(movieId) %>% ggplot(aes(n)) + geom_histogram() + scale_x_log10()

#Creating a year's colum, extracted from title's column
edx_new <- edx %>% separate_rows(title, sep = "\\|") %>% mutate(year = as.numeric(str_sub(title,-5,-2)))

#Looking at the average number of ratings in fonction of the year
edx_new %>% group_by(year) %>% summarize(avrg = mean(rating)) %>% plot(aes(year,avrg))

edx_new %>% group_by(year) %>% summarize(count = n()) %>% plot(aes(year,count))



edx %>% group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu)) %>% ggplot(aes(b_i)) + geom_histogram() 



###########################################################################################################################
# Creating models to calculate the Root Mean Square Error

#Creating a subset to traina nd test the model.  A value of 20% has been chosen
library(caret)
set.seed(7)
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.2, list = FALSE)
train_set <- edx[-test_index,]
test_set <- edx[test_index,]

#Making sure that we have the same movieID and userID in the train_set as in the test_set
test_set <- test_set %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

#Creating the RMSE function
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
  }

#Simplest model using just the average
mu_hat <- mean(train_set$rating)
mu_hat

#Implementing the simplest average to calculate the RMSE:
naive_rmse <- RMSE(test_set$rating, mu_hat)
naive_rmse

rmse_results <- data_frame(method = "Just the average", RMSE = naive_rmse)

# Considering the movie effect
# lm() fucntion will take way too long and may crash RStudio
mu <- mean(train_set$rating) 
movie_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

#Implementing the movie effect to calaculate the RMSE
predicted_ratings <- mu + test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  pull(b_i)

model_1_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie Effect Model",  
                                     RMSE = model_1_rmse))
rmse_results 


#Considering the user effect and adding it to the model 
user_avgs <- train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

#Implementing the user effect to calculate the RMSE:
predicted_ratings <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)


model_2_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User Effects Model",  
                                     RMSE = model_2_rmse))
rmse_results

# Although we have considered 2 factors, the improvement brought is still minimal and we need to look at outliers/information that are skewing our model.
# By applying regularization we can penalize movies that have been rated highly or poorly only by a few people and that are affecting our model.
# The model considers some movies that have been rated either very positevely or negatively by very few users but that are skewing the ratings as seen from the code below:
movie_titles <- edx %>% 
  select(movieId, title) %>%
  distinct()

movie_avgs %>% left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>% 
  slice(1:10)  %>% 
  pull(title)

train_set %>% count(movieId) %>% 
  left_join(movie_avgs, by="movieId") %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>% 
  slice(1:10) %>% 
  pull(n)

# Looking at the best value to set the penality so that the RMSE value is minimised
lambdas <- seq(0, 10, 0.25)

rmses <- sapply(lambdas, function(l){
  
  mu <- mean(train_set$rating)
  
  b_i <- train_set %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  predicted_ratings <- 
    test_set %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, test_set$rating))
})

#Visualizing the rmse plot in function of lambda
qplot(lambdas, rmses)  

#Finding the values of lmabda that mimizes the rmses the most
lambda <- lambdas[which.min(rmses)]
lambda
min(rmses)



rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Movie + User Effect Model",  
                                     RMSE = min(rmses)))
rmse_results %>% knitr::kable()

#Bringing everything together
# Now let's use the Validation dataset to check the overall accuracy of the model
#Compute regularized estimates of b_i using the calculated best lambda
movie_avgs_reg <- edx %>% 
  group_by(movieId) %>% 
  summarize(b_i = sum(rating - mu)/(n()+lambda))
# Compute regularized estimates of b_u using lambda
user_avgs_reg <- edx %>% 
  left_join(movie_avgs_reg, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - mu - b_i)/(n()+lambda))
# Predict ratings
predicted_ratings_reg <- validation %>% 
  left_join(movie_avgs_reg, by='movieId') %>%
  left_join(user_avgs_reg, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>% 
  pull(pred)
# Test and save results
RMSE(validation$rating,predicted_ratings_reg)
# Obtained an overall accuracy of 0.8648177











