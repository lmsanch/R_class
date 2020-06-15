## ----knitr_setup, include=FALSE------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.pos = 'h',fig.align = 'center', fig.width = 5,fig.height = 2)


## ----RMSE_function1, echo = FALSE----------------------------------------
RMSE <- function(predicted_ratings, true_ratings){
  sqrt(mean((predicted_ratings - true_ratings)^2))
}


## ----load_libs, echo = FALSE, message=FALSE------------------------------
library(tidyverse)
library(caret)
library(lubridate)
library(gridExtra)
library(knitr)
library(kableExtra)


## ----setup2, include=FALSE-----------------------------------------------
theme_update(# axis labels
             axis.title = element_text(size = 8),
             # tick labels
             axis.text = element_text(size = 6))


## ----sets_and_subsmissions, echo = TRUE, message = FALSE, warning = FALSE, eval = TRUE----
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


## ----summary_edx, echo = FALSE-------------------------------------------
summary(edx)


## ----summary_users_movies, echo = FALSE----------------------------------
edx %>%
summarize(n_users = n_distinct(userId), 
          n_movies = n_distinct(movieId))


## ----edx_summary, echo=FALSE, results='asis'-----------------------------
as_tibble(edx)%>%head()%>%
  kable()%>%kable_styling(position = 'center')%>%
  column_spec(1,border_left = T)%>%column_spec(6,border_right = T)%>%
  row_spec(0,bold=T)


## ----rating_plot, fig.width = 5,fig.height = 4, echo=FALSE---------------
mu <- mean(edx$rating)
edx%>%ggplot(aes(rating)) +
  geom_histogram(binwidth=0.25, col='darkblue', bins=10, fill='blue') +
  scale_x_continuous(breaks = seq(0.5,5,0.5))+
  geom_vline(xintercept =mu,col='red',linetype='dashed')


## ----number_of_ratings_per_movie, fig.width = 5,fig.height = 4, echo=FALSE----
edx %>%
count(movieId) %>%
ggplot(aes(n)) +
geom_histogram(bins = 30, binwidth=0.25, color ='darkblue', fill='blue') +
scale_x_log10() +
xlab("Log number of ratings") +
  ylab("Number of movies") +
ggtitle("Log number of ratings per movie")


## ----number_ratings_given_by_users, fig.width = 5,fig.height = 4, echo=FALSE----
edx %>%
count(userId) %>%
ggplot(aes(n)) +
geom_histogram(bins = 30, binwidth=0.25, color = 'darkblue', fill='blue') +
scale_x_log10() +
xlab("Log number of ratings") + 
ylab("Number of users") +
ggtitle("Number of ratings given by users")


## ----Mean_movie_ratings_given_by_users, fig.width = 5,fig.height = 4, echo=FALSE----
edx %>%
  group_by(userId) %>%
  filter(n() >= 100) %>%
  summarize(b_u = mean(rating)) %>%
  ggplot(aes(b_u)) +
  geom_histogram(bins = 30, binwidth=0.25, color = 'darkblue', fill='blue') +
  xlab("Average rating") +
  ylab("Number of users") +
  ggtitle("Mean movie ratings given by users") +
  scale_x_discrete(limits = c(seq(0.5,5,0.5)))


## ----RMSE_function2, echo = TRUE-----------------------------------------
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}


## ----average_rating, echo = TRUE-----------------------------------------
mu <- mean(edx$rating)
mu


## ----naive_rmse, echo = TRUE---------------------------------------------
naive_rmse <- RMSE(validation$rating, mu)
naive_rmse


## ----rmse_results1, echo = TRUE------------------------------------------
rmse_results <- data_frame(method = "Average movie rating model", RMSE = naive_rmse)
rmse_results %>% knitr::kable()


## ----Number_of_movies_with_the computed_b_i, fig.width = 5,fig.height = 4, echo=FALSE----
movie_avgs <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu))
movie_avgs %>% qplot(b_i, geom ="histogram", bins = 10, data = ., color = I('darkblue'), fill = I('blue'),
ylab = "Number of movies", main = "Number of movies with the computed b_i")


## ----predicted_ratings, echo = TRUE--------------------------------------
predicted_ratings <- mu +  validation %>%
  left_join(movie_avgs, by='movieId') %>%
  pull(b_i)
model_1_rmse <- RMSE(predicted_ratings, validation$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie effect model",  
                                     RMSE = model_1_rmse ))
rmse_results %>% knitr::kable()


## ----average_user_rating, fig.width = 5,fig.height = 4, echo=TRUE--------
user_avgs<- edx %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  filter(n() >= 100) %>%
  summarize(b_u = mean(rating - mu - b_i))
user_avgs%>% qplot(b_u, geom ="histogram", bins = 30, data = ., color = I('darkblue'), fill = I('blue'))


## ----user_avgs, echo = TRUE----------------------------------------------
user_avgs <- edx %>%
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))
  


## ----model_2_rmse, echo = TRUE-------------------------------------------
predicted_ratings <- validation%>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)
model_2_rmse <- RMSE(predicted_ratings, validation$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie and user effect model",  
                                     RMSE = model_2_rmse))
rmse_results %>% knitr::kable()


## ----t_rating,echo=FALSE, fig.width = 5,fig.height = 4,message=FALSE-----
edx%>%mutate(date=as_datetime(timestamp))%>%
  mutate(date=round_date(date,unit = 'week'))%>%
  group_by(date)%>%summarize(rating=mean(rating))%>%
  ggplot(aes(date,rating))+geom_point()+geom_smooth()+
  xlab('date(weekly)')+
  geom_hline(yintercept = mu,col='red',linetype='dashed')


## ----lambdas, echo = TRUE------------------------------------------------
lambdas <- seq(0, 10, 0.25)
rmses <- sapply(lambdas, function(l){
  
  mu <- mean(edx$rating)
  
  b_i <- edx %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- edx %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  predicted_ratings <- 
    validation %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, validation$rating))
})


## ----plot_lambdas, fig.width = 5,fig.height = 4, echo=FALSE--------------
qplot(lambdas, rmses) + theme_light()


## ----min_lambda, echo = TRUE---------------------------------------------
  lambda <- lambdas[which.min(rmses)]
lambda


## ----rmse_results2, echo = TRUE------------------------------------------
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized movie and user effect model",  
                                     RMSE = min(rmses)))
rmse_results %>% knitr::kable()


## ----Enviroment----------------------------------------------------------
print("Operating System:")
version

