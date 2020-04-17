# CAPSTONE COURSE - MOVIELENS_10M - GUSTAVO MAYEREGGER

#librarys required
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")

# IMPORTANT!!!
#First run the store_datasets.R to download and save locally the edx and validation datasets

load("./data/edx_&_validation.rda")

#---------------------------------------------------------------------------------------

#Root Square Mean Standard Error function

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2, na.rm=TRUE))
}

#---------------------------------------------------------------------------------------

#Plot to show the relation between the movies and the number of ratings

edx %>% count(movieId) %>% 
  ggplot(aes(n)) + geom_histogram(bins=15, color="darkblue", fill="blue", alpha=0.3) +
                   scale_x_log10() +
                   labs(x="movies IDs", y="number of ratings", 
                        title="Number of ratings vs movies")

#Plot to show the relation between the movies and the average of the ratings

edx %>% group_by(movieId) %>% summarize(avg_rat = mean(rating)) %>%
  ggplot(aes(avg_rat)) + geom_histogram(bins=15, color="darkblue", fill="blue", alpha=0.3) +
  labs(x="average rating per movie", y="number of movies", 
       title="Average ratings per movies") 

#---------------------------------------------------------------------------------------

#Plot to show the relation between the users and the ratings

edx %>% count(userId) %>% 
  ggplot(aes(n)) + geom_histogram(bins=15, color="darkgreen", fill="green", alpha=0.3) +
  scale_x_log10() +
  labs(x="user IDs", y="number of ratings", 
       title="Number of ratings per users")

#Plot to show the relation between the users and the average of the ratings

edx %>% group_by(userId) %>% summarize(avg_rat = mean(rating)) %>%
  ggplot(aes(avg_rat)) + geom_histogram(bins=15, color="darkgreen", fill="green", alpha=0.3) +
  labs(x="average rating per user", y="number of users", 
       title="Average ratings per users") 


#---------------------------------------------------------------------------------------

#Plots showing the relation between the ratings and the genres

#Dataset for individuals genres
indiv_genres <- edx %>% group_by(genres) %>% 
                summarize(n = n(), s = sum(rating)) %>% 
                separate_rows(genres, sep = "\\|") %>% group_by(genres) %>% 
                summarize(count = sum(n), avg_rat = sum(s)/count) %>%
                filter(genres != "(no genres listed)")

#Gender vs Numbeer of ratings
indiv_genres %>% ggplot(aes(x=reorder(genres, count), y=count)) +
  geom_bar(stat= "identity", fill="orange") + coord_flip(y=c(0,3800000)) +
  labs(x="", y="", title="Gender vs number of ratings") +
  geom_text(aes(label= count), hjust=-0.1, size=3) +
  theme(axis.text.x = element_blank()) 

#Gender vs average ratings
indiv_genres %>% ggplot(aes(x=reorder(genres, avg_rat), y=avg_rat)) +
  geom_bar(stat= "identity", fill="darkgreen") + coord_flip(y=c(3.2, 4.1)) +
  labs(x="", y="", title="Gender vs average ratings") +
  geom_text(aes(label= round(avg_rat, digits = 2)), hjust=-0.1, size=3) +
  theme(axis.text.x = element_blank()) 

#Dataset for multiples genres
group_genres <- edx %>% group_by(genres) %>% filter(n() > 50000) %>% 
  summarize(count = n(), avg_rat = mean(rating)) %>% top_n(10, count) %>% 
  arrange(desc(count))

#Multiples genres vs number of ratings
group_genres %>% ggplot(aes(x=reorder(genres, count), y=count)) +
  geom_bar(stat= "identity", fill="orange") + coord_flip(y=c(0,780000)) +
  labs(x="", y="", title="Multiple Genders \nvs number of ratings") +
  geom_text(aes(label= count), hjust=-0.1, size=3) +
  theme(axis.text.y = element_text(hjust = 1, size=8), 
        axis.text.x = element_blank()) 

#Multiple genres vs average ratings
group_genres %>% ggplot(aes(x=reorder(genres, avg_rat), y=avg_rat)) +
  geom_bar(stat= "identity", fill="green") + coord_flip(y=c(3.2, 4.05)) +
  labs(x="", y="", title="Multiple Genders vs average ratings") +
  geom_text(aes(label= round(avg_rat, digits = 2)), hjust=-0.1, size=3) +
  theme(axis.text.y = element_text(hjust = 1, size=8), 
        axis.text.x = element_blank()) 

#--------------------------------------------------------------------------------------

# Plot showing the relation between the ratings and the month of the rate

edx %>% mutate(month = round_date(as_datetime(timestamp), "month")) %>%
          group_by(month) %>% filter(n() >= 5) %>%
          summarize(avg_rat_per_month = mean(rating)) %>% 
          ggplot(aes(month, avg_rat_per_month)) + 
          geom_point(alpha=0.6, color="orange") + geom_smooth() +
          labs(x="date", y="average rating", 
               title="Average rating vs month of the rating") +
          geom_abline(slope=0, intercept = mean(edx$rating), color="darkblue") +
          geom_text(x=as_datetime("2005-01-01"), y = 3.54, 
                        label = "overall avg rating", color="darkblue")

#---------------------------------------------------------------------------------------

# Plot showing the relation between the ratings and the day of the month

edx %>% mutate(day = day(as_datetime(timestamp))) %>%
  group_by(day) %>% summarize(avg_rat_per_day = mean(rating)) %>% 
  ggplot(aes(day, avg_rat_per_day)) + 
  geom_point(alpha=0.6, color="orange") + geom_smooth() +
  labs(x="day", y="average rating", 
       title="Average rating vs day of the month") +
  geom_abline(slope=0, intercept = mean(edx$rating), color="darkblue") +
  geom_text(x=10, y = 3.515, label = "overall avg rating", color="darkblue")

#---------------------------------------------------------------------------------------

# Plot showing the relation between the ratings and the day of the week

edx %>% mutate(day = wday(as_datetime(timestamp))) %>%
  group_by(day) %>% summarize(avg_rat_per_day = mean(rating)) %>% 
  ggplot(aes(day, avg_rat_per_day)) + geom_smooth() +
  geom_point(alpha=0.6, color="orange") + 
  labs(x="day", y="average rating", 
       title="Average rating vs day of the week") +
  geom_abline(slope=0, intercept = mean(edx$rating), color="darkblue") +
  geom_text(x=4.5, y = 3.515, label = "overall avg rating", color="darkblue") +
  scale_x_continuous(breaks = 1:7, labels=c("mon", "tue", "wed", "thu", "fri", "sat", "sun"))

#---------------------------------------------------------------------------------------

# Plot showing the relation between the ratings and the hour of the day

edx %>% mutate(hour = hour(as_datetime(timestamp))) %>%
  group_by(hour) %>% summarize(avg_rat_per_hour = mean(rating)) %>% 
  ggplot(aes(hour, avg_rat_per_hour)) + 
  geom_point(alpha=0.6, color="orange") + geom_smooth() +
  labs(x="hour", y="average rating", 
       title="Average rating vs hour of the day") +
  geom_abline(slope=0, intercept = mean(edx$rating), color="darkblue") +
  geom_text(x=9, y = 3.515, label = "overall avg rating", color="darkblue")

#---------------------------------------------------------------------------------------

# Plot showing the dependence of the ratings against the release year of the movie

edx %>% mutate(y_rel = str_extract(title, pattern="\\([1-2]\\d{3}\\)"),
                  release = as.numeric(str_extract(y_rel, pattern="\\d{4}"))) %>%
          group_by(release) %>% filter(n() >= 5) %>%
          summarize(avg_rat_rea = mean(rating)) %>%
          ggplot(aes(release, avg_rat_rea)) + 
          geom_point(alpha=0.6, color="orange") + geom_smooth() +
          labs(x="date", y="average rating", 
               title="Average rating vs year of movie release") +
          geom_abline(slope=0, intercept = mean(edx$rating), color="darkblue") +
          geom_text(x=1950, y = 3.54, 
                    label = "overall avg rating", color="darkblue")
          
#---------------------------------------------------------------------------------------

# Just the average model

mu <- mean(edx$rating)

rmse_simple <- RMSE(mu, validation$rating)

result_table <- tibble(MODEL="Just the average", RMSE=rmse_simple)

result_table %>% kable()


#---------------------------------------------------------------------------------------

# The movie and user effect model

# The overall rating average
mu <- mean(edx$rating)

b_i <- edx %>% group_by(movieId) %>%
  summarize(b_i = mean(rating - mu)) 

b_u <- edx %>% left_join(b_i, by = "movieId") %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - b_i - mu))  

edx %>% count(movieId) %>% 
  ggplot(aes(n)) + geom_histogram(bins=15, color="darkblue", fill="blue", alpha=0.3) +
  scale_x_log10() +
  labs(x="movies IDs", y="number of ratings", 
       title="Number of ratings vs movies")


# Plot the b_i 
b_i %>% ggplot(aes(b_i)) +
  geom_histogram(bins=10, color="darkblue", fill="blue", alpha=0.5) +
  labs(x="movie bias (b_i)", y="number of movies", 
     title="Movies biases distribution")

#Plot the b_u
b_u %>% ggplot(aes(b_u)) +
  geom_histogram(bins=10, color="darkgreen", fill="green", alpha=0.5) +
  labs(x="user bias (b_u)", y="number of users", 
       title="Users biases distribution")

prediction <- validation %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  mutate(r_iu = mu + b_i + b_u) %>% select(title, rating, r_iu)

rmse1 <- RMSE(prediction$r_iu, prediction$rating)

result_table <-  bind_rows(result_table, tibble(MODEL="Movie & User Effect", RMSE=rmse1))

result_table %>% kable()

# Average predicted error vs number of ratings per movies

prediction %>% group_by(title) %>% summarize(error = abs(mean(rating-r_iu)), count=n()) %>%
  arrange(desc(error)) %>%
  ggplot(aes(count, error)) + geom_point(color="purple", alpha=0.3) + scale_x_log10() +
  labs(x="number of movies", y="average error per movie", 
       title="Average error vs number of ratings per movie") 


#--------------------------------------------------------------------------------------

# The movie and user effect model with regularization

rmse_iu_reg <- function(train_set, test_set, l=5) {
  rmses <- sapply(l, function(l){

    # The overall rating average
    mu <- mean(train_set$rating)

    b_i <- train_set %>% group_by(movieId) %>%
      summarize(b_i = sum(rating - mu)/(n()+l)) 

    b_u <- train_set %>% left_join(b_i, by = "movieId") %>%
      group_by(userId) %>%
      summarize(b_u = sum(rating - b_i - mu)/(n()+l))  

    r_iu <- test_set %>% 
      left_join(b_i, by = "movieId") %>%
      left_join(b_u, by = "userId") %>%
      mutate(r_iu = mu + b_i + b_u) %>% .$r_iu
    return(RMSE(r_iu, test_set$rating))
  })
  results <- data.frame(lambda=l, rmse = rmses)
  ind <- which.min(results$rmse)
  best_tune <- data.frame(lambda = results$lambda[ind],
                          rmse = results$rmse[ind])
  list(results = results, best_tune = best_tune)
}

lambdas <- seq(0, 10, 0.5)
result_reg <- rmse_iu_reg(edx, validation, lambdas)

result_reg$results %>% ggplot(aes(lambda, rmse)) + geom_point(color="darkblue") +
  labs(x="lambda", y="RMSE", 
       title="Lambda vs RMSE") 

rmse2 <- result_reg$best_tune$rmse

result_table <-  bind_rows(result_table, tibble(MODEL="Regularized Movie & User", 
                                                RMSE=rmse2))
result_table %>% kable()

#--------------------------------------------------------------------------------------

# The complete variables effect model
# User, movie, genre, date and release model with regularization

rmse_iugdr_reg <- function(train_set, test_set, l=5) {
  rmses <- sapply(l, function(l){

    # Creating in the train and test sets the month column that represent
    # the month that the rating was made and the release column
    # that represent the year that the movie had been released
    
    train_set <- train_set %>% 
      mutate(month = round_date(as_datetime(timestamp), "month"),
             y_rel = str_extract(title, pattern="\\([1-2]\\d{3}\\)"),
             release = as.numeric(str_extract(y_rel, pattern="\\d{4}"))) %>%
      select(-y_rel)
    
    test_set <- test_set %>% 
      mutate(month = round_date(as_datetime(timestamp), "month"),
             y_rel = str_extract(title, pattern="\\([1-2]\\d{3}\\)"),
             release = as.numeric(str_extract(y_rel, pattern="\\d{4}"))) %>%
      select(-y_rel)  
    
    # The overall rating average
    mu <- mean(train_set$rating)
    
    b_i <- train_set %>% group_by(movieId) %>%
      summarize(b_i = sum(rating - mu)/(n()+l)) 
   
    b_u <- train_set %>% left_join(b_i, by = "movieId") %>%
      group_by(userId) %>%
      summarize(b_u = sum(rating - b_i - mu)/(n()+l))  
    
    b_g <- train_set %>%  left_join(b_i, by = "movieId") %>%
      left_join(b_u, by = "userId") %>% 
      group_by(genres) %>%
      summarize(b_g = sum(rating - b_i - b_u - mu)/(n()+l))
    
    b_d <- train_set %>% left_join(b_i, by = "movieId") %>%
      left_join(b_u, by = "userId") %>% left_join(b_g, by = "genres") %>%
      group_by(month) %>%
      summarize(b_d = sum(rating - b_i - b_u - b_g - mu)/(n()+l))
    
    b_r <- train_set %>% left_join(b_i, by = "movieId") %>%
      left_join(b_u, by = "userId") %>% left_join(b_g, by = "genres") %>%
      left_join(b_d, by = "month") %>%
      group_by(release) %>%
      summarize(b_r = sum(rating - b_i - b_u - b_g - b_d - mu)/(n()+l))
    
    predicted_ratings <- test_set %>% 
      left_join(b_i, by = "movieId") %>%
      left_join(b_u, by = "userId") %>%
      left_join(b_g, by = "genres") %>%
      left_join(b_d, by = "month") %>%
      left_join(b_r, by = "release") %>%
      mutate(pred = mu + b_i + b_u + b_g + b_d + b_r) %>%
      .$pred
    return(RMSE(predicted_ratings, test_set$rating))
  })
  results <- data.frame(lambda=l, rmse = rmses)
  ind <- which.min(results$rmse)
  best_tune <- data.frame(lambda = results$lambda[ind],
                          rmse = results$rmse[ind])
  list(results = results, best_tune = best_tune)
}

lambdas <- seq(4, 6, 0.5)
result_complete <- rmse_iugdr_reg(edx, validation, lambdas)

result_complete$results %>% ggplot(aes(lambda, rmse)) + geom_point(color="darkblue") +
  labs(x="lambda", y="RMSE", 
       title="Lambda vs RMSE for complete model") 

rmse3 <- result_complete$best_tune$rmse

result_table <-  bind_rows(result_table, tibble(MODEL="Regularized Movie, User, Gender, Date & Release", 
                                                RMSE=rmse3))
result_table %>% kable()

#----------------------------------------------------------------------------------------

# Models comparision

result_table %>% mutate(IMPROVE = 1-RMSE/rmse_simple) %>% kable()

# Best and worst movies with and without regulrization

best_or_worst <- function(data, best, reg){
    mu <- mean(edx$rating)
  
    data %>% group_by(movieId) %>%
    mutate(b_i = sum(rating - mu)/(n()+5*reg)) %>% ungroup() %>%
    group_by(userId) %>%
    mutate(b_u = sum(rating - b_i - mu)/(n()+5*reg)) %>% ungroup() %>%
    group_by(title) %>% summarize(count = n(), pred = mean(mu + b_i + b_u)) %>%
    top_n(ifelse(best, 10, -10), pred) %>% select(-pred)
}

best <- FALSE
reg <- TRUE
best_or_worst(edx, best, reg)

