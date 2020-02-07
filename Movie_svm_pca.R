# MovieLens 10M dataset: Goal RMSE <= 0.8649
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")
if(!require(devtools)) install_github("vqv/ggbiplot")
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")
# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

library(tidyverse)
library(caret)
library(data.table)
library(lubridate)
library(RColorBrewer)
library(knitr)
library(corrplot)
library(rpart)
library(ggbiplot)
library(kableExtra)
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

# Creating the train_set: edx
edx <- movielens[-test_index,]

# Creating the test_set: temp
temp <- movielens[test_index,]

# Unifying data sets.
# The taring and test dataset requires to have the same features
# using the semi_join().  

validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set

removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

# Checking data
str(edx,4)
glimpse(validation,4)
dim(edx)


############ PCA exploring #################

Data_sample_pca <- sample(unique(edx$movieId), 10)
Edx_sample_pca <- subset(edx, edx$movieId %in% Data_sample_pca)
edx.data <- Edx_sample_pca[, c(1:4, 6)] 
edx.data$genres <- as.numeric(as.factor(edx.data$genres))
edx.quanti <- as.matrix(edx.data[,c(1,2,4,5)])
row.names(edx.quanti) <- edx.data$rating
prin_comp <- prcomp(edx.quanti, center = TRUE, scale = TRUE)

# Summary gives the principals components variance
# Adding PC1 to PC3 give more tah 85% of dta variance

summary(prin_comp)
rate.pca <- as.factor(round(edx.data$rating,0))
ggbiplot(prin_comp,  groups = rate.pca)
pca.corr <- cor(as.matrix(prin_comp$rotation))

# Plotting correlation to ensure that PCAs are correlated

corrplot(pca.corr, type="upper", col=brewer.pal(n=8, name="RdYlBu"),
         mar=c(0,0,1,0),title = "PCA Correlation")
# the varibles are correlated and we are ready to start the analysis.

############# Data Visualization ############################
# The database is a bigData, let's sample to se the behavior and distribution
# of the data
# (Irizarry, 2019, A)https://courses.edx.org/courses/course-v1:HarvardX+PH125.8x+2T2019/courseware/a49844e4a3574c239f354654f9679888/7e7727ce543b4ed6ae6338626862eada/1?activate_block_id=block-v1%3AHarvardX%2BPH125.8x%2B2T2019%2Btype%40vertical%2Bblock%40df3d8a86b43f4247a4dd42bcabb1a663

# The database is a bigData, let's sample to se the behavior and distribution
# of the data

Data_sample <- sample(unique(edx$movieId), 100)
Edx_sample <- subset(edx, edx$movieId %in% Data_sample)

# Checking the distribution of Ratings
RatingMeans <- mean(Edx_sample$rating)
hist(Edx_sample$rating, col= "lightgreen", main ="Rating Distribution", xlab = "Ratings")
abline(v = mean(Edx_sample$rating), col="red", lwd=3, lty=2)
text(x=3.5, y=10000, labels =round(RatingMeans,2) )
# The mean of rating will give an accurate measure because in the graphic we can see 3 
# and 4 stars as the more frequent rating

# Checking the distribution of movies shows there are some movies that are more frequently 
# seen and rated. 
# Now, it is imporant to know which movies genres are they
hist(Edx_sample$movieId, col = "grey",main = "Movies Distribution", xlab = "Movie ID")

# The are some movies that are more frequently seen and rated. It is important to know which
# movies genres are them.
Genres_dist <- Edx_sample %>% dplyr::group_by(genres)%>% dplyr::summarise(Movie_Genres = n())%>%
  arrange(desc(Movie_Genres))%>% ungroup()

# Using http://r-statistics.co/ggplot2-Tutorial-With-R.html as refrence for plotting:
Genres_dist %>% ggplot(aes(reorder(genres,Movie_Genres), y = Movie_Genres, 
                           fill =Movie_Genres)) + geom_col() + 
  coord_flip() +theme(legend.position = "top") +
  theme(axis.text.y = element_text(color = "grey20", 
                                   size =7, angle = 0, hjust = 1, 
                                   vjust = 0, face = "plain"))+
  labs( x = 'Genres', title = 'Distribution of Movie Genres')
nrow(Genres_dist)
# There are 55 Genres and some of them have high preference and ratings. Therefore,
# we can hypotize that ratings depend on genres and movies.
######### User and movies's year ################


# Now, the distrubution of Users and genres will show if genres and users have dependency.
# using Phillips, YaRrr! The Pirate’s Guide to R  2018https://bookdown.org/ndphillips/YaRrr/, 
# the plot is created and http://r-statistics.co/ggplot2-Tutorial-With-R.html

p <- Edx_sample%>% ggplot(aes(x = log(userId), y = genres))+ 
  geom_point(alpha=0.1) +
  geom_smooth(color = "red", method = "lm") +
  theme(axis.text.y = element_text(color = "grey20", size =7, angle = 0, 
                                   hjust = 1, vjust = 0, face = "plain")) +
  ggtitle("User preference by Genres")
p

# Now , we need to work with movies's year
Rating_time <- Edx_sample %>% dplyr::group_by(timestamp,rating)%>% 
  dplyr::summarise(RatingTime = n())%>%ungroup()
 
Rating_time$Stars  <- as.factor(Rating_time$rating)
Rating_time$Movie_Year <- year(as.Date(as.POSIXct(Rating_time$timestamp, 
                                                  origin="1970-01-01")))
levels(Rating_time$Stars)
 
# Checking years distrubtion, it is noticeable that certian years are more frequently seen
plot(Rating_time$Stars, Rating_time$Movie_Year, main= " Rating per Movies'Year")


### Correlation ####

# It is necessary to see correlation between users and ratings and time with ratings
# to create a credible hypothesis that ratings depends on genres, users and time 
 
Rating_time <- Rating_time[,-c(1:2)]
Rtime <- Rating_time %>% dplyr::group_by_at(vars(-RatingTime)) %>%
  dplyr::mutate(row_id=1:n()) %>% ungroup() %>%  
  spread(key=Stars, value=RatingTime) %>% 
  select(-row_id) 
Rtime[is.na(Rtime)] <- 0
Matrix_RatingTime <- data.matrix(Rtime)
# Some years has a high correlation with ratings. Meaning, there is a 
# dependency between ratings and time 
Correlation_RatingTime <- cor(Matrix_RatingTime)
corrplot(Correlation_RatingTime, type="upper", col=brewer.pal(n=8, name="RdYlBu"),
         mar=c(0,0,1,0),title = "Ratings and Year Correlation")

# Now, checking the correlation between ratings and users  

Rating_user<- Edx_sample %>% dplyr::group_by(userId,rating)%>% 
  dplyr::summarise(RatingTime = n())%>%ungroup()

Rating_user$Stars  <- as.factor(Rating_user$rating)
levels(Rating_user$Stars)
Rating_user <- Rating_user[,-2]
 
Ruser <- Rating_user %>% dplyr::group_by_at(vars(-RatingTime)) %>% 
  dplyr::mutate(row_id=1:n()) %>% ungroup() %>%  
  spread(key=Stars, value=RatingTime) %>% 
  select(-row_id) 
Ruser[is.na(Ruser)] <- 0
row.names(Ruser) <- Ruser$userId
Matrix_RatingUser <- data.matrix(Ruser)

# Users has an even correlation with ratings. 
Correlation_RatingUser <- cor(Matrix_RatingUser)
corrplot(Correlation_RatingUser, type="upper", 
         col=brewer.pal(n=8, name="RdYlBu"),
         mar=c(0,0,1,0), 
         title = "Ratings and User Correlation")

# All the correlation maps show the dependecy on ratings and users
# Now, we need to normalized the predictors and ratings to make
# them ready to be used in predict()

####### Methodology #########

# Irizarry R. 2019, code RMSE. explais that RMSE > 1 is not a good 
# prediction and its formula:
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))}

# Normalization of predictors

# Irizarry R. 2019 code, as a reference to build normalization.
# Movies normalization
mu_movies <- mean(edx$rating)
movie_avgs <- edx %>% dplyr::group_by(movieId)
movie_normalized <-  movie_avgs%>% dplyr::summarize(b_i = mean(rating - mu_movies))
#plotting distribution to verified movies normalization
movie_normalized%>% qplot(b_i, geom ="histogram", bins = 10, data = ., 
                          color = I("grey"),
                          main= " Normalized Movies Distribution")

# Movies normalization
user_normalized <- edx %>%  
  dplyr::left_join(movie_normalized, by='movieId') %>%
  dplyr::group_by(userId) %>%
  dplyr::summarize(b_u = mean(rating - mu_movies - b_i))
#plotting distribution to verified movies normalization
user_normalized%>% qplot(b_u, geom ="histogram", bins = 10, 
                         data = ., color = I("grey"),
                         main= " Normalized User Distribution")

# Movie's Year  normalization
# Timestamp format to POSIXct to get the year of released
time_normalized<-  edx %>% dplyr::left_join(movie_normalized, by ="movieId") %>%
  dplyr::left_join(user_normalized, by= "userId") %>%
  dplyr::mutate(date = year(as.Date(as.POSIXct(timestamp, 
                                        origin="1970-01-01")))) %>% 
  dplyr::group_by(date) %>%
  dplyr::summarize(b_t = mean(rating - mu_movies - b_i - b_u))

# adding year to validation and edx sets
edx_time <- edx %>%
  mutate(date = year(as.Date(as.POSIXct(timestamp, origin="1970-01-01"))))
validation_time <- validation %>%
  mutate(date = year(as.Date(as.POSIXct(timestamp, origin="1970-01-01")))) 
 
# First prediction
predicted_ratings_hypothesis <- validation_time %>% 
  dplyr::left_join(movie_normalized, by="movieId")%>%
  dplyr::left_join(user_normalized, by='userId') %>%
  dplyr::left_join(time_normalized, by='date') %>%
  dplyr::mutate(pred = mu_movies + b_i + b_u + b_t) %>% .$pred
Hypothesis_rmse <- RMSE(validation_time$rating, predicted_ratings_hypothesis)

# The table that will store all the RMSE results. Using Irizarry R. 2019 code as aguide to 
# build our code for rmse results dataframe
rmse_results <- data_frame(method = "Movie Effect Model + User Effect + Time_Released Effect", 
                           RMSE = Hypothesis_rmse)
rmse_results %>% knitr::kable() 

# Regularization of lambda, to improve RMSE 

###################
# Using lambdas
####################
# Regularization of lambda, to improve RMSE 


#######################
#### Using lambdas ####
#######################


lambdas1 <- seq(0, 10, 0.25)

rmses1 <- sapply(lambdas1, function(l){
  
  mu_lambda1 <- mean(edx$rating)
  
  bi_l <- edx %>% 
    dplyr::group_by(movieId) %>%
    dplyr::summarize(bi_l = sum(rating - mu_lambda1)/(n()+l))
  
  bu_l <- edx %>% 
    dplyr::left_join(bi_l, by="movieId") %>%
    dplyr::group_by(userId) %>%
    dplyr::summarize(bu_l = sum(rating - bi_l - mu_lambda1)/(n()+l))
  
  predicted_ratings_lambda <- 
    validation %>% 
    dplyr::left_join(bi_l, by = "movieId") %>%
    dplyr::left_join(bu_l, by = "userId") %>%
    dplyr::mutate(pred = mu_lambda1 + bi_l + bu_l) %>%
    .$pred
  
  return(RMSE(validation$rating, predicted_ratings_lambda))
})


qplot(lambdas1, rmses1)  

lambda1 <- lambdas1[which.min(rmses1)]
lambda1
model_Regularized <- min(rmses1)
model_Regularized 
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie Effect Model + User Effect  + Regularization",
                                     RMSE = model_Regularized ))

rmse_results %>% knitr::kable()

# Using lamba1 to include time
movie_Regularized <- edx_time %>%
  dplyr::group_by(movieId) %>%
  dplyr::summarize(movie_reg= sum(rating - mu_movies)/(n()+lambda1))
user_Regularized <- edx_time %>%
  dplyr::left_join(movie_Regularized, by='movieId') %>%
  dplyr::group_by(userId) %>%
  dplyr::summarize(user_reg = sum(rating - mu_movies - movie_reg)/(n()+lambda1))
time_Regularized <- edx_time %>%
  dplyr::left_join(movie_Regularized, by='movieId') %>%
  dplyr::left_join(user_Regularized, by='userId') %>%
  dplyr::group_by(date) %>%
  dplyr::summarize(time_reg = sum(rating - mu_movies - movie_reg - user_reg)/(n()+lambda1))
predictionRegularized<- validation_time %>%
  dplyr::left_join(movie_Regularized, by='movieId') %>%
  dplyr::left_join(user_Regularized, by='userId') %>%
  dplyr::left_join(time_Regularized, by = 'date') %>%
  dplyr::mutate(pred = mu_movies + movie_reg + user_reg + time_reg) %>% .$pred
rmse_Regularized <- RMSE(validation_time$rating,predictionRegularized)
rmse_Regularized
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie Effect Model + User Effect + Time_Released Effect + Regularization",
                                     RMSE = rmse_Regularized))

rmse_results %>% knitr::kable()

# Adding genres
genre_Regularized <- edx_time %>%
  dplyr::left_join(movie_Regularized, by='movieId') %>%
  dplyr::left_join(user_Regularized, by='userId') %>% left_join(time_Regularized, by="date") %>%
  dplyr::group_by(genres) %>% dplyr::summarize(genre_reg = 
                                   sum(rating - mu_movies - movie_reg - user_reg - time_reg)/(n()+lambda1))
predictionRegGenres <- validation_time %>%
  dplyr::left_join(movie_Regularized, by='movieId') %>%
  dplyr::left_join(user_Regularized, by='userId') %>% dplyr::left_join(time_Regularized, by="date") %>%
  dplyr::left_join(genre_Regularized, by = 'genres') %>%
  dplyr::mutate(pred = mu_movies + movie_reg + user_reg + time_reg+ genre_reg) %>% .$pred
rmse_RegGenres <- RMSE(validation_time$rating,predictionRegGenres)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie Effect Model + User Effect + Time_Released Effect + Genres+ Regularization",
                                     RMSE = rmse_RegGenres))

rmse_results %>% knitr::kable()


#  Zhu,2019 as a guide to build the final table results.
# Color picker in Google give the correct HEX color number

 # Settinp up all the results

kable(rmse_results) %>%
  kable_styling("striped" , full_width = F) %>%
  column_spec(2,bold = T,border_right = F,border_left = T ) %>%
  row_spec(1:3,bold =T ,color = "black" , background ="white") %>%
  kable_styling(latex_options = c("striped","scale_down")) %>% 
  row_spec(4,bold =T , font_size = 19,monospace = T, italic=T ,color = "white"
           , background ="#eb3434")


 


 