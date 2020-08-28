rm(list=ls())

setwd("C:/Users/cchousal/Documents/Data Science/FINAL PROJECT WORK/PROJECT-2-8211-BOOK-RECOMMENDER-16JUL2020142438")
books <- read.csv('P2_books.csv')
book_tags <- read.csv('P2_book_tags.csv')
tags <- read.csv('P2_tags.csv')
ratings <- read.csv('P2_ratings.csv')
##------------------------------------------------Phase 1-----------------------------------------------------------------
library(dplyr)
ratings %>% group_by(user_id, book_id) %>% mutate(N=n()) -> ratings
#table(ratings$N)
#ratings %>% filter(N>1) -> dup_ratings

### Removing Duplicate Ratings.
ratings %>% filter(N==1) -> ratings

ratings %>% group_by(user_id) %>% mutate(Ratings_Given = n()) -> ratings
#table(ratings$Ratings_Given)

### Removing users with less than 3 Ratings.
ratings %>% filter(Ratings_Given>2) -> ratings

##------------------------------------------------Phase 2-----------------------------------------------------------------
library(Matrix)
### Select sample of 2% records
set.seed(1) #so that same sample will be selected everytime.
user_fraction <- 0.02
users <- unique(ratings$user_id)
length(users)
sample_users <- sample(users, round(user_fraction*length(users)))
length(sample_users)

ratings %>% filter(user_id %in% sample_users) -> ratings
nrow(ratings)

library(ggplot2)
# Rating Distribution
ratings %>% ggplot(aes(x = rating, fill = factor(rating))) + 
  geom_bar(color = "grey20") + scale_fill_brewer(palette = "YlGnBu") + 
  guides(fill = FALSE)
# Rating per book
ratings %>% group_by(book_id) %>% summarize(number_of_ratings_per_book = n()) %>% 
  ggplot(aes(number_of_ratings_per_book)) + geom_bar(fill = "orange", color = "grey20", width = 1) + coord_cartesian(c(0,40))

# % Distribution of geners
library(stringr)
#Enter list of your fav geners:
genere <- str_to_lower(c("Fiction", "Biography", "Comics", "Mystery", "Horror", "Humor and Comedy", 
                         "Thriller","Science", "Science and Fiction", "Paranormal","History", "Sports"))
available_geners <- genere[str_to_lower(genere) %in% tags$tag_name]
available_tags <- tags$tag_id[match(available_geners,tags$tag_name)]

# Plot of Genere

fav <- book_tags %>% filter(tag_id %in% available_tags) %>% group_by(tag_id) %>% summarize (n =n()) %>% ungroup() %>% 
  mutate(sum = sum(n), percentage = n/sum(n)) %>% arrange(-percentage) %>% left_join(tags, by = "tag_id")

fav %>% ggplot(aes(reorder(tag_name,percentage), percentage, fill = percentage)) + 
  geom_bar(stat = "identity") + coord_flip() + scale_fill_distiller(palette = "YlOrRd") + labs(y = 'Percenage', x = 'Genere')

# Top 10 Books with highest rating
books %>% arrange(-average_rating) %>% top_n(10, wt = average_rating) %>% select(title, authors, average_rating, ratings_count) -> Top10_Rated_Books

# 10 Most Popular Books
books %>% arrange(-ratings_count) %>% top_n(10, wt = ratings_count) %>% select(title, authors, average_rating, ratings_count) -> Top10_Popular_Books

##------------------------------------------------Phase 3-----------------------------------------------------------------
##Recommending
library(recommenderlab)

## Making every user as every row and every column as every column

dimension_names <- list(user_id = sort(unique(ratings$user_id)), book_id = sort(unique(ratings$book_id)))
library(tidyr)
rating_mat <- spread(select(ratings, book_id, user_id, rating), book_id, rating) %>% select(-user_id)
# change from dataframe to matrix
rating_mat <- as.matrix(rating_mat)
class(rating_mat)
# remove user_id
rating_mat[,-1] -> rating_mat
dimnames(rating_mat) <- dimension_names
rating_mat[1:5, 1:5]
dim(rating_mat)

# Convert rating_mat to read rating matrix
rating_mat0 <- rating_mat
rating_mat0[1:5, 1:5]
# convert NA to 0
rating_mat0[is.na(rating_mat0)] <- 0
rating_mat0[1:5, 1:5]
sparse_ratings <- as(rating_mat0, "sparseMatrix") # Saves a lot of space
sparse_ratings[1:5, 1:5]
real_ratings <- new("realRatingMatrix", data = sparse_ratings)
real_ratings

# Split data into train test datasets
sample(x=c(T,F),size=nrow(real_ratings), replace = T, prob = c(0.8,0.2)) -> split_books
real_ratings[split_books,]->recc_train # True values for train dataset
real_ratings[!split_books,]->recc_test # False values for test dataset
class(recc_train)
# Build model from train dataset (UBCF model)
Recommender(data=recc_train,method="UBCF") -> recc_model_ubcf
n_recommended_ubcf<-6

# Predict from test dataset based on built model
predict(object=recc_model_ubcf, newdata=recc_test, n=n_recommended_ubcf)->recc_predicted_model

# Recommend books to users
recc_predicted_ubcf@items[[1]]->user1_book_numbers
recc_predicted_ubcf@itemLabels[user1_book_numbers] -> k

books %>% filter(id %in% k) select(title,authors)











