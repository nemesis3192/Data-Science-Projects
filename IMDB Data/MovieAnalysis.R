# IMDB <- read.csv("From Kaggle/movie_metadata.csv")
# 
# IMDBMovies <- read.csv("From Kaggle/tmdb_5000_movies.csv")



rm(list = ls())
gc();

setwd("D:/Personal data/Data Science projects/IMDB Data")
options(stringsAsFactors = F)
source("PackageLoadscript.R")


IMDB <- read.csv("input/imdb-5000-movie-dataset/movie_metadata.csv")
str(IMDB)


IMDB <- IMDB[which(
                    (IMDB$country %in% c('USA','India')) & (IMDB$language %in% c('English','Hindi'))
                  ), ]

IMDB <- IMDB[!(duplicated(IMDB) | is.na(IMDB$gross)), ]


# Data Cleaning
IMDB$movie_title <- gsub("Â", "", as.character(factor(IMDB$movie_title)))
IMDB$movie_title <- str_trim(IMDB$movie_title, side = "right")
IMDB <- IMDB[!(is.na(IMDB$gross) | is.na(IMDB$budget)), ]

IMDB <- IMDB %>% 
  mutate(profit = gross - budget,
         ProfitRatio = (profit/budget)*100)


genres.df <- as.data.frame(IMDB[,c("genres", "ProfitRatio")])

uniqueGenres <- paste0(genres.df$genres,collapse = "|")
uniqueGenres <- unique(unlist(str_split(uniqueGenres,"[|]")[[1]]))

means <- rep(0,23)
for(i in 1:length(uniqueGenres)){
  genre <- uniqueGenres[i]
  IMDB$Genre <- ifelse(grepl(genre,IMDB$genres,ignore.case = T),1,0)
  data.table::setnames(IMDB,"Genre",genre)
  means[i] <- mean(IMDB$ProfitRatio[IMDB[i+2]==1],na.rm = T)
}




means <- data.frame(AverageReturns = means,Genre = uniqueGenres)
# This shows us that certain Genres tend to do well over others.


# Look at missing values
colSums(sapply(IMDB, is.na))


# Heatmap of missing data
missing.values <- aggr(IMDB, sortVars = T, prop = T, sortCombs = T
                       , cex.lab = 1.5, cex.axis = .6, cex.numbers = 5
                       , combined = F, gap = -.2)




# Check importance of aspect_ratio as a feature
# Since majority of the records are either 1.85 or 2.35
# we make them into three categories and study the difference in ProfitRatio
IMDB$aspect_ratio[is.na(IMDB$aspect_ratio)] <- 0
mean(IMDB$ProfitRatio[IMDB$aspect_ratio == 1.85])
mean(IMDB$ProfitRatio[IMDB$aspect_ratio == 2.35])
mean(IMDB$ProfitRatio[IMDB$aspect_ratio != 1.85 & IMDB$aspect_ratio != 2.35])


# We see that aspect_ratio = 1.85 has very high ProfitRatio
# We change the feature to a binary class feature aspect_ratio
# aspect_ratio = 1 if aspect_ratio = 1.85 else 0

IMDB$aspect_ratio <- ifelse(IMDB$aspect_ratio == 1.85, 'Yes', 'No')
IMDB$aspect_ratio <- as.factor(IMDB$aspect_ratio)



# replace NA with column average for facenumber_in_poster
IMDB$facenumber_in_poster[is.na(IMDB$facenumber_in_poster)] <- round(mean(IMDB$facenumber_in_poster, na.rm = TRUE))
# convert 0s into NAs for other predictors
IMDB[,c(5,6,8,13,24,26)][IMDB[,c(5,6,8,13,24,26)] == 0] <- NA
# impute missing value with column mean
IMDB$num_critic_for_reviews[is.na(IMDB$num_critic_for_reviews)] <- round(mean(IMDB$num_critic_for_reviews, na.rm = TRUE))
IMDB$duration[is.na(IMDB$duration)] <- round(mean(IMDB$duration, na.rm = TRUE))
IMDB$director_facebook_likes[is.na(IMDB$director_facebook_likes)] <- round(mean(IMDB$director_facebook_likes, na.rm = TRUE))
IMDB$actor_3_facebook_likes[is.na(IMDB$actor_3_facebook_likes)] <- round(mean(IMDB$actor_3_facebook_likes, na.rm = TRUE))
IMDB$actor_1_facebook_likes[is.na(IMDB$actor_1_facebook_likes)] <- round(mean(IMDB$actor_1_facebook_likes, na.rm = TRUE))
IMDB$cast_total_facebook_likes[is.na(IMDB$cast_total_facebook_likes)] <- round(mean(IMDB$cast_total_facebook_likes, na.rm = TRUE))
IMDB$actor_2_facebook_likes[is.na(IMDB$actor_2_facebook_likes)] <- round(mean(IMDB$actor_2_facebook_likes, na.rm = TRUE))
IMDB$movie_facebook_likes[is.na(IMDB$movie_facebook_likes)] <- round(mean(IMDB$movie_facebook_likes, na.rm = TRUE))






IMDB <- IMDB[!(IMDB$content_rating %in% ""),]

IMDB$content_rating[IMDB$content_rating == 'M']   <- 'PG' 
IMDB$content_rating[IMDB$content_rating == 'GP']  <- 'PG' 
IMDB$content_rating[IMDB$content_rating == 'X']   <- 'NC-17'
IMDB$content_rating[IMDB$content_rating == 'Approved']  <- 'R' 
IMDB$content_rating[IMDB$content_rating == 'Not Rated'] <- 'R' 
IMDB$content_rating[IMDB$content_rating == 'Passed']    <- 'R' 
IMDB$content_rating[IMDB$content_rating == 'Unrated']   <- 'R' 
IMDB$content_rating <- factor(IMDB$content_rating)





# delete predictor color as most movies will be coloured
IMDB <- subset(IMDB, select = -c(color))




# country feature
IMDB$country[(IMDB$country != 'USA') & (IMDB$country != 'India')] <- 'Others' 
IMDB <- IMDB[!(IMDB$country == 'Others'),]
IMDB$country <- factor(IMDB$country)




## 4 Data Visualization

### 4.1 Histogram of Movie Released

# Movie production just exploded after year 1990. It could be due to advancement in technology and commercialisation of internet.


ggplot(IMDB, aes(title_year)) +
  geom_bar() +
  labs(x = "Year movie was released", y = "Movie Count", title = "Histogram of Movie released") +
  theme(plot.title = element_text(hjust = 0.5))

IMDB <- IMDB[IMDB$title_year >= 2005,]


# Analyse Director correlation with commercial success
 IMDB %>%
  group_by(director_name) %>%
  summarise(avg_roi = mean(ProfitRatio)) %>%
  arrange(desc(avg_roi)) %>%
  top_n(20, avg_roi) %>%
  formattable(list(avg_roi = color_bar("orange")), align = 'l')

 
 
 
 # Commercial success vs critical acclaim 
 IMDB %>%
   top_n(20, profit) %>%
   ggplot(aes(x = imdb_score, y = gross/10^6, size = profit/10^6, color = content_rating)) + 
   geom_point() + 
   geom_hline(aes(yintercept = 600)) + 
   geom_vline(aes(xintercept = 7.75)) + 
   geom_text_repel(aes(label = movie_title), size = 4) +
   xlab("Imdb score") + 
   ylab("Gross money earned in million dollars") + 
   ggtitle("Commercial success Vs Critical acclaim") +
   annotate("text", x = 8.5, y = 700, label = "High ratings \n & High gross") +
   theme(plot.title = element_text(hjust = 0.5))

 
 
 
 
# Relationship of Social outreach vs commercial success
 IMDB %>%
   plot_ly(x = ~movie_facebook_likes, y = ~profit, color = ~content_rating , mode = "markers", text = ~content_rating, alpha = 0.7, type = "scatter")
 
 # No Conclusive proof of relation
 
 
 
 
 
# Data Preprocessing
 IMDB <- subset(IMDB, select = -c(director_name, actor_2_name, actor_1_name,
                                  movie_title, actor_3_name, plot_keywords, 
                                  movie_imdb_link, movie_facebook_likes,
                                  cast_total_facebook_likes,director_facebook_likes,
                                  actor_1_facebook_likes,actor_3_facebook_likes,
                                  actor_2_facebook_likes))
 
 
 
 # Since we are using ROI as the measure of success, remove linear dependent variables
 IMDB <- subset(IMDB, select = -c(ProfitRatio,budget,gross))
 
 
# Look for correlation between feature-set
 ggcorr(IMDB, label = TRUE, label_round = 2, label_size = 3.5, size = 2, hjust = .85) +
   ggtitle("Correlation Heatmap") +
   theme(plot.title = element_text(hjust = 0.5))
 
 # We see high correlation between num_user_for_reviews & num_voted_users (> 0.75)
 IMDB$critic_review_ratio <- IMDB$num_critic_for_reviews / IMDB$num_user_for_reviews
 
 IMDB <- subset(IMDB, select = -c(num_critic_for_reviews, num_user_for_reviews))

 
 
# Re-check the correlation heatmap
 ggcorr(IMDB[,c(8:30)], label = TRUE
        , label_round = 2, label_size = 3.5, size = 2, hjust = .85) +
   ggtitle("Correlation Heatmap") +
   theme(plot.title = element_text(hjust = 0.5))
 
 
# Classify the movies in four classes based on returns (Rank commercial success)
successClass <- within(IMDB, quartile <- as.integer(cut(profit, quantile(profit, probs=0:4/4)
                                                             , include.lowest=TRUE)))
table(successClass$quartile)

IMDB$Success <- successClass$quartile



IMDB <- subset(IMDB, select = -c(profit,genres,language,facenumber_in_poster))

IMDB$Success <- ifelse(IMDB$Success == 4, "SuperHit",
                       ifelse(IMDB$Success == 3, "Hit",
                        ifelse(IMDB$Success == 2, "Average","Flop")))
IMDB$Success <- as.factor(IMDB$Success)
# As you see the movies are equally divided into four classes. $ being the movie with Best ROI






# Start ML Modelling

# Here we split data into training, validation and test sets with the ratio of 6:2:2.
colnames(IMDB) <- gsub("-","_",colnames(IMDB))

set.seed(1234)
train.index <- sample(row.names(IMDB), dim(IMDB)[1]*0.6)
valid.index <- sample(setdiff(row.names(IMDB), train.index), dim(IMDB)[1]*0.2)
test.index <- setdiff(row.names(IMDB), union(train.index, valid.index))
train <- IMDB[train.index, ]
valid <- IMDB[valid.index, ]
test <- IMDB[test.index, ]



# implement algorithm
olm <- polr(formula = Success~duration + num_voted_users + country + 
       content_rating + title_year + imdb_score + aspect_ratio + 
       critic_review_ratio,data = train)

library(nnet)
mlm <- multinom(formula = Success~., data = train)






library(rpart)
library(rpart.plot)
# Full grown tree
class.tree <- rpart(Success ~ ., data = train, method = "class")
## plot tree
prp(class.tree, type = 1, extra = 1, under = TRUE, split.font = 2, varlen = 0) 



# cross-validation procedure
# argument cp sets the smallest value for the complexity parameter.
set.seed(51)
cv.ct <- rpart(Success ~ . , data = train, method = "class", 
               cp = 0.00001, minsplit = 5, xval = 5)
printcp(cv.ct)

# This shows us that 11th tree has lowest cross validation error (0.69915)


pruned.ct <- prune(cv.ct, 
                   cp = cv.ct$cptable[which.min(cv.ct$cptable[,"xerror"]),"CP"])
length(pruned.ct$frame$var[pruned.ct$frame$var == "<leaf>"])
prp(pruned.ct, type = 1, extra = 1, split.font = 1, varlen = -10)

# Model application
# apply model on training set
tree.pred.train <- predict(pruned.ct, train, type = "class")
# generate confusion matrix for training data
confusionMatrix(tree.pred.train, train$Success)

# Accuracy is 0.5913 for training set.

# apply model on validation set
tree.pred.valid <- predict(pruned.ct, valid, type = "class")
# generate confusion matrix for validation data
confusionMatrix(tree.pred.valid, valid$Success)

# Accuracy is 0.4215 for validation set.

# apply model on test set
tree.pred.test <- predict(pruned.ct, test, type = "class")
# generate confusion matrix for test data
confusionMatrix(tree.pred.test, test$Success)

# Accuracy is 0.4379 for test set.


library(FNN)

# Create copy for data
IMDB2 <- IMDB

IMDB2[,c("content_G", "content_NC17", "content_PG", "content_PG13", "content_R")] <- model.matrix( ~ content_rating - 1, data = IMDB2)
IMDB2[,c("aspect_Yes", "aspect_No")] <- model.matrix( ~ aspect_ratio - 1, data = IMDB2)
IMDB2[,c("country_US", "country_IN")] <- model.matrix( ~ country - 1, data = IMDB2)

IMDB2$Success <- as.character(IMDB2$Success)
IMDB2$Success <- ifelse(IMDB2$Success == "SuperHit", 4,
                       ifelse(IMDB2$Success == "Hit", 3,
                              ifelse(IMDB2$Success == "Average",2,1)))

# Select only usefull column
IMDB2 <- IMDB2[,c(1,2,5,6,8:41)]

set.seed(1235)
train2 <- IMDB2[train.index, ]
valid2 <- IMDB2[valid.index, ]
test2 <- IMDB2[test.index, ]


# initialize normalized training, validation, test data, complete data frames to originals
train2.norm <- train2
valid2.norm <- valid2
test2.norm <- test2
IMDB2.norm <- IMDB2


norm.values <- preProcess(train2[, -29], method=c("center", "scale"))
train2.norm[, -29] <- predict(norm.values, train2[, -29])
valid2.norm[, -29] <- predict(norm.values, valid2[, -29])
test2.norm[, -29] <- predict(norm.values, test2[, -29])
IMDB2.norm[, -29] <- predict(norm.values, IMDB2[, -29])




# initialize a data frame with two columns: k, and accuracy.
accuracy.df <- data.frame(k = seq(1, 20, 1), accuracy = rep(0, 20))
# compute knn for different k on validation data.
for(i in 1:20) {
  knn.pred <- knn(train2.norm[, -29], valid2.norm[, -29],
                  cl = train2.norm[, 29], k = i)
  accuracy.df[i, 2] <- confusionMatrix(knn.pred, valid2.norm[, 29])$overall[1]
}
accuracy.df



# train <- train[!(train$Success == "Hit"),]
library(randomForest)
set.seed(1236)
rf <- randomForest(Success ~ ., data = train, mtry = 5)
# Show model error
plot(rf)
legend('topright', colnames(rf$err.rate), col=1:5, fill=1:5)



# Look at GINI index
importance <- importance(rf)
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))


# Create a rank variable based on importance
rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))

# Visualize the results
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'red') +
  labs(x = 'Variables') +
  coord_flip() + 
  theme_few()



set.seed(632)
# apply model on validation set
rf.pred.valid <- predict(rf, valid)
# generate confusion matrix for validation data
confusionMatrix(rf.pred.valid, valid$Success)



# Accuracy is 0.4943 for validation set.


# Confusion Matrix and Statistics
# 
# Reference
# Prediction Average Flop Hit SuperHit
# Average       49   29  24        8
# Flop          39   75  24       18
# Hit           21   15  52       26
# SuperHit      14   20  26       82
# 
# Overall Statistics
# 
# Accuracy : 0.4943         
# 95% CI : (0.4505, 0.538)
# No Information Rate : 0.2663         
# P-Value [Acc > NIR] : <2e-16         
# 
# Kappa : 0.3241         
# Mcnemar's Test P-Value : 0.4828         
# 
# Statistics by Class:
# 
# Class: Average Class: Flop Class: Hit Class: SuperHit
# Sensitivity                 0.39837      0.5396    0.41270          0.6119
# Specificity                 0.84712      0.7885    0.84343          0.8454
# Pos Pred Value              0.44545      0.4808    0.45614          0.5775
# Neg Pred Value              0.82039      0.8251    0.81863          0.8632
# Prevalence                  0.23563      0.2663    0.24138          0.2567
# Detection Rate              0.09387      0.1437    0.09962          0.1571
# Detection Prevalence        0.21073      0.2989    0.21839          0.2720
# Balanced Accuracy           0.62275      0.6640    0.62807          0.7287





# Test set
set.seed(633)
# apply model on test set
rf.pred.test <- predict(rf, test)
# generate confusion matrix for test data
confusionMatrix(rf.pred.test, test$Success)

# Accuracy is 0.5086 for Test set.

# Confusion Matrix and Statistics
# 
# Reference
# Prediction Average Flop Hit SuperHit
# Average       76   25  31       12
# Flop          33   66  35       22
# Hit           13   17  46       25
# SuperHit      12   10  22       78
# 
# Overall Statistics
# 
# Accuracy : 0.5086          
# 95% CI : (0.4649, 0.5523)
# No Information Rate : 0.262           
# P-Value [Acc > NIR] : < 2.2e-16       
# 
# Kappa : 0.3462          
# Mcnemar's Test P-Value : 0.003554        
# 
# Statistics by Class:
# 
# Class: Average Class: Flop Class: Hit Class: SuperHit
# Sensitivity                  0.5672      0.5593    0.34328          0.5693
# Specificity                  0.8252      0.7778    0.85861          0.8860
# Pos Pred Value               0.5278      0.4231    0.45545          0.6393
# Neg Pred Value               0.8470      0.8583    0.79147          0.8529
# Prevalence                   0.2562      0.2256    0.25621          0.2620
# Detection Rate               0.1453      0.1262    0.08795          0.1491
# Detection Prevalence         0.2753      0.2983    0.19312          0.2333
# Balanced Accuracy            0.6962      0.6685    0.60095          0.7277