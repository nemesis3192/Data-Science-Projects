B <- read.xlsx("Bollywood Curated universe.xlsx")



B$Profit <- as.numeric(B$TMDbRevenue) - as.numeric(B$TMDbBudget)


# Select feature space
BollywoodData <- data.frame(TitleID = B$TitleID,
                            Title = B$Title,
                            IMDBRating = as.numeric(B$IMDBRating),
                            IMDBVotes = as.integer(B$IMDBVotes),
                            TMDBRating = as.numeric(B$TMDbVote),
                            TMDBVotes = as.integer(B$TMDbVoteCount),
                            Genres = as.character(gsub(" ,","",B$IMDBGenres)),
                            Certification = as.character(B$IMDBCertificate),
                            ReleaseDate = ymd(parse_date_time(trimws(B$IMDBReleaseDate,which = "both"),orders = "dmy")),
                            Day = weekdays(parse_date_time(trimws(B$IMDBReleaseDate,which = "both"),orders = "dmy")),
                            Runtime = as.numeric(B$TMDbRunTime),
                            Earnings = as.numeric(B$TMDbRevenue),
                            Profits = B$Profit
                            )




uniqueGenres <- unique(c("Action","Crime","Drama","Mystery","Thriller","Action","Drama","Comedy"
,"Fantasy","Horror","Romance","Romance","Adventure","Music","Musical"
,"Music","Sport","Fantasy","Biography","Horror","Musical","History","Thriller"
,"War","Sci-Fi","Comedy","Animation","Family","History","Mystery","Sci-Fi"
,"Family","Crime","Documentary"))


# 3 April to 26 May 2013
# 16 April 2014 to 1 June 2014.
# 8 April 2015 to 24 May 2015
# began on 9 April 2016, and concluded on 29 May 2016
# 5 April 2017 and finished on 21 May 2017
# 7 April to 27 May 2018


IPLseason <- c(interval(ymd("2013-04-03"),ymd("2013-05-26")),
               interval(ymd("2014-04-16"),ymd("2013-06-01")),
               interval(ymd("2015-04-08"),ymd("2015-05-24")),
               interval(ymd("2016-04-09"),ymd("2016-05-29")),
               interval(ymd("2017-04-05"),ymd("2017-05-21")),
               interval(ymd("2018-04-07"),ymd("2018-05-27")))

Holidays <- c(mdy("8-20-2013"),mdy("8-10-2014"),mdy("8-29-2015"),
              mdy("8-18-2016"),mdy("8-7-2017"),mdy("8-26-2018"),
              mdy("11-3-2013"),mdy("10-23-2014"),mdy("11-11-2015"),
              mdy("10-30-2016"),mdy("10-19-2017"),mdy("11-7-2018"),
              mdy("10-27-2019"),mdy("8-9-2013"),mdy("7-29-2014"),
              mdy("7-19-2015"),mdy("7-6-2016"),mdy("6-26-2017"),
              mdy("6-15-2018"),mdy("8-15-2013"),mdy("8-15-2014"),
              mdy("8-15-2015"),mdy("8-15-2016"),mdy("8-15-2017"),
              mdy("8-15-2018"),mdy("1-26-2013"),mdy("1-26-2014"),
              mdy("1-26-2015"),mdy("1-26-2016"),mdy("1-26-2017"),
              mdy("1-26-2018"),mdy("12-25-2013"),mdy("12-25-2014"),
              mdy("12-25-2015"),mdy("12-25-2016"),mdy("12-25-2017"),
              mdy("12-25-2018"))

BollywoodData$InIPLSeasonsYN <- BollywoodData$ReleaseDate %within% IPLseason
BollywoodData$ReleaseOnHoliday <- BollywoodData$ReleaseDate %in% Holidays



means <- rep(0,length(uniqueGenres))

for(i in 1:length(uniqueGenres)){
  genre <- uniqueGenres[i]
  BollywoodData$Genre <- ifelse(grepl(genre,BollywoodData$Genres,ignore.case = T),1,0)
  means[i] <- mean(BollywoodData$Profits[grepl(genre,BollywoodData$Genres,ignore.case = T)],na.rm = T)
  data.table::setnames(BollywoodData,"Genre",genre)
}

means <- data.frame(AverageReturns = means/1000000,Genre = uniqueGenres)

# barplot(means$AverageReturns, main = "Average imdb scores for different genres"
#         ,names.arg = means$Genre)

p <-ggplot(means, aes(Genre, AverageReturns))
p+geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ylab("AverageReturns in USD Million")


# Look at missing values
colSums(sapply(BollywoodData, is.na))

# Certification data has most (22) missing records
# Heatmap of missing data
missing.values <- aggr(BollywoodData, sortVars = T, prop = T, sortCombs = T
                       , cex.lab = 1.5, cex.axis = .6, cex.numbers = 5
                       , combined = F, gap = -.2)

# To check impact of certification on profits
table(BollywoodData$Certification)
# Impute values Certification R

BollywoodData <- subset(BollywoodData, select = -c(Certification,Genres))



# Classify the movies in four classes based on returns (Rank commercial success)
successClass <- within(BollywoodData, quartile <- as.integer(cut(Earnings, quantile(Earnings, probs=0:4/4,na.rm = T)
                                                        , include.lowest=TRUE)))

BollywoodData$Success <- successClass$quartile

# Commercial success vs critical acclaim 
BollywoodData %>%
  top_n(20, Profits) %>%
  ggplot(aes(x = IMDBRating, y = Earnings/10^6, size = Profits/10^6, color = IMDBVotes)) + 
  geom_point() + 
  geom_hline(aes(yintercept = 100)) + 
  geom_vline(aes(xintercept = 7.5)) + 
  geom_text_repel(aes(label = Title), size = 4) +
  xlab("Imdb score") + 
  ylab("Gross money earned in million dollars") + 
  ggtitle("Commercial success Vs Critical acclaim") +
  annotate("text", x = 9.5, y = 350, label = "High ratings \n & High gross") +
  theme(plot.title = element_text(hjust = 0.5)) + labs(size = 'Profits in USD million')


# Fetch Cast & Crew popularity
PeopleData <- read.xlsx("peopleData1.xlsx")
CastData <- read.xlsx("allCastData1.xlsx")
CrewData <- read.xlsx("allCrewData1.xlsx")

CastPopularityIndex <- merge(CastData,PeopleData,by.x = "id", by.y = "PersonID",all.x = T) %>% 
  subset(.,select = c(TitleID.x, id, name,PopularityIndex))


CrewPopularityIndex <- merge(CrewData,PeopleData,by.x = "id", by.y = "PersonID",all.x = T) %>% 
  subset(.,select = c(TitleID.x, id, name,department,PopularityIndex))



# Get Cast popularity average
TitleCastPopularity <- CastPopularityIndex %>% group_by(TitleID.x) %>% 
  dplyr::summarise(AverageCastPopularity = mean(as.numeric(PopularityIndex),na.rm = T))

# Get Department-wise crew popularity
CrewPopularityIndex <- dcast(CrewPopularityIndex,TitleID.x+id+name~department,value.var = 'PopularityIndex')
TitleCrewPopularity <- CrewPopularityIndex %>% group_by(TitleID.x) %>% 
  dplyr::summarise(Direction = mean(as.numeric(Directing),na.rm = T),
                   Production = mean(as.numeric(Production),na.rm = T),
                   Music = mean(as.numeric(Sound),na.rm = T))



# combine crew and cast data
CrewCastIndex <- merge(TitleCastPopularity,TitleCrewPopularity,
                       by = 'TitleID.x',all = T) #%>% subset(.,select = -c)

is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

CrewCastIndex[is.nan(CrewCastIndex)] <- 0
CrewCastIndex[is.na(CrewCastIndex)] <- 0


# Combine Cast & Crew index with Movie data
B_MovieData <- merge(CrewCastIndex, BollywoodData,
                     by.x = "TitleID.x", by.y = "TitleID",all.y = T)

colnames(B_MovieData) <- gsub("[.]x","",colnames(B_MovieData))



# Relationship of Cast popularity vs commercial success
B_MovieData %>%
  plot_ly(x = ~AverageCastPopularity, y = ~Profits, color = ~IMDBRating 
          , mode = "markers", text = ~Title, alpha = 0.7, type = "scatter"
          ,name = c(B_MovieData$Title),hoverlabel = 'name')



# Relationship of Director popularity vs commercial success

B_MovieData %>%
  plot_ly(x = ~Direction, y = ~Earnings, color = ~IMDBRating 
          , mode = "markers", text = ~Title, alpha = 0.7, type = "scatter"
          ,name = c(B_MovieData$Title),hoverlabel = 'name')


# Relationship of Producer popularity vs commercial success
B_MovieData %>%
  plot_ly(x = ~Production, y = ~Earnings, color = ~IMDBRating 
          , mode = "markers", text = ~Title, alpha = 0.7, type = "scatter"
          ,name = c(B_MovieData$Title),hoverlabel = 'name')


# Relationship of Music director popularity vs commercial success
B_MovieData %>%
  plot_ly(x = ~Music, y = ~Earnings, color = ~IMDBRating 
          , mode = "markers", text = ~Title, alpha = 0.7, type = "scatter"
          ,name = c(B_MovieData$Title),hoverlabel = 'name')



# Remove or Convert char data to factor data
B_MovieData$Day <- as.factor(B_MovieData$Day)
B_MovieData$InIPLSeasonsYN <- as.integer(B_MovieData$InIPLSeasonsYN)
B_MovieData$ReleaseOnHoliday <- as.integer(B_MovieData$ReleaseOnHoliday)
# No Conclusive proof of relation between social popularity and commmercial success.
# Though we do assume movies with more popular starcast ends making more revenue, that is 
# not translated to profits


# Remove NA records
train <- train[, colSums(is.na(train)) == 0]



# Plot correlation headmap
ggcorr(B_MovieData[,c(1:11,32)], label = TRUE, label_round = 2, label_size = 3.5, size = 2, hjust = .85) +
  ggtitle("Correlation Heatmap") +
  theme(plot.title = element_text(hjust = 0.5))


# Removing TMDb votes as it shows very high 
B_MovieData <- subset(B_MovieData, select = -c(TMDBVotes,Earnings,Profits,Title
                                                   ,TitleID,ReleaseDate))


# Start ML Modelling

# Here we split data into training, validation and test sets with the ratio of 6:2:2.
colnames(B_MovieData) <- gsub("-","_",colnames(B_MovieData))

set.seed(1234)
train.index <- sample(row.names(B_MovieData), dim(B_MovieData)[1]*0.6)
valid.index <- sample(setdiff(row.names(B_MovieData), train.index), dim(B_MovieData)[1]*0.2)
test.index <- setdiff(row.names(B_MovieData), union(train.index, valid.index))
train <- B_MovieData[train.index, ]
valid <- B_MovieData[valid.index, ]
test <- B_MovieData[test.index, ]


library(nnet)
mlm <- multinom(formula = Success~., data = train)

summary(mlm)


# Calculate Z score & p-value
z <- summary(mlm)$coefficients/summary(mlm)$standard.errors

p <- (1 - pnorm(abs(z), 0, 1))*2

# Apply model on validation set
mlm.predict.value <- predict(mlm,valid,type = 'probs')


bpp=cbind(valid, mlm.predict.value)

by(bpp[,34:37], bpp$Success, colMeans)


library(rpart)
library(rpart.plot)
# Full grown tree
class.tree <- rpart(Success ~ ., data = train, method = "class")
## plot tree
prp(class.tree, type = 1, extra = 1, under = TRUE, split.font = 2, varlen = 0) 


set.seed(51)
cv.ct <- rpart(Success ~ . , data = train, method = "class", 
               cp = 0.00001, minsplit = 5, xval = 5)
printcp(cv.ct)

# This shows us that 6th tree has lowest cross validation error (0.55670)

pruned.ct <- prune(cv.ct, 
                   cp = cv.ct$cptable[which.min(cv.ct$cptable[,"xerror"]),"CP"])
length(pruned.ct$frame$var[pruned.ct$frame$var == "<leaf>"])
prp(pruned.ct, type = 1, extra = 1, split.font = 1, varlen = -10)


# Model application
# apply model on training set
tree.pred.train <- predict(pruned.ct, train, type = "class")
# generate confusion matrix for training data
confusionMatrix(tree.pred.train, train$Success)
# Accuracy is 0.8222 for training set.


# apply model on validation set
tree.pred.valid <- predict(pruned.ct, valid, type = "class")
# generate confusion matrix for validation data
confusionMatrix(tree.pred.valid, as.factor(valid$Success))

# Accuracy is 0.5682 for validation set.



# apply model on test set
tree.pred.test <- predict(pruned.ct, test, type = "class")
# generate confusion matrix for test data
confusionMatrix(tree.pred.test, as.factor(test$Success))

# Accuracy is 0.5778 for test set.



# Apply Random forest

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
confusionMatrix(rf.pred.valid, as.factor(valid$Success))

# Accuracy is 0.5909 for validation set.


# Test set
set.seed(633)
# apply model on test set
rf.pred.test <- predict(rf, test)
# generate confusion matrix for test data
confusionMatrix(rf.pred.test, as.factor(test$Success))

# Accuracy is 0.5778 for Test set.
