rm(list = ls())
gc();

setwd("D:/Personal data/Data Science projects/IMDB Data")
options(stringsAsFactors = F)
source("PackageLoadscript.R")
# Load up Movies universe

TitlesUniverse <- read.xlsx("Output Data/TitlesUniverse.xlsx")
TitlesUniverse <- TitlesUniverse[!is.na(TitlesUniverse$averageRating),]
# Source scripts

source("ScrapeRT_IMDBdata.R")
source("PackageLoadscript.R")

#load up twitter auth
source("Twitter_FB_Authentication.R")

# Set constant requestURL
requestURL <- "https://api.twitter.com/oauth/request_token"
# Set constant accessURL
accessURL <- "https://api.twitter.com/oauth/access_token"
# Set constant authURL
authURL <- "https://api.twitter.com/oauth/authorize"

twitterAuth()

# download.file(url="http://curl.haxx.se/ca/cacert.pem",destfile="cacert.pem")




# Get Tweet Data
getTweets <- function(search.string, no.of.tweets)
{
  tweets <- searchTwitter(search.string, n=no.of.tweets, lang="en")
  if(length(tweets) > 0){
  tweets.df <- twListToDF(tweets)
  
  # filter duplicates
  tweets.df %>% group_by(text,favorited,favoriteCount,replyToSN,
                         screenName,retweetCount,isRetweet,retweeted) %>% dplyr::summarise(id = min(as.numeric(id))) -> tweets.df
  
  # Remove special characters from tweets
  tweets.df2 <- gsub(" ?(f|ht)tp(s?)://(.*)[.][a-z]+","",tweets.df$text)
  
  tweets.df2 <- gsub("RT @[a-z,A-Z]*: ","",tweets.df2)
  
  tweets.df2 <- gsub("#[a-z,A-Z]*","",tweets.df2)
  
  tweets.df2 <- gsub("@[a-z,A-Z]*","",tweets.df2)
  
  tweetData <- data.frame(TweetID = as.character(tweets.df$id),
                          TweetText = as.character(tweets.df2),
                          FavoriteCount = as.character(tweets.df$favoriteCount),
                          RetweetCount = as.character(tweets.df$retweetCount))
  
  }else{tweetData <- data.frame(TweetID = as.character(NA),
                                TweetText = as.character(NA),
                                FavoriteCount = as.character(NA),
                                RetweetCount = as.character(NA))}
  
  return(tweetData)
}



# # Get FB page Data
# getPageData <- function()


  # Get sentiment scores
scoreSentiment <- function(ID,tweetdf){
 library("syuzhet")
  

    word.df <- as.vector(tweetdf$TweetText)
    word.df <- iconv(word.df, to = "ASCII", sub = "")
    emotion.df <- get_sentiment(word.df)
    
    tweetdf$EmotionScore <- emotion.df
    tweetdf <- data.table(tweetdf)
    
    positiveSentiment <- tweetdf[EmotionScore > 0,
                                sum(EmotionScore*
                                sum(c((as.numeric(FavoriteCount)*1/3)
                                ,(as.numeric(RetweetCount)*2/3))))]
    
    negativeSentiment <- tweetdf[EmotionScore < 0,
                                 sum(EmotionScore*
                                       sum(c((as.numeric(FavoriteCount)*1/3)
                                             ,(as.numeric(RetweetCount)*2/3))))]
    
    
    emotion.df2 <- data.frame(TitleID = ID,
                              positiveSentiment = positiveSentiment,
                              negativeSentiment = negativeSentiment) 
    
    
    

  
  return(emotion.df2)
}




sampleDataRun <- TitlesUniverse#[sample(1:nrow(TitlesUniverse), 5),]


library(plyr)
library(dplyr)
tweetsAndSentimentData <- data.frame()
AlltweetsData <- data.frame()
errorScrapingRecords <- data.frame()
moviesData <- data.frame()
for(i in 1:nrow(sampleDataRun)){
  print(i)
  
  searchString <- paste0("#",gsub(" ","+",sampleDataRun$title[i]))
  titleID <- sampleDataRun$titleId[i]
  
  Movietweets <- getTweets(searchString,1000)
  
  Movietweets$TitleID <- titleID
  AlltweetsData <- rbind.fill(AlltweetsData, Movietweets)
  if(length(Movietweets) > 0){
  SentimentData <- scoreSentiment(titleID,Movietweets)
  
  tweetsAndSentimentData <- rbind.fill(tweetsAndSentimentData, SentimentData)
  }
  
  titleid <- sampleDataRun$titleId[i]
  title <- sampleDataRun$title[i]
  
  movieData <- try(parseIMDB_RTData(titleid, title),silent = T)
  
  if(class(movieData) == 'try-error'){
    errorRecord <- data.frame(TitleID = titleid,
                              Title = title,
                              ErrorLog = as.character(attributes(movieData)$condition))
    errorScrapingRecords <- rbind.fill(errorScrapingRecords,errorRecord)
  }else{
    moviesData <- rbind.fill(moviesData,movieData)
  }
  
  if(i == 30){
    write.xlsx(tweetsAndSentimentData, "Output Data/TweetSentiments.xlsx")
    write.csv(AlltweetsData, "Output Data/AlltweetsData.csv")
    write.xlsx(moviesData, "Output Data/sampleMoviesData.xlsx")
  }else if(i%%30 == 0){
    x1 <- read.xlsx("Output Data/TweetSentiments.xlsx")
    tweetsAndSentimentData <- rbind.fill(x1,tweetsAndSentimentData)
    
    x2 <- read.csv("Output Data/AlltweetsData.csv")
    AlltweetsData <- rbind.fill(x2,AlltweetsData)
    
    x3 <- read.xlsx("Output Data/sampleMoviesData.xlsx")
    moviesData <- rbind.fill(x3,moviesData)
    
    
    write.xlsx(tweetsAndSentimentData, "Output Data/TweetSentiments.xlsx")
    write.csv(AlltweetsData, "Output Data/AlltweetsData.csv")
    write.xlsx(moviesData, "Output Data/sampleMoviesData.xlsx")
    
    tweetsAndSentimentData <- data.frame()
    AlltweetsData <- data.frame()
    moviesData <- data.frame()
  }
  
}

