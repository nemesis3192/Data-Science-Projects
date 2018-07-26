library(wordcloud)
library(tm)
library(twitteR)
library(ROAuth)
library(plyr)
library(stringr)
library(base64enc)
library(data.table)
library(magrittr)
library(openxlsx)


title_akas_data <- fread("D:/Personal data/Data Science projects/IMDB Data/Raw IMDB data/title_akas_data.tsv")
title_basics_data <- fread("D:/Personal data/Data Science projects/IMDB Data/Raw IMDB data/title_basics_data.tsv")
title_crew_data <- fread("D:/Personal data/Data Science projects/IMDB Data/Raw IMDB data/title_crew_data.tsv")
title_ratings_data <- fread("D:/Personal data/Data Science projects/IMDB Data/Raw IMDB data/title_ratings_data.tsv")


HollyWoodUniverse <- merge(title_akas_data[which((title_akas_data$region %in% c('US') ) & 
                                      title_akas_data$types != "working"),],
              title_basics_data[which(title_basics_data$titleType == 'movie' & 
                                      title_basics_data$startYear >= 2013)],
              by.x = "titleId", by.y = "tconst")
print(nrow(HollyWoodUniverse))

HollyWoodUniverse <- merge(HollyWoodUniverse, title_crew_data[,c("tconst","directors")],
                           by.x = "titleId", by.y = "tconst")


HollyWoodUniverse <- merge(HollyWoodUniverse, title_ratings_data,
                           by.x = "titleId", by.y = "tconst", all.x = T)

BollyWoodUniverse <- merge(title_akas_data[which((title_akas_data$region %in% c('IN') ) & 
                                                   title_akas_data$types != "working"),],
                           title_basics_data[which(title_basics_data$titleType == 'movie' & 
                                                     title_basics_data$startYear >= 2013)],
                           by.x = "titleId", by.y = "tconst")
print(nrow(BollyWoodUniverse))

BollyWoodUniverse <- merge(BollyWoodUniverse, title_crew_data[,c("tconst","directors")],
                           by.x = "titleId", by.y = "tconst")


BollyWoodUniverse <- merge(BollyWoodUniverse, title_ratings_data,
                           by.x = "titleId", by.y = "tconst", all.x = T)


colsrequired <- c("titleId","title","language","startYear","genres","isAdult"
                  ,"runtimeMinutes","directors","averageRating","numVotes")


HollyWoodUniverse <- HollyWoodUniverse[,colsrequired,with = F]
BollyWoodUniverse <- BollyWoodUniverse[,colsrequired,with = F]

HollyWoodUniverse$Industry <- "Hollywood"
BollyWoodUniverse$Industry <- "Bollywood"


TitlesUniverse <- rbind(HollyWoodUniverse, BollyWoodUniverse)

write.xlsx(TitlesUniverse, "D:/Personal data/Data Science projects/IMDB Data/Output Data/TitlesUniverse.xlsx")