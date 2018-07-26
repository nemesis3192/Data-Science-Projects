# Pull IMDB and TMDb data
rm(list = ls())
gc();


TitlesUniverse <- read.xlsx("Output Data/TitlesUniverse.xlsx")
TitlesUniverse <- TitlesUniverse[!is.na(TitlesUniverse$averageRating),]
Buniverse <- TitlesUniverse[TitlesUniverse$Industry == "Bollywood",]
Huniverse <- TitlesUniverse[TitlesUniverse$Industry == "Hollywood",]


Buniverse <- Buniverse[sample(1:nrow(Buniverse), nrow(Buniverse)),]
Huniverse <- Huniverse[sample(1:nrow(Huniverse), 5000),]


CombinedUniverse <- rbind(Buniverse,Huniverse)


source("PackageLoadscript.R")

library(rvest)


# This function pulls the data for movies
parseData <- function(titleID,title,universe){
  TitlePage <- try(read_html(paste0("http://www.imdb.com/title/",titleID,"/")),silent = T)
  if(class(TitlePage) == 'try-error'){
    IMDBMovieData <- data.frame(TitleID = titleID,
                                Title = title,
                                IMDBErrorLog = as.character(paste0("IMDB TitlePage Error:",attributes(TitlePage)$condition,collapse = "")))
  }else{
    
    # CreditsPage <- try(read_html(paste0("http://www.imdb.com/title/",titleID,"/fullcredits?")),silent = T)
    # if(class(CreditsPage) == 'try-error'){
    #   director <- as.character(NA)
    #   musicdirector <- as.character(NA)
    #   IMDBErrorLog <- as.character(paste0("IMDB CreditsPage Error:",attributes(CreditsPage)$condition,collapse = ""))
    # }else{
      # ErrorLog <- as.character(NA)
      # fullCredits <- CreditsPage %>% html_nodes(xpath = "//*[@id=\"fullcredits_content\"]") %>% html_text() %>% gsub("(?<=[\\s])\\s*|^\\s+|\\s+$","",.,perl = T)
    #   director <- str_match(gsub("\n[|]\n|[|]\n",",",fullCredits),"Directed by(.*?)\n")
    #   director <- director[,2]
    #   musicdirector <- str_match(gsub("\n[|]\n|[|]\n",",",fullCredits),"Music by(.*?)\n")
    #   musicdirector <- musicdirector[,2]
    # }
    ErrorLog <- as.character(NA)
    list(t(unlist(TitlePage %>%  html_nodes("#titleCast .itemprop span") %>%  html_text()))) -> imdb_cast
    titleCast <- gsub("[|]",",",paste(unlist(imdb_cast), collapse='|'))
    
    as.numeric(gsub(",", "", TitlePage %>% html_nodes(".imdbRating .small") %>% html_text)) -> NumOfVotes
    
    as.numeric(TitlePage %>% html_nodes(".imdbRating strong") %>% html_text) -> imdb_rating
    
    
    
    titleStoryLine <- TitlePage %>% html_nodes(xpath = "//*[@id=\"titleStoryLine\"]") %>% html_text() %>% gsub("(?<=[\\s])\\s*|^\\s+|\\s+$","",.,perl = T)
    genres <- str_match(gsub("\n[|]\n|[|]\n",",",titleStoryLine),"Genres:\n(.*?)\n")
    genres <- genres[,2]
    certificate <- str_match(gsub("\n[|]\n|[|]\n",",",titleStoryLine),"Certificate:\n(.*?)\n")
    imdbcertificate <- certificate[,2]
    
    
    titleDetails <- TitlePage %>% html_nodes(xpath = "//*[@id=\"titleDetails\"]") %>% html_text() %>% gsub("(?<=[\\s])\\s*|^\\s+|\\s+$","",.,perl = T)
    country <- str_match(gsub("\n[|]\n|[|]\n",",",titleDetails),"Country:\n(.*?)\n")
    country <- country[,2]
    Language <- str_match(gsub("\n[|]\n|[|]\n",",",titleDetails),"Language:\n(.*?)\n")
    Language <- Language[,2]
    ReleaseDate <- str_match(gsub("\n[|]\n|[|]\n",",",titleDetails),"Release Date:(.*?)\n")
    ReleaseDate <- gsub("[(](.*?)[)]","",ReleaseDate[,2])
    Budget <- str_match(gsub("\n[|]\n|[|]\n",",",titleDetails),"Budget:(.*?)\n")
    Budget <- Budget[,2]
    
    IMDBMovieData <- data.frame(TitleID = as.character(titleID),
                                Title = as.character(title),
                                IMDBRating = as.character(ifelse(length(imdb_rating) == 0,NA,imdb_rating)),
                                IMDBVotes = as.character(ifelse(length(NumOfVotes) == 0,NA,NumOfVotes)),
                                # IMDBDirectorName = as.character(director),
                                # IMDBMusicDirectorName = as.character(musicdirector),
                                # IMDBStarCast = as.character(titleCast),
                                IMDBGenres = as.character(genres),
                                IMDBCertificate = as.character(imdbcertificate),
                                IMDBCountry = as.character(country),
                                IMDBPrimaryLanguage = as.character(Language),
                                IMDBReleaseDate = as.character(ReleaseDate),
                                IMDBBudget = as.character(Budget),
                                IMDBErrorLog = ErrorLog)
  }
  
  
  # TMDb data
  # Use IMDBid to fetch tmdb data
  api_key <- "cfaed36fe780796d90b0284105d81fd4"
  
  tmdbresult <- find_tmdb(api_key,id = titleID,"imdb_id")
  
  tmdbid <- tmdbresult$movie_results$id
  titleData <- TMDb::movie(api_key,tmdbid)
  
  TMDbMovieData <- data.frame(TitleID = as.character(titleData$imdb_id),
                         TMDBID = as.character(ifelse(is.null(titleData$id),"NA",titleData$id)),
                         TMDbBudget = as.character(ifelse(is.null(titleData$budget),"NA",titleData$budget)),
                         TMDbLanguage = as.character(ifelse(is.null(titleData$original_language),"NA",titleData$original_language)),
                         TMDbRevenue = as.character(ifelse(is.null(titleData$revenue),"NA",titleData$revenue)),
                         TMDbVote = as.character(ifelse(is.null(titleData$vote_average),"NA",titleData$vote_average)),
                         TMDbVoteCount = as.character(ifelse(is.null(titleData$vote_count),"NA",titleData$vote_count)),
                         TMDbRunTime = as.character(ifelse(is.null(titleData$runtime),"NA",titleData$runtime)))
  
  
  movieData <- cbind(IMDBMovieData,TMDbMovieData)
  
  # Pull cast and crew data
  if((movieData$TMDbLanguage == "hi" & universe == "Bollywood") | (movieData$TMDbLanguage == "en" & universe == "Hollywood")){
    movieCredits <- movie_credits(api_key,tmdbid)
    
    
    Cast <- movieCredits$cast
    Cast$TitleID <- titleID
    Crew <- movieCredits$crew
    Crew$TitleID <- titleID
    Crew <- Crew[which(Crew$department %in% c("Directing","Production","Sound") & 
                         Crew$job %in% c("Director","Producer","Music","Original Music Composer")),]
    
  }else{
    Cast <- data.frame()
    Crew <- data.frame()
  }
  
    
  return(list(MovieData = movieData, CastData = Cast, CrewData = Crew))
}



# Define the universe and start pulling data iteratively

allMovieData <- data.frame()
allCastdata <- data.frame()
allCrewData <- data.frame()
for(i in 7701:nrow(CombinedUniverse)){
  print(i)
  
  titleid <- CombinedUniverse$titleId[i]
  title <- CombinedUniverse$title[i]
  universe <- CombinedUniverse$Industry[i]

  
  moviedatalist <- try(parseData(titleid,title,universe),silent = T)
  
  if(class(moviedatalist) == 'try-error'){
    errordf <- data.frame(ID = titleid
                          , error = as.character(attributes(moviedatalist)$condition))
  }else{
    MovieData <- as.data.frame(moviedatalist$MovieData)
    Castdata <- as.data.frame(moviedatalist$CastData)
    CrewData <- as.data.frame(moviedatalist$CrewData)
    
    
    allMovieData <- rbind.fill(allMovieData,MovieData)
    allCastdata <- rbind.fill(allCastdata,Castdata)
    allCrewData <- rbind.fill(allCrewData,CrewData)
    
    
    allMovieData <- data.frame(allMovieData[!duplicated(allMovieData),])
    allCastdata <- data.frame(allCastdata[!duplicated(allCastdata),])
    allCrewData <- data.frame(allCrewData[!duplicated(allCrewData),])
    
  }
  
}

write.xlsx(allMovieData,"allMovieData_hollywood3.xlsx")
write.xlsx(allCastdata,"allCastdata_hollywood3.xlsx")
write.xlsx(allCrewData,"allCrewData_hollywood3.xlsx")