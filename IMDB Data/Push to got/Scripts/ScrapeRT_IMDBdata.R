library(rvest)

# scraping IMDB data

parseIMDB_RTData <- function(titleID,title){

  
  # Rotten tomatoes data
  rottenTomatoes <- try(read_html(paste0("https://www.rottentomatoes.com/m/",gsub(" ","_",title))),silent = T)
  if(class(rottenTomatoes) == 'try-error'){
    RTMovieData <- data.frame(TitleID = titleID,
                              Title = title,
                              RTErrorLog = as.character(paste0("Rotten Tomato Error:",attributes(rottenTomatoes)$condition,collapse = "")))
  }else{
  ErrorLog <- as.character(NA)
  rtContent <- rottenTomatoes %>% html_nodes(".meta-row") %>% html_text() %>% gsub("(?<=[\\s])\\s*|^\\s+|\\s+$","",.,perl = T) %>% paste0(.,collapse = "\n")
  rtscorePanel <- rottenTomatoes %>% html_nodes("#scorePanel") %>% html_text() %>% gsub("(?<=[\\s])\\s*|^\\s+|\\s+$","",.,perl = T)
  
  rtcertificate <- str_match(gsub("\n[|]\n|[|]\n",",",rtContent),"Rating:(.*?)\n")
  rtcertificate <- as.character(rtcertificate[,2])
  rtGenre <- str_match(gsub("\n[|]\n|[|]\n",",",rtContent),"Genre:(.*?)\n")
  rtGenre <- as.character(gsub("&",",",rtGenre[,2]))
  rtDirector <- str_match(gsub("\n[|]\n|[|]\n",",",rtContent),"Directed By:(.*?)\n")
  rtDirector <- as.character(rtDirector[,2])
  rtReleaseDate <- str_match(gsub("\n[|]\n|[|]\n",",",rtContent),"In Theaters:(.*?)\n")
  rtReleaseDate <- as.character(rtReleaseDate[,2])
  rtBoxOfficeEarning <- str_match(gsub("\n[|]\n|[|]\n",",",rtContent),"Box Office:(.*?)\n")
  rtBoxOfficeEarning <- as.character(rtBoxOfficeEarning[,2])
  rtTomatometer <- str_match(gsub("\n[|]\n|[|]\n",",",rtscorePanel),"\nTomatometer(.*?)\n")
  rtTomatometer <- as.character(rtTomatometer[,2])
  rtCriticRating <- str_match(gsub("\n[|]\n|[|]\n",",",rtscorePanel),"\nAverage Rating:(.*?)\n")
  rtCriticRating <- as.character(rtCriticRating[,2])
  rtCriticVotes <- str_match(gsub("\n[|]\n|[|]\n",",",rtscorePanel),"\nReviews Counted:(.*?)\n")
  rtCriticVotes <- as.character(rtCriticVotes[,2])
  
  RTMovieData <- data.frame(TitleID = as.character(titleID),
                            Title = as.character(title),
                            Tomatometer = as.character(rtTomatometer),
                            RTCriticRating = as.character(rtCriticRating),
                            RTCriticVotes = as.character(rtCriticVotes),
                            RTDirectorName = as.character(rtDirector),
                            # RTMusicDirectorName = as.character(musicdirector),
                            RTGenre = as.character(rtGenre),
                            RTcertificate = as.character(rtcertificate),
                            # RTCountry = as.character(country),
                            # RTPrimaryLanguage = as.character(Language),
                            RTReleaseDate = as.character(rtReleaseDate),
                            RTEarnings = as.character(rtBoxOfficeEarning),
                            RTErrorLog = ErrorLog)
  
  }
  
  # IMDB data
  TitlePage <- try(read_html(paste0("http://www.imdb.com/title/",titleID,"/")),silent = T)
  if(class(TitlePage) == 'try-error'){
    IMDBMovieData <- data.frame(TitleID = titleID,
                              Title = title,
                              IMDBErrorLog = as.character(paste0("IMDB TitlePage Error:",attributes(TitlePage)$condition,collapse = "")))
  }else{
  
  CreditsPage <- try(read_html(paste0("http://www.imdb.com/title/",titleID,"/fullcredits?")),silent = T)
  if(class(CreditsPage) == 'try-error'){
    director <- as.character(NA)
    musicdirector <- as.character(NA)
    IMDBErrorLog <- as.character(paste0("IMDB CreditsPage Error:",attributes(CreditsPage)$condition,collapse = ""))
  }else{
    ErrorLog <- as.character(NA)
    fullCredits <- CreditsPage %>% html_nodes(xpath = "//*[@id=\"fullcredits_content\"]") %>% html_text() %>% gsub("(?<=[\\s])\\s*|^\\s+|\\s+$","",.,perl = T)
    director <- str_match(gsub("\n[|]\n|[|]\n",",",fullCredits),"Directed by(.*?)\n")
    director <- director[,2]
    musicdirector <- str_match(gsub("\n[|]\n|[|]\n",",",fullCredits),"Music by(.*?)\n")
    musicdirector <- musicdirector[,2]
  }
  
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
                          IMDBDirectorName = as.character(director),
                          IMDBMusicDirectorName = as.character(musicdirector),
                          IMDBStarCast = as.character(titleCast),
                          IMDBGenres = as.character(genres),
                          IMDBCertificate = as.character(imdbcertificate),
                          IMDBCountry = as.character(country),
                          IMDBPrimaryLanguage = as.character(Language),
                          IMDBReleaseDate = as.character(ReleaseDate),
                          IMDBBudget = as.character(Budget),
                          IMDBErrorLog = ErrorLog)
  }
  
  movieData <- merge(IMDBMovieData,RTMovieData, by = c("TitleID","Title"))
  colnames(movieData) <- gsub("[.]x","IMDB ",colnames(movieData))
  colnames(movieData) <- gsub("[.]y","Rotten Tomatoes ",colnames(movieData))
  return(movieData)
  }


