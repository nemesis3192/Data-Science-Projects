install.packages("rvest")
library(rvest)
# # 2016 Movies
# url16=read_html("http://www.imdb.com/search/title?title_type=feature&release_date=2016-01-01,2016-12-31&countries=in&languages=hi&adult=include&sort=release_date,asc&count=250")
# data_list16 =url16
# 
# data_list16 %>%
#   html_nodes(".lister-item-header")%>% html_nodes("a")%>%html_text()
# data_list16
# md16=html_text(html_nodes(data_list16, ".lister-item-header a"))
# md16
# 
# # 2015 Movies
# url15=read_html("http://www.imdb.com/search/title?title_type=feature&release_date=2015-01-01,2015-12-31&countries=in&languages=hi&adult=include&sort=release_date,asc&count=250")
# data_list15 =url15
# 
# data_list15 %>%
#   html_nodes(".lister-item-header")%>% html_nodes("a")%>%html_text()
# data_list15
# md15=html_text(html_nodes(data_list15, ".lister-item-header a"))
# md15
# 
# # 2014 Movies
# url14=read_html("http://www.imdb.com/search/title?title_type=feature&release_date=2014-01-01,2014-12-31&countries=in&languages=hi&adult=include&sort=release_date,asc&count=250")
# data_list14 =url14
# md14=html_text(html_nodes(data_list14, ".lister-item-header a"))
# md14
# 
# # 2013 Movies
# url13=read_html("http://www.imdb.com/search/title?title_type=feature&release_date=2013-01-01,2013-12-31&countries=in&languages=hi&adult=include&sort=release_date,asc&count=250")
# mdata= xmlToDataFrame(url13)
# data_list13 =url13
# md13=html_text(html_nodes(data_list13, ".lister-item-header a"))
# md13
# 
# # Convert and Save to XLSX 
# library(xlsx)
# library(openxlsx)
# library(XML)
# library(methods)
# xmltoFrame = xmlToDataFrame("md")
# xmldt=md
# getwd()
# setwd("C:/Users/Rucha Dighe/Documents/IIT Kanpur/Capstone/Credit risk")
# write.xlsx(xmldt, "data.xlsx")
# 


# Variables for each movie
#movie_data= data.frame("Title"=title, "Release date"= rel_date, "Genre"= genre.s, "Certificate"= cert, "Director"=director, stringsAsFactors = FALSE)
# movie_cast_url="http://www.imdb.com/title/tt2777548/fullcredits?ref_=tt_ov_st_sm"
# cast=read_html(movie_cast_url)
#movie_data<-data.frame(Title= character(), RelDate= character(),Genre= character(),Certificate= character(),Director=character(), stringsAsFactors = FALSE)

  movie_url="http://www.imdb.com/title/tt5209420/?ref_=adv_li_tt"
  movie=read_html(movie_url)
  
  title=html_text(html_node(movie,"div h1"))
  title= as.character(title)
  
  rel_date=sub("\\(.*","",sub(".*: ", "", html_text(html_nodes(movie, "#titleDetails .txt-block"))[3]))
  rel_date=as.character(rel_date)
  rel= html_text(html_nodes(movie, ".title_wrapper [title='See more release dates']"))
  
  genre=html_text(html_nodes(movie, "#titleStoryLine [itemprop=genre] a"))
        genre.s <- paste(genre, collapse=", ") 
        genre.s=as.character(genre.s)
  
  cert =html_text(html_nodes(movie, "#titleStoryLine [itemprop=contentRating]"))
  cert=as.character(cert)
  if (identical(cert,character(0))) 
    {cert="NA"} else
    {cert}
  
  director =html_text(html_nodes(movie, ".credit_summary_item [itemprop=director] span"))
  director.s=paste(director, collapse = ", ")
  director=as.character(director.s)
  
  movie_data2= data.frame("Title"=title, "Release date"= rel_date, "Genre"= genre.s, "Certificate"= cert,"Director"=director, stringsAsFactors = FALSE)
  movie_data=rbind(movie_data,movie_data2)
  
  mdata=movie_data
  library(xlsxjars)
  
  getwd()
  setwd("C:/Users/Rucha Dighe/Documents/IIT Kanpur/Capstone/Movie Data")
  
  write.csv(movie_data, file='data2013.csv')
