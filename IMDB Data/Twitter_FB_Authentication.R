# Facebook data

# library(Rfacebook)
# 
# fb_oauth <- fbOAuth(app_id = 175600919977610,
#                     app_secret = "17052cd78ba1bcd5433eebf68c0d3ec6")
# 
# 
# save(fb_oauth,file = "MyFB_Oauth")

# Twitter data
# Set constant requestURL
requestURL <- "https://api.twitter.com/oauth/request_token"
# Set constant accessURL
accessURL <- "https://api.twitter.com/oauth/access_token"
# Set constant authURL
authURL <- "https://api.twitter.com/oauth/authorize"


twitterAuth <- function(){
options(httr_oauth_cache=T)
api_key <- "oNmAiotVrpLihuzNB5hM0iC3e"
api_secret <- "Yxcu1OS5pzmvZKza4Pf4ZSjBD5YMKne0ekfFXxrkBCGC5p2KuG"
access_token <- "564745848-pbdYjcVUyj3GZjnlvizoLwA6jMHCLkFwDzH57I5t"
access_token_secret <- "cH2sWzO9jug5bO2JZEs4Fd7gTd5g9EbP0LQQWVsagmsIF"
t_oauth <- setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)
return(t_oauth)
}
# save(t_oauth,file = "MyTwitter_Oauth")