
# Twitter Authentication dengan ROAuth dan httr
# https://nurandi.net/socmed/twitter-authentication-dengan-roauth-dan-httr/
# Update: 13 Feb 2016


# Alt #1, when all credentials are available


consumerKey <- "XXX"
consumerSecret <- "XXX"
accessToken <- "XXX"
accessTokenSecret <- "XXX"

library(httr)

app <- oauth_app("twitter",
                 key = consumerKey,
                 secret = consumerSecret)

twitter_token <- Token1.0$new(endpoint = NULL, 
                              params = list(as_header = TRUE), 
                              app = app, 
                              credentials = list(oauth_token = accessToken,
                                                 oauth_token_secret = accessTokenSecret))


# Alt #2, when access token is not avalable
# and call back URL is empty


# Handshaking : Generate access token 
library(ROAuth)

consumerKey <- "XXX"
consumerSecret <- "XXX"

requestURL <- "https://api.twitter.com/oauth/request_token"
authURL <- "https://api.twitter.com/oauth/authorize"
accessURL <- "https://api.twitter.com/oauth/access_token"

creds <- OAuthFactory$new(consumerKey = consumerKey,
                          consumerSecret = consumerSecret,
                          requestURL = requestURL,
                          accessURL = accessURL,
                          authURL = authURL)
creds$handshake()

# Or:
# my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))

accessToken <- creds$oauthKey
accessTokenSecret <- creds$oauthSecret

twitter_token <- Token1.0$new(endpoint = NULL, 
                              params = list(as_header = TRUE), 
                              app = app, 
                              credentials = list(oauth_token = accessToken,
                                                 oauth_token_secret = accessTokenSecret))



# Alt #3 : require only customer key and secret
# set callback URL to http://127.0.0.1:1410

library(httr)
app <- oauth_app("twitter",
                 key = consumerKey,
                 secret = consumerSecret)

twitter_token <- oauth1.0_token(oauth_endpoints("twitter"), app = app)



# API request, example

search_twitter <- function(keyword, token) {
  
  require(httr)
  baseurl <- "https://api.twitter.com/1.1/search/tweets.json?q="
  url <- paste0(baseurl, URLencode(keyword))
  
  response <- GET(url, config(token = token))
  content <- content(response)[[1]]
  text <- unlist(sapply(content,function(x) x$text))
  
  return(text)
}

angkot_bogor <- search_twitter("angkot bogor", token = twitter_token)
angkot_bogor



# Integrate with twitteR
# alternatif to twitteR::setup_twitter_oauth()


assign("oauth_token", twitter_token)

library(twitteR)
tweets <- searchTwitter("indonesia raya")
sapply(head(tweets), statusText)

