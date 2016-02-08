# Twitter Authentication using ROAuth & httr Package
# https://nurandi.net
# https://nurandi.net/socmed/twitter-authentication-dengan-roauth-dan-httr/

twitter_OAuth <- function(consumerKey, consumerSecret, accessToken = NULL, accessTokenSecret = NULL){
  
  require(ROAuth)
  
  requestURL <- "https://api.twitter.com/oauth/request_token"
  authURL <- "https://api.twitter.com/oauth/authorize"
  accessURL <- "https://api.twitter.com/oauth/access_token"
  
  if ( is.null(accessToken) || is.null(accessTokenSecret) ) {
    
    my_oauth <- OAuthFactory$new(consumerKey = consumerKey,
                                 consumerSecret = consumerSecret,
                                 requestURL = requestURL,
                                 accessURL = accessURL,
                                 authURL = authURL)
    my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
    
  } else {
    
    my_oauth <- OAuthFactory$new(consumerKey = consumerKey,
                                 consumerSecret = consumerSecret,
                                 oauthKey = accessToken,
                                 oauthSecret = accessTokenSecret,
                                 needsVerifier = FALSE,
                                 handshakeComplete = TRUE,
                                 verifier = "1",
                                 requestURL = requestURL,
                                 accessURL = accessURL,
                                 authURL = authURL,
                                 signMethod = "HMAC")
  }
  
  return(my_oauth)

}


twitter_token <- function(oauth) {

  require(httr)
  
  my_oauth <- oauth
  app <- oauth_app("twitter", key = my_oauth$consumerKey, secret = my_oauth$consumerSecret)
  creds <- list(oauth_token = my_oauth$oauthKey, oauth_token_secret = my_oauth$oauthSecret)
  token <- Token1.0$new(endpoint = NULL, params = list(as_header = TRUE), 
                        app = app, credentials = creds)
  
  return(token)
  
}



search_twitter <- function(query, token) {

  require(httr)
  baseurl <- "https://api.twitter.com/1.1/search/tweets.json?q="
  query <- URLencode(query)
  url <- paste0(baseurl, query)
  
  response <- GET(url, config(token = token))
  content <- content(response)[[1]]
  text <- unlist(sapply(content,function(x) x$text))
  
  return(text)
}


#### 

my_oauth <- twitter_OAuth(
  consumerKey = "XXX", 
  consumerSecret = "XXX", 
  accessToken = "XXXX", 
  accessTokenSecret = "XXX"
)

token <- twitter_token(oauth = my_oauth)

angkot_bogor <- search_twitter(query = "angkot bogor", token = token)

# > angkot_bogor
#  [1] "RT @NyunyuCom: Kalo di Bogor, jangan sampe salah milih angkot! Biar kalo mau kemana-mana cepet, pilih angkot yg kayak gini gaes! https://t.…"
#  [2] "Kalo di Bogor, jangan sampe salah milih angkot! Biar kalo mau kemana-mana cepet, pilih angkot yg kayak gini gaes! https://t.co/hlA0tsDTSp"   
#  [3] "Kota hujan. Kota sejuta angkot. \xf0\u009f\u009a\u0098\u26c5\u2614\xf0\u009f\u008c\u009e @ Patung Kujang Bogor https://t.co/xZiOvisWvv"      
#  [4] "BOCORE - Angkot Bogor - Rp 70.000,- (S/M/L/XL/XXL) SMS/Telp : 081574232423 https://t.co/VRykHK1EkK https://t.co/GmoXJ9n4Zt #kaos #bogor"     
#  [5] "Program One Way di Kota Bogor Terbentur Rute Angkot - Pojok Jabar https://t.co/UGxAD9PaUU #Bogor"                                            
#  [6] "Move movee.....to second place we go....ujan2an di Bogor.....ihiiiyyyy...… (at Angkot 02 Cisarua Sukasari) — https://t.co/CANS9sTLlq"        
#  [7] "RT @BogorSelatan_: Program One Way di Kota Bogor Terbentur Rute Angkot - Pojok Jabar https://t.co/Ac5J52xA7I #trafjak"                       
#  [8] "pasar ciawi tetep pasar tumpah dan tempat ngetem angkot depan pasar macet hallloooo wakikota Bogor ,,, mana bukti nya ????"                  
#  [9] "Program One Way di Kota Bogor Terbentur Rute Angkot - Pojok Jabar https://t.co/mBMZXt5jXA #Bogor #Jabar"                                     
# [10] "Program One Way di Kota Bogor Terbentur Rute Angkot - Pojok Jabar https://t.co/H5M8UuVyJ9 #Bogor"                                            
# [11] "Program One Way di Kota Bogor Terbentur Rute Angkot - Pojok Jabar https://t.co/ffVAu8IuKl #BeritaBogor"                                      
# [12] "#BogorApaKabar Program One Way di Kota Bogor Terbentur Rute Angkot https://t.co/xfi9tjN8ag"                                                  
# [13] "@lindaulin dr stasiun Bogor tinggal naik angkot 10 turunnya di depan outlet Lapis Bogor Sangkuriang Pajajaran (JL.Pajajaran komplek ruko"    
# [14] "Program One Way di Kota Bogor Terbentur Rute Angkot - Pojok Jabar https://t.co/plR5z4bga8"                                                   
# [15] "Program One Way di Kota Bogor Terbentur Rute Angkot https://t.co/xiL7M5srhy"   

# Credit:
# https://github.com/hadley/httr
# https://github.com/geoffjentry/ROAuth
# https://github.com/SMAPPNYU/smappR
# https://github.com/pablobarbera/streamR
