# Simple analysis on twitter data
# by Nur Andi
# 1 Nov 2014

# Required packages : qdap, plyr, base
# Please check required package are installed by typing command : installed.packages()
# If not already installed, try to install by typing commands (need internet conection):
#
# install.packages("devtools")  
# library(devtools)
# install_github("qdapDictionaries", "trinker") 
# install_github("qdap", "trinker") 
# install.packages("plyr")


# Load package
library(qdap)
library(plyr)

options(scipen=999)

# Import data from csv file
tweets = read.csv(file="C:\\Users\\NANDI\\Desktop\\tweets.csv")

# Extract twitter client that uses when sending tweet (e.g TweetDeck, Twitter Android, etc.)
client = gsub('#.*', NA, 
            gsub('.*>(.+)<.*', '\\1', tweets$source)
            )

# Extract hashtags from tweet
hashtags = gsub('character.0.|c[^a-zA-Z0-9]|"|)', '',
                as.factor(paste(regmatches(tweets$text, gregexpr("#(\\d|\\w)+", tweets$text)), sep=",")
                ))
hashtags[hashtags == ""] = NA


# Extract mentions from tweet
mentions = gsub('character.0.|c[^a-zA-Z0-9]|"|)', '',
                as.factor(paste(regmatches(tweets$text, gregexpr("@(\\d|\\w)+", tweets$text)), sep=",")
                ))
mentions[mentions == ""] = NA

# Combine variable into new table
tweets_clean = cbind(tweets[c("tweet_id","timestamp","source","text")],
                  hashtags,mentions,client
                  )

# Check table structure
str(tweets_clean)

# Formating timestamp as timestamp, since timestamp is written as string
tweets_clean$timestamp = as.POSIXct(tweets_clean$timestamp, format="%Y-%m-%d %H:%M:%S")

# Count hashtag
count_hashtag = with(tweets_clean, word_list(hashtags, char.keep="#", cut.n=100))
count_hashtag = as.data.frame(count_hashtag$fwl)
names(count_hashtag) = c("hashtag","freq")

# Count mention
count_mention = with(tweets_clean, word_list(mentions, char.keep="@", cut.n=100))
count_mention = as.data.frame(count_mention$fwl)
names(count_mention) = c("mention","freq")

# Count word (including hashtag and mention)
count_word_all = with(tweets_clean, word_list(text, char.keep=c("#",'@')))
count_word_all = as.data.frame(count_word_all$fwl)
names(count_word_all) = c("word","freq")

# Count word (excluding hashtag and mention)
count_word_exc = with(tweets_clean, word_list(gsub('(@|#)[a-zA-Z0-9]+', '', text), cut.n=300))
count_word_exc = as.data.frame(count_word_exc$fwl)
names(count_word_exc) = c("word","freq")

# Import count data to csv.
# Run if necessary
# write.csv(tweets_clean, file="C:\\Users\\NANDI\\Desktop\\tweets_clean.csv")
# write.csv(count_hashtag, file="C:\\Users\\NANDI\\Desktop\\count_hashtag.csv")
# write.csv(count_mention, file="C:\\Users\\NANDI\\Desktop\\count_mention.csv")
# write.csv(count_word_exc, file="C:\\Users\\NANDI\\Desktop\\count_word_exc.csv")
# write.csv(count_word_all, file="C:\\Users\\NANDI\\Desktop\\count_word_all.csv")


# Visualization ###########################
###########################################


# Char of Client
plot_client = qplot(factor(client), data=tweets_clean, geom="bar", fill=factor(client))
plot_client = plot_client + theme(axis.text.x = element_text(angle=90, hjust=1), plot.title = element_text(size = 16, vjust=2)) + labs(title = "Count of Tweets by Client", x = "Client", y = "Count")

# Chart of Top 10 Hastags
c_hashtag = ddply(count_hashtag, "hashtag", summarise, freq = mean(freq))
c_hashtag = c_hashtag [order(-c_hashtag$freq),] 
plot_hashtag = ggplot(head(c_hashtag,10), aes(x=hashtag, y=freq, fill=hashtag)) + geom_bar(stat = "identity")
plot_hashtag = plot_hashtag + theme(axis.text.x = element_text(angle=90, hjust=1), plot.title = element_text(size = 16, vjust=2)) + labs(title = "Top 10 Hashtags", x = "Hashtag", y = "Frequency")

# Chart of Top 10 Mentions
c_mention = ddply(count_mention, "mention", summarise, freq = mean(freq))
c_mention = c_mention [order(-c_mention$freq),] 
plot_mention = ggplot(head(c_mention,10), aes(x=mention, y=freq, fill=mention)) + geom_bar(stat = "identity")
plot_mention = plot_mention + theme(axis.text.x = element_text(angle=90, hjust=1), plot.title = element_text(size = 16, vjust=2)) + labs(title = "Top 10 Mentions", x = "Mention", y = "Frequency")

# Chart of Top 10 Words
c_word = ddply(count_word_exc, "word", summarise, freq = mean(freq))
c_word = c_word [order(-c_word$freq),] 
plot_word = ggplot(head(c_word,10), aes(x=word, y=freq, fill=word)) + geom_bar(stat = "identity")
plot_word = plot_word + theme(axis.text.x = element_text(angle=90, hjust=1), plot.title = element_text(size = 16, vjust=2)) + labs(title = "Top 10 Words (Exc. Hashtag & Mention)", x = "Word", y = "Frequency")

# Histogram of #Words per Tweet
word_per_tweet = as.data.frame(word_count(tweets_clean$text))
names(word_per_tweet) = "count"
hist_word = ggplot(word_per_tweet, aes(x=count))
hist_word = hist_word + geom_histogram(aes(fill = ..count..),binwidth = 1) + theme(plot.title = element_text(size = 16, vjust=2)) + labs(title = "Number of Words per Tweets", x = "Number of Words", y = "Number of Tweets")

# Word cloud by time
time = format(tweets_clean$timestamp,"%H")
time[time > "06" & time <= "18"] <- "Day"
time[time != "Day"] <- "Night"
word_cloud = gradient_cloud(tweets_clean$text, time, title="Word on Tweets by Time", max.word.size = 5, min.word.size = .05)


# View plot/chart.
plot_client
plot_hashtag
plot_mention
plot_word
hist_word

#### end of file ####
