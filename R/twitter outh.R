pkgs <-c('twitteR','ROAuth','httr','plyr','stringr','ggplot2','plotly')

for(p in pkgs) if(p %in% rownames(installed.packages()) == FALSE) {install.packages(p)}

for(p in pkgs) suppressPackageStartupMessages(library(p, quietly=TRUE, character.only=TRUE))

api_key <- "ZJErjheQDLCoIDtFAk8foyoQs"

api_secret <- "wBIfYq8PQ6tLMitV83Dm7pGGJtnbuXdfsHwO6mQSoY94AGqlNW"

access_token <- "788639088043814912-3ifNFQ5ajYftQAj9P7fvv1QEKT2OclS"

access_token_secret <- "4lFnt9BNyqrv6P6ZN7gXKVqmXQkwqzxsgUoTv3Q8vKDOi"

setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)

tweets_smartcity <- searchTwitter('@smartcity', n=3000)

tweets_india <- searchTwitter('#smartcityindia', n=3000)

feed_smartcity <- laply(tweets_smartcity, function(t) t$getText())

feed_india <- laply(tweets_india, function(t) t$getText())

tweets_smartcityDF <- twListToDF(tweets_smartcity)

write.csv(tweets_smartcityDF, "H:/smartcity/final project/tweets_smartcity.csv")

tweets_indiaDF<-twListToDF(tweets_india)

write.csv(tweets_indiaDF,"H:/smartcity/final project/tweets_smartcity.csv")
