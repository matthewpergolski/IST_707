



library(arules)
library(rtweet)
library(twitteR)
library(ROAuth)
library (jsonlite)
library(streamR)
library(rjson)
library(tokenizers)
library(tidyverse)
library(plyr)
library(dplyr)
library(ggplot2)
#install.packages ("syuzhet")
## sentiment analysis
library(syuzhet)
library(stringr)
library(arulesViz) ## load last


############## Using twittR##############################################
#setup_twitter_oauth(consumerKey, consumerSecret, access_Token, access_Secret)


setup_twitter_oauth(consumerKey, consumerSecret, access_Token, access_Secret)
#setup_twitter_oauth(consumer_key = API.Key, consumer_secret = API.Key.Secret, access_token = Access.Token, access_secret = Access.Token.Secret)
Search<-twitteR::searchTwitter ("#Packers",n=100, since="2022-01-22")
(Search_DF <- twListToDF(Search))
TransactionTweetsFile = "PackersPlayoffAfterMath2022.csv"

## Start the file
Trans <- file (TransactionTweetsFile)
## Tokenize to words
# Tokens <- tokenizers::tokenize_words(Search_DF$text[1], 
#                                    stopwords = stopwords::stopwords("en"),
#                                       lowercase = TRUE, strip_punct = TRUE, 
#                                    strip_numeric = TRUE, 
#                                    simplify = TRUE)

Tokens <- tokenize_words(Search_DF$text[1], stopwords = "en", lowercase = TRUE, 
                         strip_punct = TRUE, 
                         strip_numeric = TRUE,
                         simplify = TRUE)

## Write squished tokens
cat(unlist(str_squish(Tokens)), "\n", file=Trans, sep=",")
close (Trans)

## Append remaining lists of tokens into file
## Recall - a list of tokens is the set of words from a Tweet
Trans <- file(TransactionTweetsFile, open = "a")
for(i in 2:nrow(Search_DF)){
  Tokens <- tokenize_words(Search_DF$text[i],stopwords = ("en"),
                          lowercase = TRUE, strip_punct = TRUE, simplify = TRUE)
  
  cat(unlist(str_squish (Tokens)), "\n", file=Trans, sep=",")
}
  close (Trans)

# # API Key ®
#     API.Key <- 'FAWe5RKcwu234FCbq2vHWO6os'
#     API.Key
# 
# # API Key Secret ®
#     API.Key.Secret <- 'XchJLXra3artvFTGRHxaJjmZHdIYykyBgbHrvv0R8mlspoYoha'
#     API.Key.Secret
#  
# # Bearer Token ®
#     Bearer.Token <- 'AAAAAAAAAAAAAAAAAAAAACXvYgEAAAAAoWzRmqUWQz%2BKZsLK8Y7iIpz8fKk%3DJWw41sK419SVPrMsnc73ND1IULXviCFDjykrvWWLKz3bmbtAl8'
#     Bearer.Token
# 
# # Access Token
#     Access.Token <- '1485648627607429123-83Hicrjd6fJRHhT702OwyRohpD3qEw'
#     Access.Token
# 
# # Access Token Secret
#     Access.Token.Secret <- 'Cm7br0W9Ka3p9AR1Ir7igybq7vcIlJSACCO4zaRE1zsoC'
#     Access.Token.Secret




