library(plyr)
library(ggplot2)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(twitteR)

combine<-read.csv("H:/smartcity/bhopal/bhopal.csv")

yay <- scan('H:/smartcity/bhopal/positive-words.txt',what='character', comment.char=';')
boo <- scan('H:/smartcity/bhopal/negative-words.txt',what='character', comment.char=';')

bad_text <- c(boo, 'wtf', 'wait', 'waiting','epicfail', 'slow','road','transport','cleanliness')
good_text <- c(yay, 'upgrade', ':)', '#iVoted', 'voted')


score.sentiment <- function(sentences, good_text, bad_text, .progress='none')
  
{
  require(plyr)
  
  require(stringr)
  scores = laply(sentences, function(sentence, good_text, bad_text)
  {
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence=gsub("(RT|via)((?:\\b\\W*@\\w+)+)","",sentence)
    sentence =gsub("@\\w+",'',sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub("[[:digit:]]",'',sentence)
    sentence = gsub('\\d+', '', sentence)
    sentence = gsub("http\\w+",'',sentence)
    sentence <- iconv(sentence, 'UTF-8', 'ASCII')
    sentence = tolower(sentence)
    
    word.list = str_split(sentence, '\\s+')
    words = unlist(word.list)
    
    pos.matches = match(words, good_text)
    neg.matches = match(words, bad_text)
    
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
    
  }, good_text, bad_text, .progress=.progress )
  
  
  
  scores.df = data.frame(score=scores, text=sentences)
  
  return(scores.df)
  
}

combine$text<-as.factor(combine$text)
combine.scores=score.sentiment(combine$text,good_text,bad_text, .progress = 'text')

summary(combine.scores)
sentimentscore<-combine.scores$score
hist(sentimentscore,main="bhopal Smartcity",col = rainbow(12))









