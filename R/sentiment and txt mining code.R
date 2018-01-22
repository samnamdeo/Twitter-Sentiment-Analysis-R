library(plyr)
library(ggplot2)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(twitteR)


combine<-read.csv("H:/smartcity/final project/india.csv")

feed_combine <- laply(combine, function(t) t$getText())

yay <- scan("H:/smartcity/final project/positive-words.txt",what='character', comment.char=';')
boo <- scan("H:/smartcity/final project/negative-words.txt",what='character', comment.char=';')

bad_text <- c(boo, 'wtf', 'wait', 'waiting','epicfail', 'slow')
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

#put this in the csv file for future purpose
path<-"H:/smartcity/final project/"
write.csv(combine.scores,file = paste(path,"combine1.csv",sep = ""),row.names = TRUE)

plotdat <- combine.scores
library(ggplot2)
library(plotly)

summary(combine.scores)
sentimentscore<-combine.scores$score
hist(sentimentscore,main="histogram:india Smartcities",col = rainbow(9))

qplot(factor(score), data=plotdat, geom = "bar",xlab = "Sentiment Score",main="barplot:India Smartcities")

ep <- plotdat %>% ggplot(aes(x = score,)) +geom_histogram(binwidth = 1) +
  scale_fill_manual(values = c("#0067F7","#7B00F7", "#7CF700", "#F70000")) +
  theme_classic(base_size = 12) +scale_x_continuous(name = "Sentiment Score") +
  scale_y_continuous(name = "Text count of tweets") +ggtitle("Smartcity India")+
  theme(axis.title.y = element_text(face="bold", colour="#000000", size=10),axis.title.x = element_text(face="bold", colour="#000000", size=8),
        axis.text.x = element_text(angle=16, vjust=0, size=8))

ggplotly(ep)

#text analysis
library(tm)
library(wordcloud)

combine2=read.csv("H:/smartcity/final project/combine1.csv")

clean=Corpus(VectorSource(combine2),readerControl = list(language="eng"))

inspect(clean)

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(clean, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
docs <- tm_map(docs, toSpace,"https*")
docs <- tm_map(docs, toSpace,"tco*")
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove your own stop word
myStopwords <- c(stopwords('english'),"r", "big","uuuu","uucu","uuf","uufu","uufuf","uufufue","androida","blooddonorsin","clienta","ciudades",
                 "con","conn","cop","des","eampit","eduaubdedubu","eduaubdedububd","eduaubdedubueduaubdedubuuuuueuuudrkumarvis","eec",
                 "engb","españa","euw","evankirstel","href","hrefhttpsdoordacom","htt","http","ipada","iphonea","ipfconline","iwaterbarcelona",
                 "los", "las","mer","nov","otvnews","ppchaudharymos","ravi","relnofollowaaretweeta","relnofollowapp","relnofollowbuffera","relnofollowfacebooka",
                 "relnofollowgrabinboxa","relnofollowhootsuitea","relnofollowifttta","relnofollowmobile","relnofollowpaperlia","relnofollowscbotbackenda",
                 "relnofollowtweetdecka","relnofollowwiocitiesa","relnofollowclickkla","relnofollowfacebooka","relnofollowhootsuitea","uuuufufu","vikezmedia",
                 "relnofollowinstagrama","relnofollowtopicly","relnofollowtwitter","revolució","rfyouthsports","rsprasad","rfyouthsports","scewc","vijayshekhar",
                 "shankar","sunnybhullar","ttot","ububuuc","ucuc","ucuu","uduu","uufudueu","uufue","uuu","uufudueu","uufue","uuuu","uufuuu","uuue","wiomaxmd","ueubuu",                                    
                 "ueuu","ueuuuduueuuf","ufu","ufuu","uidai","una","uub","uubuf","uudufue","uueu","uueucuduf","yfnvillage","yfnmwc","available","via","tco","ufu","uuu")

docs <- tm_map(docs, removeWords, myStopwords)
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, removeURL)
# Text stemming
library(SnowballC)
docs <- tm_map(docs, stemDocument)

dtm <- TermDocumentMatrix(docs)
dtm

# inspect frequent words 
(freq.terms <- findFreqTerms(dtm, lowfreq = 15))

term.freq <- rowSums(as.matrix(dtm)) 
term.freq <- subset(term.freq, term.freq >= 500) 
df <- data.frame(term = names(term.freq), freq = term.freq)
library(ggplot2) 
ggplot(df, aes(x = term, y = freq)) + geom_bar(stat = "identity")+geom_point() + xlab("Terms") + ylab("Count")+ coord_flip()




m <- as.matrix(dtm)
m
v <- sort(rowSums(m),decreasing=TRUE)
v

d <- data.frame(word = names(v),freq=v)
head(d, 10)

wordcloud(words = d$word, freq = d$freq, min.freq = 5,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=rainbow(6))

wordcloud(words = d$word, freq = d$freq, min.freq = 5,
          max.words=100, random.order=TRUE, rot.per=0.35, 
          colors=brewer.pal(10, "Dark2"))
