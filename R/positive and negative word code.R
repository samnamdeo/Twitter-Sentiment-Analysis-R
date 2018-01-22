library(plyr)
library(ggplot2)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(twitteR)
library(dplyr)

combine<-read.csv("C:/users/HP/Desktop/smart city/india.csv")


positive <- scan('C:/users/HP/Desktop/New folder/positive-words.txt',what='character', comment.char=';')
negative <- scan('C:/users/HP/Desktop/New folder/negative-words.txt',what='character', comment.char=';')

negative <- c(negative, 'wait', 'waiting','epicfail', 'slow','roads','transport','cleaniness','garbage')
positive <- c(positive, 'upgrade', ':)', '#iVoted', 'voted')




tryTolower=function(x)
{
  y=NA
  #tryCatch error
  try_error=tryCatch(tolower(x),error=function(e) e)
  #if not an error
  if(!inherits(try_error,"error"))
    y=tolower(x)
  return(y)
}

clean=function(t){
  t=gsub('[[:punct:]]','',t)
  t=gsub('[[:cntrl:]]','',t)
  t=gsub('\\d+','',t)
  t=gsub('[[:digit:]]','',t)
  t=gsub('@\\w+','',t)
  t=gsub('http\\w+','',t)
  t=gsub("^\\s+|\\s+$","",t)
  t=sapply(t,function(x) tryTolower(x))
  
  t=unlist(t)
  return(t)
  
}

combineclean=lapply(combine,function(x) clean(x))

#returnpscore for counting the positive matching words
returnpscore=function(tweet) {
  pos.match=match(tweet,positive)
  pos.match=!is.na(pos.match)
  pos.score=sum(pos.match)
  return(pos.score)
}

positive.score=lapply(combineclean,function(x) returnpscore(x))

#define a loop to count the total number of positive words present in the tweets
pcount=0
for (i in 1:length(positive.score)) {
  pcount=pcount+positive.score[[i]]
}
pcount

#returnpscore for counting the positive matching words
returnNscore=function(tweet) {
  neg.match=match(tweet,negative)
  neg.match=!is.na(neg.match)
  neg.score=sum(neg.match)
  return(neg.score)
}

negative.score=lapply(combineclean,function(x) returnNscore(x))

#define a loop to count the total number of positive words present in the tweets
ncount=0
for (i in 1:length(negative.score)) {
  ncount=ncount+negative.score[[i]]
}
ncount



#retrieves the positive and negative matching words
poswords=function(tweets){
  pmatch=match(t,positive)
  posw=positive[pmatch]
  posw=posw[!is.na(posw)]
  return(posw)
}

#This function is applied to our combineclean list and a loop is called to add words to a data frame, pdatamart. The code below also shows first 10 matches of positive words
words=NULL
pdatamart=data.frame(words)

for (t in combineclean) {
  pdatamart=c(poswords(t),pdatamart)
}
head(pdatamart,10)


#retrieves the positive and negative matching words
negwords=function(tweets){
  nmatch=match(t,negative)
  negw=negative[nmatch]
  negw=negw[!is.na(negw)]
  return(negw)
}

#This function is applied to our combineclean list and a loop is called to add words to a data frame, pdatamart. The code below also shows first 10 matches of positive words
words=NULL
ndatamart=data.frame(words)

for (t in combineclean) {
  ndatamart=c(negwords(t),ndatamart)
}
head(ndatamart,10)

######here 1st use the unlist function and assign that in pwords and nwords

pwords<-sapply(pdatamart,function(x)paste(unlist(x),collapse = " "))

nwords<-sapply(ndatamart,function(x)paste(unlist(x),collapse = " "))



#Plotting high frequency negative and positive words
# the unlist() function to convert the list to vectors. The vector variables pwords and nwords are converted to dataframe objects
dpwords=data.frame(table(pwords))
dnwords=data.frame(table(nwords))

##first mutate the words as character variables and then filter for frequency
#for positive words
dpwords=dpwords%>%
  mutate(pwords=as.character(pwords))%>%
  filter(Freq>1)

#now plot major positive words

ggplot(dpwords,aes(pwords,Freq))+geom_bar(stat="identity",fill="lightblue")+theme_bw()+
  geom_text(aes(pwords,Freq,label=Freq),size=4)+
  labs(x="Major Positive Words", y="Frequency of Occurence",title="Major Positive Words")+
  geom_text(aes(1,5,label=paste("Positive Words")),size=4,hjust=0)+theme(axis.text.x=element_text(angle=45))

#for negative words
dnwords=dnwords%>%
  mutate(nwords=as.character(nwords))


#plot for major negative words
ggplot(dnwords,aes(nwords,Freq))+geom_bar(stat="identity",fill="lightblue")+theme_bw()+
  geom_text(aes(nwords,Freq,label=Freq),size=4)+
  labs(x="Major Negative Words", y="Frequency of Occurence",title="Major Negative Words")+
  geom_text(aes(1,5,label=paste("Negative Words ")),size=4,hjust=0)+theme(axis.text.x=element_text(angle=45))

# Removing common words and creating wordcloud
combinecorpus=Corpus(VectorSource(combineclean))
combinecorpus=tm_map(combinecorpus,removeWords,stopwords("english"))
myStopwords <- c(stopwords('english'), "available", "via")
myStopwords <- c(stopwords('english'),"r", "big","uuuu","uucu","uuf","uufu","uufuf","uufufue","androida","blooddonorsin","clienta","ciudades",
                 "con","conn","cop","des","eampit","eduaubdedubu","eduaubdedububd","eduaubdedubueduaubdedubuuuuueuuudrkumarvis","eec",
                 "engb","españa","euw","evankirstel","href","hrefhttpsdoordacom","htt","http","ipada","iphonea","ipfconline","iwaterbarcelona",
                 "los", "las","mer","nov","otvnews","ppchaudharymos","ravi","relnofollowaaretweeta","relnofollowapp","relnofollowbuffera","relnofollowfacebooka",
                 "relnofollowgrabinboxa","relnofollowhootsuitea","relnofollowifttta","relnofollowmobile","relnofollowpaperlia","relnofollowscbotbackenda",
                 "relnofollowtweetdecka","relnofollowwiocitiesa","relnofollowclickkla","relnofollowfacebooka","relnofollowhootsuitea","uuuufufu","vikezmedia",
                 "relnofollowinstagrama","relnofollowtopicly","relnofollowtwitter","revolució","rfyouthsports","rsprasad","rfyouthsports","scewc","vijayshekhar",
                 "shankar","sunnybhullar","ttot","ububuuc","ucuc","ucuu","uduu","uufudueu","uufue","uuu","uufudueu","uufue","uuuu","uufuuu","uuue","wiomaxmd","ueubuu",                                    
                 "ueuu","ueuuuduueuuf","ufu","ufuu","uidai","una","uub","uubuf","uudufue","uueu","uueucuduf","yfnvillage","yfnmwc","available","via")





combinecorpus <- tm_map(combinecorpus, removeWords, myStopwords)
wordcloud(combinecorpus,scale=c(5,0.5),random.order = TRUE,rot.per = 0.20,use.r.layout = FALSE,colors = brewer.pal(6,"Dark2"),max.words = 300)


#Analyzing and plotting high frequency words

dtm=DocumentTermMatrix(combinecorpus)

# #removing sparse terms
dtms=removeSparseTerms(dtm,.99)
freq=sort(colSums(as.matrix(dtm)),decreasing=TRUE)


#get some more frequent terms
findfd<-findFreqTerms(dtm,lowfreq=100)
findfd
#convert the matrix to a data frame, filter for Minimum frequency > 75and plot using ggplot2
wf=data.frame(word=names(freq),freq=freq)
wfh=wf%>%
  filter(freq>=550,!word==tolower(findfd))


ggplot(wfh,aes(word,freq))+geom_bar(stat="identity",fill='lightblue')+theme_bw()+
  
  geom_text(aes(word,freq,label=freq),size=4)+labs(x="High Frequency Words",y="Number of Occurences", title=paste("High Frequency Words and Occurence"))+
  theme(axis.text.x=element_text(angle=45,hjust=1))
