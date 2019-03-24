install.packages("twitteR")
install.packages("purrr")
install.packages("ROAuth")
install.packages("RCurl")
require('ROAuth')
require('RCurl')
install.packages("plyr")
install.packages("stringr")
library(twitteR)
library(purrr)
library(plyr)
library(stringr)

##SENTIMENT FUNTION

score.sentiment <- function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  score<-laply(sentences,function(sentence, pos.words, neg.words){
    sentence<-gsub('[[:punct:]]', "",sentence)#gsub is a function from stringr which means gsub=global subsition of punctions in the line of code
    #subsitute punction with the null space in var. sentence
    sentence<-gsub('[[:cntrl:]]', "",sentence)#contol words
    sentence<-gsub('\\d+',"",sentence)#digits
    sentence<-tolower(sentence)#conversion to lower case
    word.list<-str_split(sentence,'\\s+')#split sentence into words and store them in list
    words<-unlist(word.list)#converting list to vector
    #matching
    #returning the location of that word in the list
    pos.matches<-match(words,pos.words) 
    neg.matches<-match(words,neg.words)
    #whenever there is a NA in pos.matches store it in num
    pos.matches<-!is.na(pos.matches)
    neg.matches<-!is.na(neg.matches)
    #sum(num) will be give sum of true
    score<-sum(pos.matches)-sum(neg.matches)
    return(score)
  },pos.words,neg.words, .progress=.progress)
  score.df<-data.frame(score=scores,text=sentences)
  return(scores.df)
}

##NEW
pos.words=scan('/location',what='character',comment.char = ';')
neg.words=scan('/location',what = 'character',comment.char = ';')
#function
bscore<-score.sentiment(tweet_df$text,pos.words,neg.words,.progress = 'text'hist)
rscore<-score.sentiment(tweet2_df$text,pos.words,neg.words,.progress = 'text')
#graph
hist(rscore$score)
hist(bscore$score)

#steaming tweets from twitter
#------------
consumerKey<-""
reqURL<-""
accessURL<-""
authURL<-""
consumerSecret<-""
accessToken<-""
accessTokenSecret<-""
twitCred<-OAuthFactory$new()

twitCred$handshake()
  setup_twitter_oauth(consumer_key,consumer_secret,accessToken,accessTokenSecret)
  