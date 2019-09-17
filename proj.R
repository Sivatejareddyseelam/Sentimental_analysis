library("RColorBrewer")
library("wordcloud")
library("NLP")
library(tm)
library(twitteR)
library(ROAuth)
library(plyr)
library(stringr)
library(base64enc)
library(e1071)
library(SnowballC)
library(caret)

consumerKey<-""
consumerSecret<-""
accessToken<-""
accessTokenSecret<-""
setup_twitter_oauth(consumerKey,consumerSecret,accessToken,accessTokenSecret)

Tweets=searchTwitter("#Area51",n=1000,lang="en")
Tweets.df<-twListToDF(Tweets)
head(Tweets.df$text)
tweet_text=Tweets.df$text
tweet_text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ", tweet_text)
tweet_text= gsub("(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", " ", tweet_text)
tweet_text = gsub("#\\w+", " ", tweet_text)
tweet_text = gsub("@\\w+", " ", tweet_text)
tweet_text = gsub("[[:punct:]]", " ", tweet_text)
tweet_text = gsub("[[:digit:]]", " ", tweet_text)
tweet_text = gsub("[ \t]{2,}", " ", tweet_text)
tweet_text = gsub("^\\s+|\\s+$", "", tweet_text)
tweet_text = gsub("\n","",tweet_text)
tweet_text = iconv(tweet_text,"UTF-8","ASCII",sub="")
head(tweet_text)
tweet_text = tweet_text[!is.na(tweet_text)]
names(tweet_text)=NULL
tweet_text=unique(tweet_text)
library(sentimentr)
tweet_text<-get_sentences(tweet_text)
score<-sentiment(tweet_text)
tweet_text_Corpus<- Corpus(VectorSource(tweet_text))
tweet_text_Corpus<- tm_map(tweet_text_Corpus, removePunctuation)
tweet_text_Corpus<- tm_map(tweet_text_Corpus, content_transformer(tolower))
tweet_text_Corpus<- tm_map(tweet_text_Corpus, removeWords, stopwords("english"))
tweet_text_Corpus<- tm_map(tweet_text_Corpus, stripWhitespace)
tweet_text_Dtm<- DocumentTermMatrix(tweet_text_Corpus,control = list(weighting= function(x) weightBin(x)))
tweet_text_Dtm<-removeSparseTerms(tweet_text_Dtm,.99)
scr<-score[,"sentiment"]
m<-median(scr[["sentiment"]])
for (i in 1:length(scr[["sentiment"]])){
  if (scr[i,"sentiment"]>=0.0){
    scr[i,"label"]="positive"
  }else{
    scr[i,"label"]="negative"
  }
}
length(scr[["label"]])
dtm.train <- tweet_text_Dtm[1:220,]
dtm.cross.val <- tweet_text_Dtm[220:350,]
dtm.test <- tweet_text_Dtm[350:546,]
y.train<-scr[1:220,"label"]
y.cross.val<-scr[220:350,"label"]
y.test<-scr[350:546,"label"]
x.train <- as.matrix(dtm.train)
x.val<-as.matrix(dtm.cross.val)
x.test<-as.matrix(dtm.test)
training_data <- as.data.frame(cbind(y.train,x.train))
cross_val_data <- as.data.frame(x.val)
test_data <- as.data.frame(x.test)
sv <- svm(label~., training_data, type="C-classification", kernel="linear", cost=1)
prediction1<-predict(sv,test_data)                                                                                                                                              
c.f<-table("Predictions"= prediction1,  "Actual" = y.test[["label"]] )
TP = c.f[1,1] + c.f[2,2];  # true predictions
total = nrow(test_data);  # total predictions
acc = TP / total
ypred.train<-scr[1:220,"sentiment"]
ypred.cross.val<-scr[220:350,"sentiment"]
ypred.test<-scr[350:546,"sentiment"]
training_pred_data<- as.data.frame(cbind(ypred.train,x.train))
linear<-lm(sentiment~., training_pred_data)                                   
summary(linear)
cross_val_pred_data<- as.data.frame(cbind(ypred.cross.val,x.val))
linear_val<-lm(sentiment~., cross_val_pred_data)
summary(linear_val)
test_pred_data<- as.data.frame(cbind(ypred.test,x.test))
linear_test<-lm(sentiment~., test_pred_data)
summary(linear_test)
twt=c(tweet_text[[350]],tweet_text[[351]],tweet_text[[352]],tweet_text[[353]],
      tweet_text[[354]],tweet_text[[355]],tweet_text[[356]],tweet_text[[357]],
      tweet_text[[358]],tweet_text[[359]],tweet_text[[360]])
label=scr[350:360,"label"]
prd=prediction1[1:11]
output=data.frame(original_tweets=twt,original_sentiment=label,predicted_sentiment=prd,stringsAsFactors = FALSE)
score1=scr[350:360,"sentiment"]
pred_scr=linear_test[["fitted.values"]][1:11]
output_reg=data.frame(original_tweets=twt,original_sentiment_score=score1,predicted_sentiment_score=pred_scr,stringsAsFactors = FALSE)
summary(linear_test)
error_per=abs((linear_test[["residuals"]]/scr[350:546,"sentiment"]))*100
mean(error_per)
