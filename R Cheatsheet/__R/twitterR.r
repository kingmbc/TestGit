library(twitteR)
library(KoNLP)
library(wordcloud)
library(tm)

# gogamza <- userTimeline(user="gogamza", n=1000)
#
# gogamzatw <- c()
# for(i in 1:length(gogamza)){
#   gogamzatw <- append(gogamzatw, gogamza[[i]]$text)
# }
#
# gogamzaNoun <- sapply(gogamzatw, extractNoun,USE.NAMES=F)
#
# gogamza.corpus <- Corpus(VectorSource(gogamzaNoun))
#
# control <- list(stopwords=T, removePunctuation = T, removeNumbers=T, minDocFreq=2)
# tw.tdm <- TermDocumentMatrix(gogamza.corpus, control)


gogamza <- getUser("gogamza")
#gogamza.friends <- gogamza$getFriends()
#gogamza.friendsID <- gogamza$getFriendIDs()
gogamza.followers <- gogamza$getFollowers()


#�ȷο����� �ڱ� �Ұ��� ���Ϳ� ���� �Ѵ�.
followerDesc <- c()

for(i in gogamza.followers){
  followerDesc <- append(followerDesc, i$description)
}


#������� ���ڵ��� �����Ѵ�.
followerDesc <- gsub("\n","", followerDesc)
followerDesc <- gsub("\r", "", followerDesc)


nouns <- Map(extractNoun, followerDesc)

wordsvec <- unlist(nouns, use.name=F)
#������� ���ڵ��� �����Ѵ�. Ư�� �������� ��� tm�� stopwords�� Ȱ���Ѵ�.
wordsvec <- wordsvec[-which(wordsvec %in% stopwords("english"))]
wordsvec <- gsub("[[:punct:]]","", wordsvec)
wordsvec <- Filter(function(x){nchar(x)>=2}, wordsvec)

wordcount <- table(wordsvec)
pal <- brewer.pal(8,"Dark2")


wordcloud(names(wordcount),freq=wordcount,scale=c(4,0.5),min.freq=10,
          random.order=T,rot.per=.1,colors=pal)