# 관련 패키지 설치
pacman::p_load(KoNLP, wordcloud, plyr, twitteR, tm)

consumer_key <- "[YOUR CONSUMER KEY HERE]"
consumer_secret <- "[YOUR CONSUMER SECRET HERE]"
access_token <- "[YOUR ACCESS TOKEN HERE]"
access_secret <- "[YOUR ACCESS SECRET HERE]"
options(httr_oauth_cache=T) #This will enable the use of a local file to cache OAuth access credentials between R sessions.
setup_twitter_oauth(consumer_key,
		consumer_secret,
		access_token,
		access_secret)


# 키워드 설정
keyword <- 'kth'
# 트위터에서 키워드로 검색
result <- searchTwitter(keyword, since='2013-01-21', until='2013-01-27', lang='ko',n=1000)
# 결과 중에서 텍스트에 해당하는 부분만 뽑는다
result.df <- twListToDF(result)
result.text <- result.df$text
# 불필요한 문자를 걸러준다
result.text <- gsub("\n", "", result.text)
result.text <- gsub("\r", "", result.text)
result.text <- gsub("RT", "", result.text)
result.text <- gsub("http", "", result.text)
# 문자 분리
result_nouns <- Map(extractNoun, result.text)
# 쓸모없는 문자들을 제거한다. 특히 영문자의 경우 tm의 stopwords를 활용한다.
result_wordsvec <- unlist(result_nouns, use.name=F)
result_wordsvec <- result_wordsvec[-which(result_wordsvec %in% stopwords("english"))]
result_wordsvec <- gsub("[[:punct:]]","", result_wordsvec)
                        result_wordsvec <- Filter(function(x){nchar(x)>=2}, result_wordsvec)
                        # 문자 카운팅
                        result_wordcount <- table(result_wordsvec)
                        # 컬러 세팅
                        pal <- brewer.pal(12,"Paired")
                        # 폰트 세팅
                        windowsFonts(malgun=windowsFont("맑은 고딕"))
                        # 그리기
                        wordcloud(names(result_wordcount), freq=result_wordcount, scale=c(5,0.5), min.freq=5, random.order=F, rot.per=.1, colors=pal, family="malgun")