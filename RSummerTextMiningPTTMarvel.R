###RSummer Text Mining PPT Marvel
###Parser
originalURL1<-'https://www.ptt.cc/bbs/marvel/index'
startpage1<-590
endpage1<-600
marveldata<-data.frame()
for(i in startpage1:endpage1){
  marvelURL<-paste(originalURL1,i,'.html',sep='')
  marvelURLExists<-url.exists(marvelURL)
  if(marvelURLExists){
    html = getURL(marvelURL, ssl.verifypeer = FALSE)
    xml = htmlParse(html, encoding ='utf-8')
    title = xpathSApply(xml, "//div[@class='title']/a//text()", xmlValue)
    author = xpathSApply(xml, "//div[@class='author']", xmlValue)
    path = xpathSApply(xml, "//div[@class='title']/a//@href")
    date = xpathSApply(xml, "//div[@class='date']", xmlValue)
    response = xpathSApply(xml, "//div[@class='nrec']", xmlValue)
    tempdata = data.frame(title, author, path, date, response)
    marveldata = rbind(marveldata, tempdata)
  }
}
###Saving tesxt file
library(XML)
library(bitops)
library(RCurl)
library(NLP)
library(httr)
alldata<-as.data.frame(marveldata)
orgURL<- "https://www.ptt.cc"
for( i in 1:nrow(alldata))
{
  pttURL <- paste(orgURL, alldata$path[i], sep='')
  urlExists = url.exists(pttURL)
  
  if(urlExists)
  {
    html = getURL(pttURL, ssl.verifypeer = FALSE, encoding='UTF-8')
    xml = htmlParse(html, encoding='UTF-8')
    text = xpathSApply(xml, "//div[@id='main-content']", xmlValue)
    name <- paste('/Users/priest/Desktop/R/alltext/', i, '.txt', sep='')
    write(text, name)
  }
}

###Wordcloud
library(jiebaRD)
library(jiebaR)       # 斷詞利器
library(NLP)
library(tm)           # 文字詞彙矩陣運算
library(slam)         # 稀疏矩陣運算
library(RColorBrewer)
library(wordcloud)    # 文字雲
library(topicmodels)  # 主題模型
library(igraph)       # 主題模型關聯
orgPath = "/Users/priest/Desktop/R/alltext/"
text = Corpus(DirSource(orgPath), list(language = NA))
text <- tm_map(text, removePunctuation)
text <- tm_map(text, removeNumbers)
text <- tm_map(text, function(word)
{ gsub("[A-Za-z0-9]", "", word) })

mixseg = worker()
mat <- matrix( unlist(text), nrow=length(text) )
totalSegment = data.frame()
for( j in 1:length(text) )
{
  for( i in 1:length(mat[j,]) )
  {
    result = segment(as.character(mat[j,i]), jiebar=mixseg)
  }
  totalSegment = rbind(totalSegment, data.frame(result))
}

totaldiff = levels(totalSegment$result)
countMat = data.frame(totaldiff, c(rep(0, length(totaldiff))))
for( i in 1:length(totalSegment$result) )
{
  for( j in 1:length(totaldiff) )
  {
    if( totaldiff[j] == as.character(totalSegment$result[i]) )
    {
      countMat[j,2] = countMat[j,2] + 1
    }
  }
}

names(countMat) = c("totaldiff", "freq")
countMat[,2] = countMat[,2] / sum(countMat[,2]) 
par(family=('Heiti TC Light'))
wordcloud(countMat$totaldiff, countMat$freq, min.freq = 1, random.order = F, ordered.colors = T, 
          colors = rainbow(length(totaldiff)))