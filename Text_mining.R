#download the three musketeers
#install.packages("gutenbergr")
library(gutenbergr)
book<-gutenberg_download(c(1257))

library(dplyr)
library(stringr)
#install.packages("tidytext")
library(tidytext)
library(wordcloud)
library(tidyr)
tidy_book<-book%>%unnest_tokens(word,text)%>%
  mutate(word=str_extract(word,"[a-z']+"))#extract words
#first word cloud
data(stop_words)
tidy_book%>%anti_join(stop_words)%>%
  count(word,sort=TRUE)%>%
  with(wordcloud(word,n,max.words=100))

#sentiment

book_sentiment<-tidy_book%>%
  inner_join(get_sentiments("bing"))%>%
  mutate(linenumber=row_number())%>%
  count(index=linenumber%/%80,sentiment)%>%
  spread(sentiment,n,fill=0)%>%
  mutate(sentiment=positive-negative)

library(ggplot2)
ggplot(book_sentiment,aes(index,sentiment))+
  geom_col(show.legend = FALSE)
