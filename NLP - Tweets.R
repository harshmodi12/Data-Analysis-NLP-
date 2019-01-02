data <- read.csv("data_elonmusk.csv")

library(tm)
library(stringr)
install.packages("SnowballC")
library(SnowballC)
library(ggplot2)
install.packages("wordcloud")
library(wordcloud)

data_df <- as.data.frame(data)

data_df$row.ID <- NULL
data_df$User <- NULL

myCorpus <- Corpus(VectorSource(data_df$Tweet))

#Lower case
myCorpus <- tm_map(myCorpus, content_transformer(str_to_lower))

#remove URL
removeURL <- function(x) gsub("http[^[:space:]]*", "", x) 
myCorpus <- tm_map(myCorpus, content_transformer(removeURL))

#remove Numbers and punctuations
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x) 
myCorpus <- tm_map(myCorpus, content_transformer(removeNumPunct))

#remove stopwords
myStopwords <- myStopwords <- c(stopwords('english')) 
myCorpus <- tm_map(myCorpus, removeWords, myStopwords) 

#remove whitespace
myCorpus <- tm_map(myCorpus, stripWhitespace)

#remove rt
myCorpus <- tm_map(myCorpus, removeWords, c("rt"))

#stemming
myCorpus <- tm_map(myCorpus, stemDocument)
inspect(myCorpus[1:3])

#Term Document matrix
tdm <- TermDocumentMatrix(myCorpus, control = list(wordLengths = c(1, Inf))) 
tdm


#Frequency graph
term.freq <- rowSums(as.matrix(tdm)) 
term.freq <- subset(term.freq, term.freq >= 80) 
term.freq
#v <- sort(rowSums(m),decreasing=TRUE)
#d <- data.frame(word = names(v),freq=v)
#d

#head(d, 10)
df <- data.frame(term = names(term.freq), freq = term.freq)
df

#Frequency of terms in Elon Musk's Tweets
ggplot(df, aes(x=term, y=freq)) + 
  geom_bar(stat="identity", color = "red") + 
  scale_fill_manual(values = c('black','lightgray')) +
  theme_minimal()+
  xlab("Terms") + 
  ylab("Count") + 
  geom_text(aes(label=freq), hjust=-0.3, color="black", size=4) +
  coord_flip()  

#Word Cloud
wordcloud(words = df$term, freq = df$freq, min.freq = 1,
          max.words=80, random.order=FALSE, rot.per = 0, 
          colors=brewer.pal(9, "Set1"))

#Finding frequent terms
findFreqTerms(tdm, lowfreq = 80)

#Finding Associations
findAssocs(tdm, terms = "spacex", corlimit = 0.2)
findAssocs(tdm, terms = "dragon", corlimit = 0.2)

