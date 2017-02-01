library(tm)##text mining library
library(wordcloud)
library(RColorBrewer)
library(dplyr)
library(tidytext)

##(1)list the names of txt files in a vector
##and then use "substr" to get the names of each speeach
setwd("C:/Users/Elaine/Documents/Columbia/Spring_2017/AppliedDataScience/Spr2017-Proj1-eeelaine")
folder.path=paste(getwd(),"/data/InauguralSpeeches/",sep = "")
speeches=list.files(path = folder.path, pattern = "*.txt")
prez.out=substr(speeches, 6, nchar(speeches)-4)

##(2)set up a Vcorpus of our files using dirsource 
length.speeches=rep(NA, length(speeches))
ff.all<-Corpus(DirSource(folder.path))

##(3)clean the text
ff.all<-tm_map(ff.all, stripWhitespace) ##eliminating extra whitespace
ff.all<-tm_map(ff.all, content_transformer(tolower))##convert to lower cases
ff.all<-tm_map(ff.all, removeWords, stopwords("english"))##stop word is a commonly used word like "the"
ff.all<-tm_map(ff.all, removeWords, character(0))
ff.all<-tm_map(ff.all, removePunctuation)##remove punctuation
tdm.all<-TermDocumentMatrix(ff.all)##create a matrix of terms(rows) and document(columns) names
tdm.tidy=tidy(tdm.all)##tidy the matrix into a list with count

##summarise the count of terms in term document matrix
##"group_by" is necessary in this case, it will summarise by the group of terms
tdm.overall=summarise(group_by(tdm.tidy, term), sum(count))

##(4)use wordcloud to produce pictures
wordcloud(tdm.overall$term, tdm.overall$`sum(count)`,
          scale=c(5,0.5),
          max.words=100,
          min.freq=1,
          random.order=FALSE,
          rot.per=0.3,
          use.r.layout=T,
          random.color=FALSE,
          colors=brewer.pal(9,"Blues"))

##(5)create a document term matrix
dtm <- DocumentTermMatrix(ff.all,
                          control = list(weighting =
                                           function(x)
                                             weightTfIdf(x, normalize =
                                                           FALSE),
                                         stopwords = TRUE))
ff.dtm=tidy(dtm)
#ff.all<-tm_map(ff.all, stemDocument)

##get the picture of each wordcloud
for(i in 1:length(speeches)){
#  #crude=stemDocument(ff.all[[i]])
#  crude=Corpus(VectorSource(ff.all[[i]]))
#  tdm <- TermDocumentMatrix(crude[1], list(wordLengths=c(3, Inf)))
#  m <- as.matrix(tdm)
#  v <- sort(rowSums(m),decreasing=TRUE)
#  d <- data.frame(word = names(v),freq=v)
  
  png(paste(getwd(),"/output1/", prez.out[i], ".png", sep=""),
      width=300, height=300)
  wordcloud(ff.dtm$term[ff.dtm$document==speeches[i]],
            ff.dtm$count[ff.dtm$document==speeches[i]],
              scale=c(5,0.5),
              max.words=200,
              min.freq=1,
              random.order=FALSE,
              rot.per=0,
              use.r.layout=FALSE,
              random.color=FALSE,
              colors=brewer.pal(10,"Blues"), 
            main=prez.out[i])
  dev.off()
  
}
