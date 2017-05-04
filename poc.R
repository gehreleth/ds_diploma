library(caret)
library(tm)
library(RWeka)
#library(NLP)
#library(openNLP)
#library(kernlab)
#library(openNLPmodels.en)

set.seed(31337)

load.text.as.table <- function(filename) {
  lines <- read.table(filename, sep='\n', stringsAsFactors = FALSE)
  data.frame(id = 1:length(lines$V1), text = lines$V1, stringsAsFactors=FALSE)
}

src <- load.text.as.table('./src_data/en_US/en_US.news.txt')
inSlice <- createDataPartition(src$id, p = .6, list = FALSE)
slice <- src[inSlice,]

K <- Corpus(VectorSource(slice$text))
K <- tm_map(K, removeNumbers)
K <- tm_map(K, removePunctuation)
K <- tm_map(K, stripWhitespace)
K <- tm_map(K, tolower)
K <- tm_map(K, removeWords, stopwords("english"))
K.stem <- tm_map(K, stemDocument, language = "english") 

#inspect(K[[1]]) - show document
#meta(K[[1]], tag = "comment") <- "A short comment." - modify document metadata

adtm <-DocumentTermMatrix(K.stem) 
adtm <- removeSparseTerms(adtm, 0.9)

inspect(adtm)
findFreqTerms(adtm)

findFreqTerms(adtm, lowfreq=1) # find terms with a frequency higher than 10
findAssocs(adtm, "russia", 0.001) # just looking for some associations  
findAssocs(adtm, "china",.5)


#mx <- TermDocumentMatrix(Corpus(VectorSource(slice$text)),
#                         control = list(tokenize = NGramTokenizer))

TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))

tdm <- TermDocumentMatrix(K, control = list(tokenize = TrigramTokenizer))
tdm <- removeSparseTerms(tdm, 0.75)
inspect(tdm[1:30,1:40])


## Some text.
s <- paste(c("Pierre Vinken, 61 years old, will join the board as a ",
             "nonexecutive director Nov. 29.\n",
             "Mr. Vinken is chairman of Elsevier N.V., ",
             "the Dutch publishing group."),
           collapse = "")
s <- as.String(s)
## Need sentence and word token annotations.
sent_token_annotator <- Maxent_Sent_Token_Annotator()
word_token_annotator <- Maxent_Word_Token_Annotator()
a2 <- annotate(s, list(sent_token_annotator, word_token_annotator))
## Entity recognition for persons.
entity_annotator <- Maxent_Entity_Annotator()
entity_annotator
annotate(s, entity_annotator, a2)
## Directly:
entity_annotator(s, a2)
## And slice ...
s[entity_annotator(s, a2)]
## Variant with sentence probabilities as features.
annotate(s, Maxent_Entity_Annotator(probs = TRUE), a2)