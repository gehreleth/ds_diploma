library(caret)
library(tm)
options(mc.cores=1)
library(RWeka)

set.seed(31337)

load.text.as.table <- function(filename) {
  lines <- read.table(filename, sep='\n', stringsAsFactors = FALSE, encoding = "ISO-8859-1")
  data.frame(id = 1:length(lines$V1), text = lines$V1, stringsAsFactors=FALSE)
}

src <- load.text.as.table('./src_data/en_US/en_US.news.txt')
#inSlice <- createDataPartition(src$id, p = .2, list = FALSE)
inSlice <- 1:lengths(src)[2]
slice <- src[inSlice,]

K <- VCorpus(VectorSource(paste(slice$text, collapse = ' ')))
#K <- VCorpus(DirSource('./src_data/en_US'))
K <- tm_map(K, removeNumbers)
#K <- tm_map(K, removePunctuation)
#K <- tm_map(K, stripWhitespace)
#K <- tm_map(K, tolower)
#K <- tm_map(K, removeWords, stopwords("english"))
#K.stem <- tm_map(K, stemDocument, language = "english") 

#inspect(K[[1]]) - show document
#meta(K[[1]], tag = "comment") <- "A short comment." - modify document metadata

#adtm <- DocumentTermMatrix(K.stem) 
#adtm <- removeSparseTerms(adtm, 0.9)

#inspect(adtm)
#findFreqTerms(adtm)

#findFreqTerms(adtm, lowfreq=1) # find terms with a frequency higher than 10
#findAssocs(adtm, "russia", 0.001) # just looking for some associations  
#findAssocs(adtm, "china",.5)


#mx <- TermDocumentMatrix(Corpus(VectorSource(slice$text)),
#                         control = list(tokenize = NGramTokenizer))

TrigramTokenizer <- function(x) { RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 3, max = 3)) }

tdm <- TermDocumentMatrix(K, control = list(tokenize = TrigramTokenizer))
tdm <- removeSparseTerms(tdm, 0.9)
inspect(tdm)

freq = sort(rowSums(as.matrix(tdm)), decreasing = TRUE)
freq.df = data.frame(word=names(freq), freq=freq)


#K1 <- tm_map(K1, tolower)
#K1 <- tm_map(K1, removeNumbers)
#K1 <- tm_map(K1, removePunctuation)
#K1 <- tm_map(K1, stripWhitespace)
#K1 <- tm_map(K1, removeWords, stopwords("english"))
#K1.stem <- tm_map(K1, stemDocument, language = "english") 

lines <- readLines('./src_data/en_US/en_US.news.txt', encoding = "utf-8", warn = FALSE)

# Remove junk. Kind thanks to another student, Alfredo Hung, whose thoriugh work
# allowed me not to invet it myself.

lines <- gsub("â€™", "'", lines)
lines <- gsub("\"", " ", lines) 
spchars <- c("â","€","œ","¥","™","ð","Ÿ","\",Â", "`","˜","#")
for (i in 1:length(spchars)) {
  lines <- gsub(spchars[i], "", lines)
}

# Replace apostrophes in word contractions
lines <- gsub("'m", " am", lines)
lines <- gsub("'re", " are", lines)
lines <- gsub("'s", " is", lines)
lines <- gsub("can't", "can not", lines)
lines <- gsub("won't", "will not", lines)
lines <- gsub("n't", " not", lines)
lines <- gsub("'ve", " have", lines)
lines <- gsub("'d", " had", lines)
lines <- gsub("'ll", " will", lines)


K1 <- VCorpus(VectorSource(lines))
K1 <- tm_map(K1, tolower)
K1 <- tm_map(K1, stripWhitespace)
tdm1 <- TermDocumentMatrix(K1, control = list(removeNumbers=TRUE, removePunctuation = TRUE, stopwords=TRUE))
freq.tdm1 = sort(rowSums(as.matrix(tdm1)), decreasing = TRUE)
freq.tdm1.df = data.frame(word=names(freq.tdm1), freq=freq.tdm1)



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