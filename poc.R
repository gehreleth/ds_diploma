library(caret)
library(tm)
options(mc.cores=1)
options(java.parameters = "-Xmx2048m")
library(RWeka)
library(openNLP)
library(openNLPmodels.en)
library(wordcloud)

news.nouns <- read.csv('./src_data/en_US/top_news_words.csv', sep = ';')
news.nouns$count <- as.numeric(news.nouns$count)
wordcloud(news.nouns$stem, news.nouns$count,c(8,.3))


set.seed(31337)

load.text.as.table <- function(filename) {
  lines <- read.table(filename, sep='\n', stringsAsFactors = FALSE, encoding = "utf-8")
  data.frame(id = 1:length(lines$V1), text = lines$V1, stringsAsFactors=FALSE)
}

load.corpus <- function(filename) {
  lines <- readLines(filename, encoding = "utf-8", warn = FALSE)
  
  # Remove junk. Kind thanks to another coursera student, Alfredo Hung, whose thorough work
  # allowed me not to invent this myself.
  
  lines <- gsub("Ã¢â‚¬â„¢", "'", lines)
  lines <- gsub("\"", " ", lines) 
  spchars <- c("Ã¢","â‚¬","Å“","Â¥","â„¢","Ã°","Å¸","\",Ã‚", "`","Ëœ","#")
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
  
  VCorpus(VectorSource(lines))
}

load.preprocessed.corpus <- function(sentences, loadFactor) {
  VCorpus(VectorSource(sentences$text))
}

lines <- load.text.as.table('./src_data/en_US/en_US.twitter.pp.txt');
Kpp <- load.preprocessed.corpus(lines)

# These two functions are based on Tony Breyal's examples from StackOverflow
# http://stackoverflow.com/questions/18712878/r-break-corpus-into-sentences
convert.text.to.sentences <- function(text, lang = "en") {
  # Function to compute sentence annotations using the Apache OpenNLP Maxent sentence detector
  # employing the default model for language 'en'. 
  sentence_token_annotator <- Maxent_Sent_Token_Annotator(language = lang)
  
  # Convert text to class String from package NLP
  text <- as.String(text)
  
  # Sentence boundaries in text
  sentence.boundaries <- annotate(text, sentence_token_annotator)
  
  # Extract sentences
  sentences <- text[sentence.boundaries]
  
  # return sentences
  return(sentences)
}

reshape.corpus <- function(current.corpus, FUN, ...) {
  # Extract the text from each document in the corpus and put into a list
  text <- lapply(current.corpus, content)
  
  # Basically convert the text
  docs <- lapply(text, FUN, ...)
  docs <- as.vector(unlist(docs))
  
  # Create a new corpus structure and return it
  new.corpus <- VCorpus(VectorSource(docs))
  return(new.corpus)
}

# This is a long process. Let's make a cache
if (file.exists('./processed_news_corpus.bin')) {
  print('Loading news corpus from cache')
  load(file = './processed_news_corpus.bin')
} else {
  print('Processing raw news corpus. This might take a while ...')
  K.news <- load.corpus('./src_data/en_US/en_US.news.txt')
  K.news <- reshape.corpus(K.news, convert.text.to.sentences)
  save(K.news, file = "./processed_news_corpus.bin")
}

if (file.exists('./processed_blogs_corpus.bin')) {
  print('Loading blogs corpus from cache')
  load(file = './processed_blogs_corpus.bin')
} else {
  print('Processing raw blogs corpus. This might take a while ...')
  K.blogs <- load.corpus('./src_data/en_US/en_US.blogs.txt')
  K.blogs <- reshape.corpus(K.blogs, convert.text.to.sentences)
  save(K.blogs, file = "./processed_blogs_corpus.bin")
}

if (file.exists('./processed_twitter_corpus.bin')) {
  print('Loading twitter corpus from cache')
  load(file = './processed_twitter_corpus.bin')
} else {
  print('Processing raw twitter corpus. This might take a while ...')
  K.twitter <- load.corpus('./src_data/en_US/en_US.twitter.txt')
  K.twitter <- reshape.corpus(K.twitter, convert.text.to.sentences)
  save(K.twitter, file = "./processed_twitter_corpus.bin")
}
