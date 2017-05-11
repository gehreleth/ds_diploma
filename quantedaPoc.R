library(quanteda)
library(R.utils)
library(doParallel)
library(slam) # maybe not needed
require(dplyr)
library(data.table)


f <- readLines('./src_data/en_US/en_US.twitter.pp.txt')
qcorpus <- corpus(f)

parallelizeTask <- function(task, ...) {
  # Calculate the number of cores
  ncores <- detectCores() - 1
  # Initiate cluster
  cl <- makeCluster(ncores)
  registerDoParallel(cl)
  #print("Starting task")
  r <- task(...)
  #print("Task done")
  stopCluster(cl)
  r
}

# Returns a vector of profanity words
getProfanityWords <- function(corpus) {
  profanityFileName <- "profanity.txt"
  if (!file.exists(profanityFileName)) {
    profanity.url <- "https://raw.githubusercontent.com/shutterstock/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/master/en"
    download.file(profanity.url, destfile = profanityFileName, method = "curl")
  }
  
  if (sum(ls() == "profanity") < 1) {
    profanity <- read.csv(profanityFileName, header = FALSE, stringsAsFactors = FALSE)
    profanity <- profanity$V1
    profanity <- profanity[1:length(profanity)-1]
  }
  
  profanity
}

makeSentences <- function(input) {
  output <- tokenize(input, what = "sentence", removeNumbers = TRUE,
                     removePunct = TRUE, removeSeparators = TRUE,
                     removeTwitter = TRUE, removeHyphens = TRUE)
  #output <- removeFeatures(output, getProfanityWords())
  unlist(lapply(output, function(a) paste('#s#', toLower(a), '#e#')))
}

makeTokens <- function(input, n = 1L) {
  tokenize(input, what = "word", removeNumbers = TRUE,
           removePunct = TRUE, removeSeparators = TRUE,
           removeTwitter = FALSE, removeHyphens = TRUE,
           ngrams = n, simplify = TRUE)
}

sentences <- parallelizeTask(makeSentences, qcorpus)
ngram1 <- parallelizeTask(makeTokens, sentences, 1)
ngram2 <- parallelizeTask(makeTokens, sentences, 2)
ngram3 <- parallelizeTask(makeTokens, sentences, 3)
ngram4 <- parallelizeTask(makeTokens, sentences, 4)

dfm1 <- parallelizeTask(dfm, ngram1)
dfm2 <- parallelizeTask(dfm, ngram2)
dfm3 <- parallelizeTask(dfm, ngram3)
dfm4 <- parallelizeTask(dfm, ngram4)