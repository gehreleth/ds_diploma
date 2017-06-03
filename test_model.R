require(tm)
require(NLP)
require(openNLP)

if(!exists("m", mode="list")) source("build_model.R")

convert_text_to_sentences <- function(text, lang = "en") {
  # Function to compute sentence annotations using the Apache OpenNLP Maxent sentence detector employing the default model for language 'en'. 
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

w1match <- 0L
w1miss <- 0L
w3match <- 0L
w3miss <- 0L

process.single.sentence <- function(rawSentence) {
  tokens <- tokenize.sentence(rawSentence)
  for (nToken in 2:(length(tokens) - 1)) { #tokens[1] is Start marker, skip last word
    testStr <- paste(paste(tokens[2:nToken], collapse = ' '), ' ', sep = '')
    predictions <- predict.next.word(m, testStr, num.possibilities = 3)
    if (nrow(predictions) > 0) {
      nextWord <- tokens[nToken + 1]
      w1m <- nextWord == predictions[1]$token
      assign("w1match", w1match + w1m, envir = .GlobalEnv)
      assign("w1miss", w1miss + 1 - w1m, envir = .GlobalEnv)
      w3m <- nextWord %in% predictions$token
      assign("w3match", w3match + w3m, envir = .GlobalEnv)
      assign("w3miss", w3miss + 1 - w3m, envir = .GlobalEnv)
      if (w3m) {
        print(sprintf("Match \"%s\", valiants = {%s}, success rate = %.4f",
                      nextWord, paste(predictions$token, collapse = ', '),
                      (as.numeric(w3match) / (as.numeric(w3match) + as.numeric(w3miss)))))
      } else {
        print(sprintf("Miss \"%s\", valiants = {%s}, success rate = %.4f",
                      nextWord, paste(predictions$token, collapse = ', '),
                      (as.numeric(w3match) / (as.numeric(w3match) + as.numeric(w3miss)))))
      }
    } else {
      print(sprintf("Error - No variants given, context = [%s]"), testStr)
    }
  }
}

process.single.file <- function(fileName) {
  con <- file(fileName, "r", blocking = FALSE)
  text <- readLines(con, encoding = "UTF-8")
  for (lineNum in 1:length(text)) {
    line <- text[[lineNum]]
    sentences <- convert_text_to_sentences(line)
    for (sentNum in 1:length(sentences)) {
      process.single.sentence(sentences[[sentNum]])
    }
  }
}

process.single.file('./src_data/en_US/en_US.blogs.txt')
process.single.file('./src_data/en_US/en_US.news.txt')
process.single.file('./src_data/en_US/en_US.twitter.txt')
