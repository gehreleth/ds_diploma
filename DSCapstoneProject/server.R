library(data.table)
library(stringi)
require(wordcloud)
require(RColorBrewer)

token.ix <-function(model, tokens) {
  rv <- model$lookupTable[tokens]$ix
  rv[is.na(rv)] <- model$lookupTable["#unk"]$ix
  rv
}

token.name <- function(model, ix) {
  model$lookupTable[ix]$name
}

get.next.word.prefix <- function(sentence) {
  empty.str <- function(arg) {
    sum(!stri_isempty(arg)) == 0
  }
  
  if (!(empty.str(sentence) || sum(grepl(pattern ='\\s+$', x=sentence)) > 0)) {
    prefix <- ''
    regex <- '\\s+([^ ]+)$'
    prefix <- gsub(regex, '\\1', regmatches(sentence, gregexpr(regex, sentence))[[1]])
    if (!empty.str(prefix)) {
      l <- nchar(sentence)
      prefL <- nchar(prefix)
      sentence <- substr(sentence, 1, l - prefL)
      list(sentence = sentence, prefix = prefix)
    } else {
      list(sentence = '', prefix = sentence)
    }
  } else {
    list(sentence = sentence, prefix = '')
  }  
}

expand.macros <- function(model, acc, prefixKeepCase) {
  retVal <- data.table(ngram=integer(), token = character(), logProb = double())
  retVal <- rbind(retVal, acc[!grepl('^#', acc$token)])
  macro <- acc[token == '#location']
  if (nrow(macro) > 0) {
    tmp <- model$locations[startsWith(tolower(model$locations$name), tolower(prefixKeepCase))]
    tmp <- data.table(ngram = rep(macro[1]$ngram, nrow(tmp)),
                      token = tmp$name, logProb = tmp$logProb + rep(macro[1]$logProb, nrow(tmp)))
    retVal <- rbind(retVal, tmp)
  }
  macro <- acc[token == '#date']
  if (nrow(macro) > 0) {
    tmp <- model$dates[startsWith(tolower(model$dates$name), tolower(prefixKeepCase))]
    tmp <- data.table(ngram = rep(macro[1]$ngram, nrow(tmp)),
                      token = tmp$name, logProb = tmp$logProb + rep(macro[1]$logProb, nrow(tmp)))
    retVal <- rbind(retVal, tmp)
  }
  macro <- acc[token == '#time']
  if (nrow(macro) > 0) {
    tmp <- model$times[startsWith(tolower(model$times$name), tolower(prefixKeepCase))]
    tmp <- data.table(ngram = rep(macro[1]$ngram, nrow(tmp)),
                      token = tmp$name, logProb = tmp$logProb + rep(macro[1]$logProb, nrow(tmp)))
    retVal <- rbind(retVal, tmp)
  }
  macro <- acc[token == '#organization']
  if (nrow(macro) > 0) {
    tmp <- model$organizations[startsWith(tolower(model$organizations$name), tolower(prefixKeepCase))]
    tmp <- data.table(ngram = rep(macro[1]$ngram, nrow(tmp)),
                      token = tmp$name, logProb = tmp$logProb + rep(macro[1]$logProb, nrow(tmp)))
    retVal <- rbind(retVal, tmp)
  }
  retVal
}

tokenize.sentence <- function(sentence) {
  sentence <- tolower(sentence)
  sentence <- stri_replace_all_fixed(sentence,
                                     c('\U201c', '\U201d', '\U2019', "`"),
                                     c('"', '"', "'", "'"),
                                     vectorize_all = FALSE)
  
  sentence <- stri_replace_all_fixed(sentence, c(":", "%", "$", "'s", "n't", "'ve",
                                                 "'re", "'m", "'ll", "'d", "o'"), 
                                     c(" abca668bd918a519226db7fa0ea0da01cff015cf ",
                                       " 258e79facea2fd35bd92f9da3d922f1852b74190 ",
                                       " d4f00bc54048c3281213673ef4b30d4a4afcb6b3 ",
                                       "33154e7b61c3afc053755ea8ed9f525cf3f5d76f",
                                       "663ea678f3cb6551bab5d31cf5df2c647bfaebb9",
                                       "e26645e51393af9c2313f6eab1c1c4209bafac74",
                                       "3a3dedae3056af0061b0e11e8d849e9c74fc77ac",
                                       "b3bd8d46bdda868915bd0790523f7cd288380992",
                                       "ba3b37020a2aa7af50656277667a05420eba3d46",
                                       "ca15f19d8dfb82766c8090b03a62aff86cef73ee",
                                       "c56b33ea771ba103f8e2a451a85627ec83b89eb0"), vectorize_all = FALSE)
  sentence <- gsub(pattern='[[:punct:]]', sentence, replacement=' ')
  tokens <- unlist(strsplit(sentence, "\\s+"))
  tokens <- sapply(tokens, function(token) {
    token <- stri_replace_all_fixed(token, c('abca668bd918a519226db7fa0ea0da01cff015cf',
                                             '258e79facea2fd35bd92f9da3d922f1852b74190',
                                             'd4f00bc54048c3281213673ef4b30d4a4afcb6b3',
                                             '33154e7b61c3afc053755ea8ed9f525cf3f5d76f',
                                             '663ea678f3cb6551bab5d31cf5df2c647bfaebb9',
                                             'e26645e51393af9c2313f6eab1c1c4209bafac74',
                                             '3a3dedae3056af0061b0e11e8d849e9c74fc77ac',
                                             'b3bd8d46bdda868915bd0790523f7cd288380992',
                                             'ba3b37020a2aa7af50656277667a05420eba3d46',
                                             'ca15f19d8dfb82766c8090b03a62aff86cef73ee',
                                             'c56b33ea771ba103f8e2a451a85627ec83b89eb0'),
                                    c(":", "%", "$", "'s", "n't", "'ve",
                                      "'re", "'m", "'ll", "'d", "o'"), vectorize_all = FALSE)
    gsub(pattern='[[:digit:]]+', token, replacement='#number')
  })
  c('#b', unname(tokens))
}

predict.next.word <- function(model, text, num.possibilities=NULL) {
  last.sentence <- function(str) {
    if (!grepl(pattern ='\\.\\s*$', str)) {
      str <- unlist(strsplit(str, '\\.'))
      str[length(str)]
    } else {
      ''
    }
  }
  
  capitalize.personal.pronoun <- function(arg) {
    regex <- '^(i)(\'(m|ve|d|ll))?$'
    matches <- grepl(x=arg, pattern = regex)
    arg[matches] <- paste('I', substr(arg[matches], 2, nchar(arg[matches])), sep='')
    arg  
  }
  
  tmp <- get.next.word.prefix(last.sentence(text))
  sentence <- tolower(tmp$sentence)
  prefixKeepCase <- tmp$prefix
  prefix <- tolower(prefixKeepCase)
  tokens <- tokenize.sentence(sentence)
  capitalizeFirstLetter <- FALSE
  if (stri_isempty(prefixKeepCase) && length(tokens) == 1 && tokens[1] == '#b'){
    capitalizeFirstLetter <- TRUE
  }
  patLength <- min(length(tokens), model$ngramCardinality - 1) 
  pattern <- token.ix(model, tail(tokens, patLength))
  cardinalities <- -(-patLength:0)
  pattern <- matrix(rep(c(rep(NA, model$ngramCardinality - patLength - 1), pattern, NA), patLength + 1),
                    nrow = patLength + 1,
                    ncol = model$ngramCardinality,
                    byrow = TRUE,
                    dimnames = list(paste(cardinalities),
                                    c(sapply(-(-(model$ngramCardinality - 1):-1),
                                             function(n){ paste('ix', n, sep='')}), 'ngram')))
  
  pattern[, 'ngram'] <- cardinalities
  for (i in (patLength + 1):1) {
    pattern[i, 1:(model$ngramCardinality - patLength + i - 2)] <- NA
  }
  matches <- merge(model$flatNgrams, pattern)
  matches <- matches[,.(ngram = max(ngram), logProb=max(logProb)), by = ix0]
  matches <- matches[order(matches$ngram, matches$logProb, decreasing = TRUE)]
  rv <- data.table(ngram = matches$ngram, 
                   token = token.name(model, matches$ix0), 
                   logProb = matches$logProb)
  rv <- expand.macros(model, rv, prefixKeepCase)
  if (!stri_isempty(prefixKeepCase)) {
    rv$token <- paste(rep(prefixKeepCase, length(rv$token)), 
                      substr(rv$token, nchar(prefixKeepCase) + 1, nchar(rv$token)), sep='')
  } else if (capitalizeFirstLetter) {
    rv$token <- paste(toupper(substr(rv$token, 0, 1)), substr(rv$token, 2, nchar(rv$token)), sep='')
  }
  rv$token <- capitalize.personal.pronoun(rv$token)
  if (!is.null(num.possibilities)) {
    head(rv, num.possibilities)
  } else {
    rv
  }
}

apply.completion <- function(sentence, completion) {
  tmp <- get.next.word.prefix(sentence)
  if (!stri_isempty(tmp$prefix)) {
    if (startsWith(completion, tmp$prefix)) {
      l <- nchar(sentence)
      prefL <- nchar(tmp$prefix)
      sentence <- substr(sentence, 1, l - prefL)
      paste(c(sentence, completion, ' '), collapse = '')
    } else {
      paste(c(sentence, completion, ' '), collapse = '')
    }
  } else {
    paste(c(sentence, completion, ' '), collapse = '')
  }
}

num.possibilities <- 8

dict <- "blogs"
load(file = 'en_US_model_blogs_cache.bin')

gui.repr <- function(text, newDict) {
  if (is.null(dict) || dict != newDict) {
    if (newDict == "blogs") {
      load(file = 'en_US_model_blogs_cache.bin')
    } else if (newDict == "news") {
      load(file = 'en_US_model_news_cache.bin')
    } else if (newDict == "twitter") {
      load(file = 'en_US_model_twitter_cache.bin')
    }
    dict <<- newDict
  }
  tbl <- predict.next.word(m, text, num.possibilities = 100)
  tbl2 <- cbind(tbl, data.table(probs = exp(tbl$logProb)))
  tbl2$logProb <- NULL
  list(top10 = head(tbl, 10), top100 = tbl2)
}

pal2 <- brewer.pal(8,"Dark2")

function(session, input, output) {
  continuations <- eventReactive(list(input$text, input$dict), {
    gui.repr(input$text, input$dict)
  })
  
  output$continuations <- renderTable(continuations()$top10)
  output$wordcloud <- renderPlot({
    wordcloud(words = continuations()$top100$token,
              freq = continuations()$top100$probs,
              colors = pal2)
  })
}
