library(data.table)
library(stringi)
library(shinyjs)
library(wordcloud)
library(RColorBrewer)

is.token.person <- function (model, token) {
  p1 <- grepl(pattern = '^[[:upper:]][[:lower:]]+(\\\'s)?', x = token) &
    tolower(gsub(pattern = '([^\\\']*)(\\\'.*)?$', replacement = '\\1', x = token)) %in% model$persons$name
  p2 <- p1 & grepl('\'s?$', token)
  list(is_pers = p1, poss = p2)
}

token.ix <- function(model, tokens, generify.persons = FALSE) {
  rv <- model$lookupTable[tolower(tokens)]$ix
  rv[is.na(rv)] <- model$lookupTable["#unk"]$ix
  if (generify.persons) {
    personToks <- is.token.person(model, tokens)
    possPersonIx <- model$lookupTable["#person's"]$ix
    nonPossPersonIx <- model$lookupTable["#person"]$ix
    rv[personToks$is_pers] <- nonPossPersonIx
    rv[personToks$poss] <- possPersonIx
    indicesToCollapse <- c(possPersonIx, nonPossPersonIx)
    acc <- NULL
    for (i in 1:length(rv)) {
      ix <- rv[i]
      if (!is.null(acc) & ix %in% indicesToCollapse) {
        prevIx <- tail(acc, 1)
        if (prevIx %in% indicesToCollapse) {
          acc <- append(head(acc, length(acc) - 1), ix)
        } else {
          acc <- append(acc, ix)
        }
      } else {
        acc <- append(acc, ix)
      }
    }
    rv <- unlist(acc)
  }
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

expand.macros <- function(model, acc, prefix) {
  retVal <- data.table(ngram=integer(), token = character(), logProb = double())
  retVal <- rbind(retVal, acc[!grepl('^((the|a|an)\\s)?#', acc$token)])
  macro <- acc[token == '#location']
  if (nrow(macro) > 0) {
    tmp <- model$locations[startsWith(tolower(model$locations$name), tolower(prefix))]
    tmp <- data.table(ngram = rep(macro[1]$ngram, nrow(tmp)),
                      token = tmp$name, logProb = tmp$logProb + rep(macro[1]$logProb, nrow(tmp)))
    retVal <- rbind(retVal, tmp)
  }
  macro <- acc[token == '#date']
  if (nrow(macro) > 0) {
    tmp <- model$dates[startsWith(tolower(model$dates$name), tolower(prefix))]
    tmp <- data.table(ngram = rep(macro[1]$ngram, nrow(tmp)),
                      token = tmp$name, logProb = tmp$logProb + rep(macro[1]$logProb, nrow(tmp)))
    retVal <- rbind(retVal, tmp)
  }
  macro <- acc[token == '#time']
  if (nrow(macro) > 0) {
    tmp <- model$times[startsWith(tolower(model$times$name), tolower(prefix))]
    tmp <- data.table(ngram = rep(macro[1]$ngram, nrow(tmp)),
                      token = tmp$name, logProb = tmp$logProb + rep(macro[1]$logProb, nrow(tmp)))
    retVal <- rbind(retVal, tmp)
  }
  macro <- acc[token == '#organization']
  if (nrow(macro) > 0) {
    tmp <- model$organizations[startsWith(tolower(model$organizations$name), tolower(prefix))]
    tmp <- data.table(ngram = rep(macro[1]$ngram, nrow(tmp)),
                      token = tmp$name, logProb = tmp$logProb + rep(macro[1]$logProb, nrow(tmp)))
    retVal <- rbind(retVal, tmp)
  }
  retVal
}

tokenize.sentence <- function(sentence) {
  sentence <- stri_replace_all_fixed(sentence,
                                     c('\U201c', '\U201d', '\U2019', "`"),
                                     c('"', '"', "'", "'"),
                                     vectorize_all = FALSE)
  
  sentence <- stri_replace_all_regex(sentence,
                                     "([:upper:][:lower:]+(s|z))'",
                                     "$14c5265eabb095522afe529a944ffa51cc26510bd")
  
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
                                             'c56b33ea771ba103f8e2a451a85627ec83b89eb0',
                                             '4c5265eabb095522afe529a944ffa51cc26510bd'),
                                    c(":", "%", "$", "'s", "n't", "'ve",
                                      "'re", "'m", "'ll", "'d", "o'", "'"), vectorize_all = FALSE)
    gsub(pattern='[[:digit:]]+', token, replacement='#number')
  })
  c('#b', unname(tokens))
}

predict.next.word <- function(model, text, num.possibilities=NULL, propagate.articles=FALSE) {
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
  
  make.pattern.matrix <- function(tokens) {
    indices <- token.ix(model, tokens, generify.persons = TRUE)
    patLength <- min(length(indices), model$ngramCardinality - 1) 
    pattern <- tail(indices, patLength)
    pattern <- matrix(rep(pattern, patLength), nrow = patLength, ncol = patLength, byrow = TRUE)
    pattern[lower.tri(pattern)] <- NA
    pattern[, 1:patLength] <- pattern[, patLength:1]
    pattern <- rbind(pattern, matrix(ncol = patLength, nrow = 1))
    pattern <- cbind(pattern,
                     matrix(ncol = model$ngramCardinality - patLength - 1, nrow = patLength + 1),
                     as.integer(1 + -(-patLength:0)))
    matchCols <- sapply(1:(model$ngramCardinality - 1), function(x) {paste('ix', x, sep = '')})
    colnames(pattern) <- c(matchCols, 'ngram')
    pattern
  }
  
  propagate.article.matches <- function(articleMatches) {
    pattern <- matrix(articleMatches$ix0, ncol=1)
    for (i in 1:(m$ngramCardinality - 2)) {
      pattern <- cbind(pattern, articleMatches[[paste('ix', i, sep='')]])
    }
    pattern <- cbind(pattern, articleMatches$ngram, articleMatches$logProb)
    matchCols <- sapply(1:(model$ngramCardinality - 1), function(x) {paste('ix', x, sep = '')})
    colnames(pattern) <- c(matchCols, 'ngram', 'logProb.L0')
    matches <- merge(model$flatNgrams[ix1 %in% pattern[,1],], pattern)
    data.table(ngram = matches$ngram, 
               token = paste(token.name(model, matches$ix1), token.name(model, matches$ix0)),
               logProb = matches$logProb + matches$logProb.L0)
  }
  
  sentence.pref <- get.next.word.prefix(last.sentence(text))
  tokens <- tokenize.sentence(sentence.pref$sentence)
  prefix <- sentence.pref$prefix
  
  capitalizeFirstLetter <- FALSE
  if (stri_isempty(prefix) && length(tokens) == 1 && tokens[1] == '#b'){
    capitalizeFirstLetter <- TRUE
  }
  
  pattern <- make.pattern.matrix(tokens)
  matches <- merge(model$flatNgrams[ix1 == pattern[1, 1] | is.na(ix1),], pattern)
  if (propagate.articles) {
    articles <- token.ix(model, c('the', 'a', 'an'))
    articleMatches <- matches[ix0 %in% articles,]
    matches <- matches[!(ix0 %in% articles),]
    rv <- rbind(data.table(ngram = matches$ngram, 
                           token = token.name(model, matches$ix0), 
                           logProb = matches$logProb),
                propagate.article.matches(articleMatches))
  } else {
    rv <- data.table(ngram = matches$ngram, 
                     token = token.name(model, matches$ix0), 
                     logProb = matches$logProb)
  }
  rv <- rv[,.(logProb=max(logProb), ngram = max(ngram)), by = token]
  rv <- rv[order(rv$logProb, rv$ngram, decreasing = TRUE)]
  rv <- expand.macros(model, rv, prefix)
  if (!stri_isempty(prefix)) {
    rv$token <- paste(rep(prefix, length(rv$token)), 
                      substr(rv$token, nchar(prefix) + 1, nchar(rv$token)), sep='')
  } else if (capitalizeFirstLetter) {
    rv$token <- paste(toupper(substr(rv$token, 0, 1)), substr(rv$token, 2, nchar(rv$token)), sep='')
  }
  rv$token <- capitalize.personal.pronoun(rv$token)
  rv$ngram <- as.integer(rv$ngram)
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
loaded.dict <- NULL

ensure.data.loaded <- function () {
  if (!(sum(loaded.dict == dict) > 0)) {
    show("loading_page")
    hide("main_content")
    if (dict == "blogs") {
      load(envir = .GlobalEnv, file = 'en_US_model_blogs_cache.bin')
    } else if (dict == "news") {
      load(envir = .GlobalEnv, file = 'en_US_model_news_cache.bin')
    } else if (dict == "twitter") {
      load(envir = .GlobalEnv, file = 'en_US_model_twitter_cache.bin')
    }
    loaded.dict <<- dict
  }
  hide("loading_page")
  show("main_content")
}

gui.repr <- function(text) {
  tbl <- predict.next.word(m, text, num.possibilities = 100, propagate.articles = TRUE)
  tbl2 <- cbind(tbl, data.table(probs = exp(tbl$logProb)))
  tbl2$logProb <- NULL
  list(top10 = head(tbl, 10), top100 = tbl2)
}

pal2 <- brewer.pal(8,"Dark2")

function(session, input, output) {
  ensure.data.loaded()
  
  continuations <- eventReactive(list(input$text, input$dict), {
    dict <<- input$dict
    ensure.data.loaded()
    gui.repr(input$text)
  })
  
  output$continuations <- renderTable(continuations()$top10)
  output$wordcloud <- renderPlot({
    wordcloud(words = continuations()$top100$token,
              freq = continuations()$top100$probs,
              colors = pal2)
  })
}
