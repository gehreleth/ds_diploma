library(data.table)
library(stringi)

load(file = 'en_US_model_cache.bin')

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
  
  match.nxgrams <- function(model, ngramIndices, prefix) {
    ngrams <- model[[paste('n', i, 'grams', sep='')]]
    l <- length(ngramIndices)
    pattern <- data.table(t(ngramIndices[(n-i+1):l]))
    mergepattern = sapply(-(-(i - 1):-1), function(n){ paste('ix', n, sep='')})
    names(pattern) <- mergepattern
    matches <- merge(ngrams, pattern, by = mergepattern)
    prefixMatches <- model$lookupTable[startsWith(model$lookupTable$name, prefix)]$ix0
    macros <- model$lookupTable[startsWith(model$lookupTable$name, '#')]$ix0
    matches <- matches[ix0 %in% c(prefixMatches, macros)]
    data.table(ngram = i*rep(1, nrow(matches)), token = token.name(model, matches$ix0), logProb = matches$logProb)
  }
  
  match.n1grams <- function(model, prefix, num.possibilities) {
    if (!stri_isempty(prefix)) {
      f <- model$lookupTable[startsWith(model$lookupTable$name, prefix)]
    } else {
      f <- model$lookupTable
    }
    f <- f[!(name %in% acc$token)]
    matches <- model$n1grams[ix0 %in% f$ix0]
    matches <- head(matches[order(matches$logProb, decreasing = TRUE)], max(64, num.possibilities))
    data.table(ngram = rep(1, nrow(matches)), token = token.name(model, matches$ix0), logProb = matches$logProb)
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
  ngramIndices <- token.ix(model, tokens[max(1, (length(tokens) - model$ngramCardinality + 2)):length(tokens)])
  acc <- data.table(ngram=integer(), token = character(), logProb = double())
  n <- min(model$ngramCardinality, length(ngramIndices) + 1)
  for (i in n:1) {
    if (i > 1) {
      matches <- match.nxgrams(model, ngramIndices, prefix)
      acc <- rbind(acc, matches[!(token %in% acc$token),])
    } else {
      matches <- match.n1grams(model, prefix, num.possibilities)
      acc <- rbind(acc, matches[!(token %in% acc$token),])
    }
    acc <- expand.macros(model, acc, prefixKeepCase)
    if (!is.null(num.possibilities) && nrow(acc) >= num.possibilities) {
      break
    }
  }
  setkey(acc, 'token')
  if (!is.null(num.possibilities)) {
    rv <- head(acc[order(acc$ngram, acc$logProb, decreasing = TRUE)], num.possibilities)
  } else {
    rv <- acc[order(acc$ngram, acc$logProb, decreasing = TRUE)]
  }
  if (!stri_isempty(prefixKeepCase)) {
    rv$token <- paste(rep(prefixKeepCase, length(rv$token)), 
                      substr(rv$token, nchar(prefixKeepCase) + 1, nchar(rv$token)), sep='')
  } else if (capitalizeFirstLetter) {
    rv$token <- paste(toupper(substr(rv$token, 0, 1)), substr(rv$token, 2, nchar(rv$token)), sep='')
  }
  rv$token <- capitalize.personal.pronoun(rv$token)
  rv
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

function(session, input, output) {
  completions <- reactive({predict.next.word(m, input$text, num.possibilities = num.possibilities)$token })
  output$completions <- renderUI({
    completions <- completions()
    if (length(completions) > 0) {
      buttonCount <- min(num.possibilities, length(completions))
      mapply(function(arg) {
        tagList(actionButton(paste("completion", arg, sep=''), width = 100, label = { completions[arg] }))
      }, arg = 1:buttonCount)
    }
  })
  
  mapply(function(arg) {
    observeEvent(input[[paste("completion", arg, sep='')]], {
      updateTextInput(session, "text", value=apply.completion(input$text, completions()[arg]))
    })
  }, arg = 1:num.possibilities)
}