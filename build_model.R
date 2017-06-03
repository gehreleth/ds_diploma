library(DBI)
library(RSQLite)
library(data.table)
require(stringi)

#sort( sapply(ls(),function(x){object.size(get(x))})) 

token.ix <-function(model, tokens) {
  rv <- model$lookupTable[tokens]$ix
  rv[is.na(rv)] <- model$lookupTable["#unk"]$ix
  rv
}

token.name <- function(model, ix) {
  model$lookupTable[ix]$name
}

token.prior <- function(model, tokens) {
  model$n1grams[token.ix(tokens)]$logProb
}

build.ngram.prediction.model <- function(conn, d) {
  load.ngrams.sql <- function(conn, model, n) {
    sqlCollList <-
      paste(sapply(1:n, function(arg) {
        paste('w', arg, sep = '')
      }), collapse = ', ')
    sqlQuery <-
      sprintf('select %s, count from n%dgram order by %s',
              sqlCollList,
              n,
              sqlCollList)
    rs <- dbGetQuery(con, sqlQuery)
    acc <- NULL
    for (i in 1:n) {
      srcColName <- paste('w', i, sep = '')
      destColName <- paste('ix', n - i, sep = '')
      col <- data.frame(token.ix(model, rs[, srcColName]))
      names(col)[1] <- destColName
      if (i > 1) {
        acc <- cbind(acc, col)
      } else {
        acc <- col
      }
    }
    acc <- cbind(acc, data.frame(ngramCount = rs[, 'count']))
    as.data.table(acc)
  }
  
  load.sql.distinct.w1.prec.all.w2 <- function(conn) {
    rs <- dbGetQuery(con, 'select count(distinct w1) as count from n2gram')
    head(rs[,"count"], 1)
  }
  
  build.locations <- function(conn, model) {
    locations <- as.data.table(dbGetQuery(con, 'select name, count from location'))
    denom <- locations[, log(sum(count))]
    locations$logProb <- log(locations$count) - denom
    locations$count <- NULL
    locations <- locations[ logProb > log(.0001)]
    setkey(locations, name)
    list(locations = locations)
  }
  
  build.dates <- function(conn, model) {
    dates <- as.data.table(dbGetQuery(con, 'select name, count from date'))
    denom <- dates[, log(sum(count))]
    dates$logProb <- log(dates$count) - denom
    dates$count <- NULL
    dates <- dates[ logProb > log(.001)]
    setkey(dates, name)
    list(dates = dates)
  }
  
  build.times <- function(conn, model) {
    tmp <- as.data.table(dbGetQuery(con, 'select name, count from time'))
    denom <- tmp[, log(sum(count))]
    tmp$logProb <- log(tmp$count) - denom
    tmp$count <- NULL
    tmp <- tmp[ logProb > log(.001)]
    setkey(tmp, name)
    list(times = tmp)
  }
  
  build.organizations <- function(conn, model) {
    tmp <- as.data.table(dbGetQuery(con, 'select name, count from organization'))
    denom <- tmp[, log(sum(count))]
    tmp$logProb <- log(tmp$count) - denom
    tmp$count <- NULL
    tmp <- tmp[ logProb > log(.001)]
    setkey(tmp, name)
    list(organizations = tmp)
  }

  initialize.new.model <- function(conn, ngramCardinality) {
    raw.tokens <- as.data.table(dbGetQuery(con, 'select w1 from n1gram order by w1'))
    lookupTable <- data.table(name = raw.tokens$w1, ix0 = 1:length(raw.tokens$w1))
    setkey(lookupTable, name)
    list(lookupTable = lookupTable, ngramCardinality = ngramCardinality)
  }
  
  build.n1grams <- function(conn, model) {
    ngram_tmp <- load.ngrams.sql(conn, model, 1)
    unigramCount <- sum(ngram_tmp$ngramCount)
    n1grams <- data.table(ix0 = ngram_tmp$ix0, logProb = log(ngram_tmp$ngramCount) - log(unigramCount))
    list(n1GramCount = ngram_tmp, n1grams = n1grams)
  }
  
  build.n2grams <- function(conn, model) {
    n1GramCount <- model$n1GramCount
    ngram_tmp <- load.ngrams.sql(conn, model, 2)
    
    tmp <- ngram_tmp[, .N, by=ix0]
    names(tmp) <- c('ix0', 'nUnigramsSeenBeforeW0')
    ngram_tmp <- merge(ngram_tmp, tmp, by='ix0')
    
    ngram_tmp <- merge(ngram_tmp, n1GramCount, by.x='ix1', by.y='ix0', suffixes = c('.2', '.1'))
    setnames(ngram_tmp, c('ngramCount.2'), c('ngramCount'))
    
    tmp <- ngram_tmp[, .N, by=ix1]
    names(tmp) <- c('ix1', 'nPossibleW0ForSamePrefix')
    ngram_tmp <- merge(ngram_tmp, tmp, by='ix1')
    
    allPossiblePrefixUnigrams <- load.sql.distinct.w1.prec.all.w2(conn)
    tmp <- ngram_tmp[,.(a = log(max(ngramCount - d, .Machine$double.xmin)) - log(ngramCount.1)    #a
                        , logLambda = log(d) + log(nPossibleW0ForSamePrefix) - log(ngramCount.1)   #lambda
                        , logPCont = log(nUnigramsSeenBeforeW0) - log(allPossiblePrefixUnigrams)),
                     by = c('ix1', 'ix0')]
    n2grams <- tmp[, .(logProb = a + log1p(exp(logLambda + logPCont - a))), by = c('ix1', 'ix0')]
    
    tmp_cols <- names(ngram_tmp)
    tmp_cols <- tmp_cols[!tmp_cols %in% c('ix0', 'ix1', 'ngramCount')]
    ngram_tmp[,(tmp_cols) := NULL]
    
    list(n2GramCount = ngram_tmp, n2grams = n2grams)
  }
  
  build.nxgrams <- function(conn, model, n) {
    knRecursiveNGrams <- model[[paste('n', n - 1, 'grams', sep = '')]]
    nMinus1GramCount <- model[[paste('n', n - 1, 'GramCount', sep = '')]]
    
    ngram_tmp <- load.ngrams.sql(conn, model, n)
    
    mergekeys.x <- sapply(-(-(n - 1):-1), function(arg) { paste('ix', arg, sep = '')})
    mergekeys.y <- sapply(-(-(n - 2):0), function(arg) { paste('ix', arg, sep = '') })
    suffixes <- sapply(c(n, n - 1), function(arg) { paste('.', arg, sep = '') })
    
    ngram_tmp <- merge(ngram_tmp, nMinus1GramCount, by.x = mergekeys.x, by.y = mergekeys.y, suffixes = suffixes, allow.cartesian=TRUE)
    
    setnames(ngram_tmp, 
             c(paste('ngramCount.', n, sep = ''),
               paste('ngramCount.', n - 1, sep = '')), 
             c('ngramCount', 'ngramCount.lower'))
    
    tmp <- ngram_tmp[, .N, by = mergekeys.x]
    names(tmp) <- c(mergekeys.x, 'nPossibleW0ForSamePrefix')
    ngram_tmp <- merge(ngram_tmp, tmp, by = mergekeys.x)
    
    aggkeys <- sapply(-(-(n - 1):0), function(arg) { paste('ix', arg, sep = '') })
    tmp <- ngram_tmp[, .(a = log(max(ngramCount - d, .Machine$double.xmin)) - log(ngramCount.lower),
                         logLambda = log(d) + log(nPossibleW0ForSamePrefix) - log(ngramCount.lower)),
                     by = aggkeys]
    
    tmp <- merge(tmp, knRecursiveNGrams, by.x = mergekeys.x, by.y = mergekeys.y, suffixes = suffixes, allow.cartesian=TRUE)
    setnames(tmp, c('logProb'), c('logPCont'))
    
    nxgrams <- tmp[, .(logProb = a + log1p(exp(logLambda + logPCont - a))), by = aggkeys]
    
    tmp_cols <- names(ngram_tmp)
    tmp_cols <- tmp_cols[!tmp_cols %in% c(aggkeys, 'ngramCount')]
    ngram_tmp[,(tmp_cols) := NULL]
    
    rv <- list()
    rv[[paste('n', n, 'grams', sep = '')]] <- nxgrams
    rv[[paste('n', n, 'GramCount', sep = '')]] <- ngram_tmp
    rv
  }
  
  tableList <- dbListTables(con)
  tableList <- tableList[grep('^n(\\d+)gram$', tableList)]
  n <- max(as.integer(gsub('^n(\\d+)gram$',"\\1", tableList)))
  remove(tableList)
  
  model <- NULL
  for (i in 1:n) {
    print(sprintf("Build n%dgrams...", i))
    if (i == 1) {
      print("Create blank model ...")
      model <- initialize.new.model(conn, n)
      model <- append(model, build.n1grams(conn, model))
      print("Build location table ...")
      model <- append(model, build.locations(conn, model))
      print("Build date table ...")
      model <- append(model, build.dates(conn, model))
      print("Build time table ...")
      model <- append(model, build.times(conn, model))
      print("Build organization table ...")
      model <- append(model, build.organizations(conn, model))
    } else if (i == 2) {
      model <- append(model, build.n2grams(conn, model))
      model[['n1GramCount']] <- NULL
    } else {
      model <- append(model, build.nxgrams(conn, model, n=i))
      tempTable <- paste('n', i - 1, 'GramCount', sep = '')
      model[[tempTable]] <- NULL
    }
  }
  tempTable <- paste('n', n, 'GramCount', sep = '')
  model[[tempTable]] <- NULL
  model
}

if (!exists("m")) {
  if(file.exists('./src_data/en_US/en_US_model_cache.bin')){
    load(file = './src_data/en_US/en_US_model_cache.bin')
  } else { # Doing this the hard way...
    con <- dbConnect(RSQLite::SQLite(), dbname='./src_data/en_US/en_US.db')
    m <- build.ngram.prediction.model(conn = con, d=.75)
    dbDisconnect(con)
    remove(con)
    save(m, file='./src_data/en_US/en_US_model_cache.bin')
  }
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

calculate.probs <- function(model, sentence, variants) {
  continuations <- predict.next.word(model, sentence)
  continuations[token %in% variants]
}
