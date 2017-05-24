library(DBI)
library(RSQLite)
library(data.table)

#sort( sapply(ls(),function(x){object.size(get(x))})) 

# number of words seen to precede w normalized by num of words preceding all words

token.ix <-function(model, tokens) {
  rv <- model$lookupTable[tokens]$ix
  rv[is.na(rv)] <- model$lookupTable["#unk"]$ix
  rv
}

token.name <-function(model, ix) {
  model$lookupTable[ix]$name
}

token.prior <- function(model, tokens) {
  model$n1grams[token.ix(tokens)]$logProb
}

con = dbConnect(RSQLite::SQLite(), dbname='./src_data/en_US/en_US.db')

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

initialize.new.model <- function(conn) {
  raw.tokens <- as.data.table(dbGetQuery(con, 'select w1 from n1gram order by w1'))
  lookupTable <- data.table(name = raw.tokens$w1, ix0 = 1:length(raw.tokens$w1))
  setkey(lookupTable, name)
  list(lookupTable = lookupTable)
}

build.ngram.prediction.model <- function(conn, n, d) {
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
    names(tmp) <- c('ix0', 'nContextsSeenBeforeW0')
    ngram_tmp <- merge(ngram_tmp, tmp, by='ix0')
    
    ngram_tmp <- merge(ngram_tmp, n1GramCount, by.x='ix1', by.y='ix0', suffixes = c('.2', '.1'))
    setnames(ngram_tmp, c('ngramCount.2'), c('ngramCount'))
    
    tmp <- ngram_tmp[, .N, by=ix1]
    names(tmp) <- c('ix1', 'nPossibleW0ForSamePrefix')
    ngram_tmp <- merge(ngram_tmp, tmp, by='ix1')
    
    allPossiblePrefixes <- load.sql.distinct.w1.prec.all.w2(conn)
    tmp <- ngram_tmp[,.(a = log(max(ngramCount - d, .Machine$double.xmin)) - log(ngramCount.1)    #a
                        , logLambda = log(d) + log(nPossibleW0ForSamePrefix) - log(ngramCount.1)   #lambda
                        , logPCont = log(nContextsSeenBeforeW0) - log(allPossiblePrefixes)),
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
    
    ngram_tmp <- load.ngrams.sql(conn, model, 3)
    tmp <- ngram_tmp[, .N, by = ix0]
    names(tmp) <- c('ix0', 'nContextsSeenBeforeW0')
    ngram_tmp <- merge(ngram_tmp, tmp, by = 'ix0')
    
    mergekeys.x <- sapply(-(-(n - 1):-1), function(arg) { paste('ix', arg, sep = '')})
    mergekeys.y <- sapply(-(-(n - 2):0), function(arg) { paste('ix', arg, sep = '') })
    suffixes <- sapply(c(n, n - 1), function(arg) { paste('.', arg, sep = '') })
    
    ngram_tmp <- merge(ngram_tmp, nMinus1GramCount, by.x = mergekeys.x, by.y = mergekeys.y, suffixes = suffixes)
    
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
    
    tmp <- merge(tmp, knRecursiveNGrams, by.x = mergekeys.x, by.y = mergekeys.y, suffixes = suffixes)
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
  
  model <- NULL
  for (i in 1:n) {
    print(sprintf("Build n%dgrams...", i))
    if (i == 1) {
      model <- initialize.new.model(conn)
      model <- append(model, build.n1grams(conn, model))
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

tst <- build.ngram.prediction.model(conn = con, n=3, d=.75)

#tmp <- dbGetQuery(con, 'select w1, w2, w3, count from n3gram order by w1, w2, w3')
#n3grams_tmp <- data.table(ix1 = token.ix(n3grams_tmp$w1),
#                          ix2 = token.ix(n3grams_tmp$w2),
#                          ix3 = token.ix(n3grams_tmp$w3),
#                          n3gramCount = n3grams_tmp$count)


#continuation token.name(head(result[order(result$logProb, decreasing = TRUE),], 3)$ix2)

## 46145 42978 85895
#> token.name(42978)
#[1] "have"
#> tmp[ix1==token.ix('i'), sum(count),]
#[1] 1600520
#> 85895 / 1600520
