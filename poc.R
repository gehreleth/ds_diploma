library(DBI)
library(RSQLite)
library(data.table)

#sort( sapply(ls(),function(x){object.size(get(x))})) 

# number of words seen to precede w normalized by num of words preceding all words

token.ix <-function(tokens) {
  rv <- lookupTable[tokens]$ix
  rv[is.na(rv)] <- lookupTable["#unk"]$ix
  rv
}

token.name <-function(ix) {
  lookupTable[ix]$name
}

token.prior <- function(tokens) {
  n1grams[token.ix(tokens)]$logProb
}

n2gram.prob <- function(w0, w1) {
  n2grams[ix1 == token.ix(w0) & ix2 %in% token.ix(w1),]$logProb
}

con = dbConnect(RSQLite::SQLite(), dbname="./src_data/en_US/en_US.db")

n1grams_tmp <- as.data.table(dbGetQuery(con, 'select w1, count from n1gram order by w1'))
lookupTable <- data.table(name = n1grams_tmp$w1, ix1 = 1:length(n1grams_tmp$w1))
setkey(lookupTable, name)
n1grams_tmp <- data.table(ix1 = 1:length(n1grams_tmp$w1), w1count = n1grams_tmp$count)
setkey(n1grams_tmp, ix1)

n2grams_tmp <- dbGetQuery(con, 'select w1, w2, count from n2gram order by w1, w2')
n2grams_tmp <- data.table(ix1 = token.ix(n2grams_tmp$w1),
                          ix2 = token.ix(n2grams_tmp$w2),
                          bigramCount = n2grams_tmp$count)
tmp <- n2grams_tmp[, .N, by=ix2]
names(tmp) <- c('ix2', "distinctUnigramsPrecW2Count")
n2grams_tmp <- merge(n2grams_tmp, tmp, by='ix2')
tmp <- n1grams_tmp
names(tmp) <- c('ix1', "w1UnigramCount")
n2grams_tmp <- merge(n2grams_tmp, tmp, by='ix1')
discinctW1PrecAllW2 <- length(unique(n2grams$ix1))
tmp <- n2grams_tmp[, .N, by=ix1]
names(tmp) <- c('ix1', "distinctUnigramsSeenAfterW1")
n2grams_tmp <- merge(n2grams_tmp, tmp, by='ix1')
d = .75
tmp <- n2grams_tmp[,.(a = log(max(bigramCount - d, .Machine$double.xmin)) - log(w1UnigramCount)#a
                      , logLambda = log(d) + log(distinctUnigramsSeenAfterW1) - log(w1UnigramCount) #lambda
                      , logPCont = log(distinctUnigramsPrecW2Count) - log(discinctW1PrecAllW2)),
                   by = c('ix1', 'ix2')]
n2grams <- tmp[, .(logProb = a + log1p(exp(logLambda + logPCont - a))), by = c('ix1', 'ix2')]

n3grams_tmp <- dbGetQuery(con, 'select w1, w2, w3, count from n3gram order by w1, w2, w3')
n3grams_tmp <- data.table(ix1 = token.ix(n3grams_tmp$w1),
                          ix2 = token.ix(n3grams_tmp$w2),
                          ix3 = token.ix(n3grams_tmp$w3),
                          n3gramCount = n3grams_tmp$count)
remove(tmp)

build.ngrams <- function(conn, n, d) {
  load.sql <- function(conn, n) {
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
    tmp <- NULL
    for (i in 1:n) {
      if (i == 1) {
        tmp <- data.frame(ix1 = token.ix(rs[, 'w1']))
      } else {
        col <- data.frame(token.ix(rs[, paste('w', i, sep = '')]))
        names(col)[1] <- paste('ix', i, sep = '')
        tmp <- cbind(tmp, col)
      }
    }
    tmp <- cbind(tmp, data.frame(count = rs[, 'count']))
    as.data.table(tmp)
  }
  ngram_tmp <- load.sql(conn, n)
  wnColName <- paste('ix', n, sep='')
  tmp <- ngram_tmp[, .N, by=wnColName]
  names(tmp) <- c(wnColName, 'distinctWMinus1GramsPrecWNCount')
  ngram_tmp <- merge(ngram_tmp, tmp, by=wnColName)
}

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
