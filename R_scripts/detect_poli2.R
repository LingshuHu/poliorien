
## function for matching words
word_match_boolean <- function(x, dict, tokenize = FALSE) {
  if (tokenize) {
    ## this removes URLs
    x <- gsub("https?://\\S+|@\\S+", "", x)
    x <- gsub("@", "", x)
    x <- tokenizers::tokenize_words(
      x, lowercase = TRUE, strip_punct = TRUE, strip_numeric = FALSE
    )
  }
  
  l <- length(x)
  p <- FALSE
  while (l >= 1 & !p & any(!is.na(x))) { # if x has length more than one then is.na report warning
    p <- x[[l]] %in% dict
    l <- l - 1
  }
  return(p)
}

## filt out non-political tweets
poli_filter <- function(rt, dict_text, dict_mention, ...) {
  tf <- grepl(pattern = dict_text, rt$text, ignore.case = TRUE)
  for (i in seq_along(tf)) {
    if (!tf[i]) {
      tf[i] <- word_match_boolean(rt$mentions_screen_name[[i]], dict = dict_mention)
    }
  }
  return(tf)
}

## get political mentions
politician <- readRDS("data/cong_politician_tweets_2020-3-12.rds")
#politician$mention <- sapply(politician$mentions_screen_name, paste0, collapse = " ")
mw <- lapply(politician$mentions_screen_name, unlist)
mw <- unlist(mw)
mw <- unique(mw)
mw <- mw[!is.na(mw)]
sw <- unique(politician$screen_name)
mw <- c(mw, sw)
mw <- unique(mw)


## get political keywords
dfpw <- read.csv("political_keywords_vague.csv", stringsAsFactors = F)
colnames(dfpw) <- "word"
dfpw$value <- 1
dfpw$word <- gsub("^\\s+|\\s+$", "", dfpw$word)
pw <- paste0(dfpw$word, collapse = "|")


## try one data set ###
ut <- readRDS("../coronavirus/user_tweets_for_ideology/covid_users20tweets_usa_jan24-mar20_21pm-23pm_onesixth_1.rds")

ut2 <- ut[1:1000, ]

x <- grepl(pattern = pw, ut2$text, ignore.case = TRUE)

t1 <- Sys.time()
utp <- poli_filter(ut2, pw, mw)
t2 <- Sys.time()
t2 - t1

utp <- cbind(ut2[, "status_id"], utp)
## do more dataset at one time

files <- list.files(path = '../coronavirus/user_tweets_for_ideology', pattern = '*[0-9].csv', full.names = TRUE)

t1 <- Sys.time()
for (i in seq_along(files)) {
  df <- rtweet::read_twitter_csv(files[i])
  poli <- poli_filter(df, pw, mw)
  poli <- cbind(df[, "status_id"], poli)
  filename <- sub("tweets.csv", "", files[i])
  newfile <- paste0(filename, "poli.csv")
  rtweet::write_as_csv(poli, newfile)
}
t2 <- Sys.time()
t2 - t1




