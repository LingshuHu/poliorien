
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
  
  i <- length(x)
  p <- FALSE
  while (i >= 1 & !p) {
    p <- x[i] %in% dict
    i <- i - 1
  }
  return(p)
}

## filt out non-political tweets
poli_filter <- function(text, mention, dict_text, dict_mention, ...) {
  tf <- grepl(pattern = dict_text, text, ignore.case = TRUE)
  for (i in seq_along(tf)) {
    if (!tf[i]) {
      tf[i] <- word_match_boolean(mention[i], dict = dict_mention)
    }
  }
  return(tf)
}

t1 <- Sys.time()
utmt2 <- lapply(ut$text, function(x) word_match_boolean(x, mt, tokenize = T))
t2 <- Sys.time()
t2 - t1

utmt2.1 <- unlist(utmt2)
table(utmt2.1)

politician <- readRDS("data/cong_politician_tweets_2020-3-12.rds")

politician$mention <- sapply(politician$mentions_screen_name, paste0, collapse = " ")

cong <- read.csv("data/cong116_politician_twitter.csv", stringsAsFactors = F)

politician$user_id <- as.numeric(politician$user_id)

politician <- dplyr::left_join(politician, 
                               cong[!duplicated(cong$user_id), c("user_id", "party")], 
                               by = "user_id")

p <- politician[is.na(politician$party), ]

p <- dplyr::left_join(p[, 1:5], 
                      cong[!duplicated(cong$user_id), c("screen_name", "party")], 
                      by = "screen_name")

nrow(p[is.na(p$party), ])

politician <- politician[!is.na(politician$party), ]

politician <- rbind(politician, p)

rtweet::write_as_csv(politician[, c("user_id","status_id","screen_name","text","mention", "party")], 
                     "data/cong_politician_tweets_2020-3-12_text.csv")

politician <- rtweet::read_twitter_csv("data/cong_politician_tweets_2020-3-12_text.csv")

user_tweets2 <- user_tweets[, c("user_id","status_id","screen_name","text","mentions_screen_name")]
ut <- readRDS("../coronavirus/data/users/covid_users20tweets_usa_jan24-mar20_21pm-23pm_onesixth_1.rds")

str(ut$mentions_screen_name[1])

ut$mention <- paste0(ut$mentions_screen_name, collapse = " ")

ut$mention <- sapply(ut$mentions_screen_name, paste0, collapse = " ")
str(ut$mention)
ut$mention[1:50]

dfpw <- read.csv("political_keywords_vague.csv", stringsAsFactors = F)
colnames(dfpw) <- "word"
dfpw$value <- 1
dfpw$word <- gsub("^\\s+|\\s+$", "", dfpw$word)
pw <- paste0(pw$word, collapse = "|")

x <- word_match(ut$text, dfpw)
x2 <- word_match2(ut$mentions_screen_name, dfmt)


dict_words_count2 <- x2$dict_words_count
nrow(subset(x2, dict_words_count > 0))

ut <- cbind(ut, dict_words_count2)

ut3 <- subset(ut, dict_words_count2 > 0)

pt <- user_tweets[grepl(pattern = pw, user_tweets$text, ignore.case = TRUE), ]

pt <- ut[grepl(pattern = pw, ut$text, ignore.case = TRUE), ]

pt_id <- c(pt$status_id, ut3$status_id)

length(unique(pt_id))
length(unique(pt$user_id))

npt <- dplyr::anti_join(user_tweets, pt, by = "status_id")


user_tweets[user_tweets$screen_name == "DanWasta", ]

library(quanteda)
x <- dim(user_tweets$text)

mt <- unique(unlist(user_tweets$mentions_screen_name))
mt <- mt[!is.na(mt)]

dfmt <- data.frame(word = mt, value = rep.int(1, length(mt)))
