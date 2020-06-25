
#install.packages("rtweet")
#update.packages("rtweet")
library(rtweet)
packageVersion("rtweet")
#install.packages("ggplot2")
library(ggplot2)
#install.packages("rvest")
library(rvest)
#install.packages("tidyverse")
library(tidyverse)

#############################################################
##### use this 

df <- read.csv("data/cong116_politician_party.csv")
df$user_id <- as.character(df$user_id)

df2 <- df[!duplicated(df$user_id), ]
df2[df2$user_id == "168673083", ]

df[df$user_id == "168673083", ]

dupl <- user_tweets3[user_tweets3$user_id == "168673083", ]
table(duplicated(dupl$screen_name))

## get their tweets


urs <- unique(rt2$user_id)
saveRDS(urs, "data/covid_usa_jan24-mar20_21pm-23pm_onesixth_userid.rds")
urs <- readRDS("data/covid_usa_jan24-mar20_21pm-23pm_onesixth_userid.rds")

saveRDS(urs2, "data/covid_usa_jan24-mar20_21pm-23pm_twosixth_userid.rds")
urs2 <- readRDS("data/covid_usa_jan24-mar20_21pm-23pm_twosixth_userid.rds")

wait_search <- function(s, t, inu = NULL, tm = 60*14, uids, n) {
  token <- get_tokens()
  lst <- vector("list") # empty list
  
  if(n <= 200) { # calculate number of quires
    rq = 1
  } else {rq = ceiling(3200/n)}
  
  if(is.null(inu)) { # max number of users can get per search
    inu <- floor((1500)/rq)
  }
  
  gt <- function(st, en) {
    tryCatch(
      rt <- rtweet::get_timeline(uids[st:en], 
                                 n = n, 
                                 token = rtweet::bearer_token(),
                                 check = F),
      error = function(e) NULL
    )
  }
  
  i = 1
  while(s < t) {
    #rtlimit <- rate_limit(token, "search/tweets")
    #remaining <- rtlimit[["remaining"]] * 100
    #reset <- rtlimit[["reset"]]
    #reset <- as.numeric(reset, "secs")
    e <- s + inu
    if(e > t) {
      e <- t
    } 
    lst[[i]] <- gt(st = s, en = e)
    s <- which(uids == lst[[i]]$user_id[nrow(lst[[i]])]) + 1
    i <- i + 1
    if(s < t) {
      message(paste0("waiting about ", tm/60, " minutes"))
      Sys.sleep(tm)
    }
  }
  #lst <- do.call("rbind", lst)
  return(lst)
}

library(lubridate)
user_tweets <- wait_search(s = 1, t = 1040, tm = 60*10, uids = df2$user_id, n = 1000)

user_tweets3 <- subset(user_tweets2, !(status_id %in% user_tweets$status_id))

user_tweets2 <- do.call("rbind", user_tweets)

user_tweets <- rtweet::get_timeline(df$user_id[2], n = 1000)

user_tweets <- rbind(user_tweets, user_tweets2)
user_tweets3 <- user_tweets[!duplicated(user_tweets$status_id), ]

user_tweets4 <- dplyr::left_join(user_tweets3, df2[, -3], by = "user_id")

saveRDS(user_tweets3, "data/cong_politician_tweets_2020-4-16.rds")

user_tweets <- readRDS("data/cong_politician_tweets_2020-3-12.rds")

rtweet::write_as_csv(user_tweets4, "data/cong_politician_tweets.csv")

user <- user_tweets2[!duplicated(user_tweets2$user_id),]

last <- subset(user_tweets2, user_id == user$user_id[74])

user$screen_name[72]

which(df$user_id == user$user_id[69])

#########################################################################

?get_timeline
length(unique(user_tweets3$user_id))

user_tweets <- rbind(user_tweets, user_tweets2, user_tweets3)

oo <- dplyr::bind_rows(o[801:1317])
oo2 <- dplyr::bind_rows(o[101:800])
oo2 <- do.call("rbind", o[701:1334])

tweets <- rbind(oo, tweets)
rm(oo2)

saveRDS(tweets, "data/politician_list_all.rds")
df <- readRDS("data/politician_list_all.rds")


saveRDS(o, "data/politician_listedtweets.rds")
o <- readRDS("data/movie_star_tweets_74.rds")

#install.packages("cld3")
#install.packages("textcat")
cld3::detect_language_mixed(df$description[1])
textcat::textcat(df$description[5])



####################################################################
## get random tweets/twitter

tw <- list()
while(TRUE) {
  tw[[length(tw) + 1]] <-
    rtweet::search_tweets("-filter:verified", n = 10000)
  Sys.sleep(runif(1, 0, 60*60*1))
}

saveRDS(dftw, "data/randomtweets.rds")

dftw <- do.call("rbind", tw)

head(dftw)

names(dftw)

dftw_en <- dplyr::filter(dftw, lang == "en")

dftw_en_user <- dftw_en[!duplicated(dftw_en$user_id), ]

saveRDS(dftw_en_user, "data/randomuser20190827-29.rds")

#install.packages("tweetbotornot")
devtools::install_github("mkearney/tweetbotornot")
library(tweetbotornot)

df <- readRDS("data/randomuser20190827-29.rds")

bots <- tweetbotornot::tweetbotornot(df$user_id, fast = TRUE)
