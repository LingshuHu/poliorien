
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

df <- read.csv("data/cong116_politician_twitter.csv", stringsAsFactors = F)
df$user_id <- as.character(as.numeric(df$user_id))

df2 <- df[!duplicated(df$screen_name), ]
df2 <- df[!duplicated(df$user_id), ]
df2[df2$user_id == "168673083", ]

df[df$user_id == "168673083", ]

dupl <- user_tweets3[user_tweets3$user_id == "168673083", ]
table(duplicated(dupl$screen_name))

## get their tweets



wait_search <- function(s, t, id, inu = NULL, uids, n) {
  token <- get_tokens()
  lst <- vector("list") # empty list
  
  if(n <= 200) { # calculate number of quires
    rq = 1
  } else {rq = ceiling(n/200)}
  
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
    time_s <- Sys.time()
    lst[[i]] <- gt(st = s, en = e)
    time_e <- Sys.time()
    time_diff <- as.numeric(time_e - time_s, units = "secs")
    #s <- e + 1 
    if (id == "screen_name") {
      s <- which(uids == lst[[i]]$screen_name[nrow(lst[[i]])]) + 1
    } else {
      s <- which(uids == lst[[i]]$user_id[nrow(lst[[i]])]) + 1
    }
    
    i <- i + 1
    if (s < t & time_diff < 910) {
      message(paste0("waiting about ", (910 - time_diff)/60, " minutes"))
      Sys.sleep(910 - time_diff)
    }
  }
  #lst <- do.call("rbind", lst)
  return(lst)
}

library(lubridate)

u <- rtweet::get_timeline(df2$screen_name[1000], n = 100)

user_tweets <- wait_search(s = 1, t = 1041, id = "screen_name", uids = df2$screen_name, n = 3200)

user_tweets3 <- subset(user_tweets2, !(status_id %in% user_tweets$status_id))

user_tweets2 <- do.call("rbind", user_tweets)

user_tweets <- rtweet::get_timeline(df$user_id[2], n = 1000)

user_tweets <- rbind(user_tweets, user_tweets2)
user_tweets3 <- user_tweets[!duplicated(user_tweets$status_id), ]

user_tweets4 <- dplyr::left_join(user_tweets3, df2[, -3], by = "user_id")

saveRDS(user_tweets2, "data/cong_politician_tweets_2020-7-5.rds")
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
## classify political orientation based on score of each tweet
library(dplyr)

pt <- rtweet::read_twitter_csv("../coronavirus/user_tweets_for_ideology/covid_users30tweets_usa_apr11-may25_21pm-23pm_onethird_2_party.csv")
poli <- rtweet::read_twitter_csv("../coronavirus/user_tweets_for_ideology/covid_users30tweets_usa_apr11-may25_21pm-23pm_onethird_2_poli.csv")
party <- rtweet::read_twitter_csv(paste0("../coronavirus/user_tweets_for_ideology/", 
                                         files[1], "_party.csv"))


get_user_party <- function(path, filename) {
  party <- rtweet::read_twitter_csv(paste0(path, filename, "_party.csv"))
  poli <- rtweet::read_twitter_csv(paste0(path, filename, "_poli.csv"))
  user <- rtweet::read_twitter_csv(paste0(path, filename, "_tweets.csv"))
  
  df <- cbind(user[, c("status_id", "user_id", "screen_name")], poli[, "poli"], party)
  df <- df[df$poli, ]
  classify_party <- function(x) {
    if (x < 0.3) {p = "D"}
    else if (x > 0.7) {p = "R"}
    else {p = "N"}
    return(p)
  }
  getmode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  df <- dplyr::mutate(df, partyc = sapply(party, classify_party))
  df_user <- df %>% group_by(user_id) %>% summarise(user_partyc = getmode(partyc),
                                                    poli_tweets = n())
  return(df_user)
}


ut <- get_user_party("../coronavirus/user_tweets_for_ideology/", 
                     "covid_users30tweets_usa_apr11-may25_21pm-23pm_onethird_2")
ut2 <- ut[ut$poli_tweets>3, ]

# multiple datasets at once
files <- list.files(path = '../coronavirus/user_tweets_for_ideology', pattern = '*tweets.csv', full.names = FALSE)
files <- sub("_tweets\\.csv", "", files)

ut_party <- lapply(files, function(x) get_user_party(path = '../coronavirus/user_tweets_for_ideology/', filename = x))

ut_party2 <- do.call("rbind", ut_party)

saveRDS(ut_party2, "../coronavirus/user_tweets_for_ideology/user_party_poli_jan24-may25_21pm-23pm_onethird.rds")





