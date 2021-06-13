
library(rtweet)
usr_id <- read.csv("Twitter_user_lists_test_deeplearning_Jan_10.csv",
                   stringsAsFactors = F)

usr_id$ID <- sub("@", "", usr_id$ID)
usr_id$ID <- sub("\\s+", "", usr_id$ID)

#rt <- search_tweets("covid", 18000)

usr <- get_timeline(usr_id$ID, 
                    n = 3200, 
                    token = rtweet::bearer_token(), 
                    check = T)


which(usr_id$ID == usr$screen_name[nrow(usr)])

library(dplyr)
usr_info <- dplyr::group_by(usr, user_id) %>% summarise(tweet_count = n())

usr <- dplyr::left_join(usr, usr_info, by = "user_id")
colnames(usr_id)[2] <- "screen_name"
usr <- dplyr::left_join(usr, usr_id[,2:4], by = "screen_name")

rtweet::write_as_csv(usr, "data/test_user200_2021-05-30.csv")

### extract users from election2020 datasets
rt <- readRDS("../dissertation/data/election2020_seed_users_tweets_20210131-20210601_15001-20000.rds")
dem <- rt[grepl("democrat", rt$description, ignore.case = T),]
length(unique(dem$user_id))

rep <- rt[grepl("republican|\\<GOP\\>", rt$description, ignore.case = T),]
length(unique(rep$user_id))

poli_test2 <- rbind(dem, rep)
poli_test <- rbind(poli_test, poli_test2)
saveRDS(poli_test, "data/test_users_from_profile.rds")
rtweet::write_as_csv(poli_test[,1:5], "data/test_users_from_profile_text.csv")

pt <- readRDS("data/test_users_from_profile.rds")

dem <- pt[grepl("democrat", pt$description, ignore.case = T),]
rep <- pt[grepl("republican|\\<GOP\\>", pt$description, ignore.case = T),]

dem$Party <- "D"
rep$Party <- "R"

pt2 <- rbind(dem, rep)

dem2 <- dem[!duplicated(dem$user_id),]
rep2 <- rep[!duplicated(rep$user_id),]

pu <- rbind(dem2, rep2)

pu2 <- group_by(pu, user_id) %>% summarise(n = n())
pu2 <- ungroup(pu2)
repeated <- pu2[pu2$n > 1, ]
#pu_rept <- pu[pu$user_id %in% repeated$user_id, ]
#pu_clear <- pu[!pu$user_id %in% repeated$user_id, ]
pt_clear <- pt2[!pt2$user_id %in% repeated$user_id, ]

pt_pred <- rtweet::read_twitter_csv("data/test_users_from_profile_party_proba.csv")

pt_clear <- dplyr::inner_join(pt_clear, pt_pred[, c("status_id", "pred_proba")], by = "status_id")
saveRDS(pt_clear, "data/test_users_from_profile_preprocessed.rds")
usr <- readRDS("data/test_users_from_profile_preprocessed.rds")

########################## get pred data ##############################
party3 <- function(x) {
  if (x < 0.4) {p = "D"}
  else if (x > 0.6) {p = "R"}
  else {p = "N"}
  return(p)
}

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

## only when R or D is more than 50%. Don't use this one
getmode <- function(v) {
  if (length(v[v=="D"])/length(v) > 0.5) p = "D" 
  else if (length(v[v=="R"])/length(v) > 0.5) p = "R"
  else p = "N"
  return(p)
}

library(dplyr)

usr <- rtweet::read_twitter_csv("data/test_user200_2021-05-30_pred.csv")
usr <- rtweet::read_twitter_csv("data/test_movie_star_1-300_text_pred.csv")
usr$Party <- "N"
usr <- subset(usr, tweet_count >= 50)

usr_id <- unique(usr$screen_name)

## only keep 30 tweets
usr30 <- vector("list", length = length(usr_id))
for (i in seq_along(usr_id)) {
  df <- subset(usr, screen_name == usr_id[i])
  if (nrow(df) > 50) {
    set.seed(123)
    df <- dplyr::sample_n(df, 50)
  }
  usr30[[i]] <- df
}
usr30 <- do.call("rbind", usr30)

### obtain catigory party
usr30 <- dplyr::mutate(usr30, partyc = sapply(pred_proba, party3))

usr30_party <- group_by(usr30, screen_name) %>% summarise(user_partyc = getmode(partyc))
#usr30_party2 <- group_by(usr30, screen_name) %>% summarise(user_party2 = getmode(pred_label))

usr30_user <- usr30[!duplicated(usr30$screen_name), ]
usr30_user <- dplyr::left_join(usr30_user, usr30_party, by = "screen_name")


sum(usr30_user$user_partyc == usr30_user$Party, na.rm = T) / nrow(usr30_user) *100


MLmetrics::F1_Score(usr30_user$Party, usr30_user$user_partyc, positive = NULL)
table(usr30_user$user_partyc)


x <- usr30_user[, c("Party", "partyc")]
#usr30_user <- dplyr::left_join(usr30_user, usr30_party2, by = "screen_name")
### calcualte accuracy
usr30_user_u <- subset(usr30_user, Typical == "UT")
usr30_user_u$Party <- "N"
usr30_user <- subset(usr30_user, Typical == "T")
usr30_user <- rbind(usr30_user_t, usr30_user_u)
