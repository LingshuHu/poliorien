
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



