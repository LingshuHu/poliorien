
library(rtweet)
usr_id <- read.csv("Twitter_user_lists_test_deeplearning_Jan_10.csv",
                   stringsAsFactors = F)

usr_id$ID <- sub("@", "", usr_id$ID)
usr_id$ID <- sub("\\s+", "", usr_id$ID)

rt <- search_tweets("covid", 18000)

usr <- get_timeline(usr_id$ID, 
                    n = 3200, 
                    token = rtweet::bearer_token(), 
                    check = T)
