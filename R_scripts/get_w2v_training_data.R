

rt1 <- rtweet::read_twitter_csv("data/cong_politician_tweets_2020-3-12-2021-5-28_text_party.csv")
rt1 <- rt1[!duplicated(rt1$text),]
rt2 <- rtweet::read_twitter_csv("../dissertation/data/election2020_seed_users_tweets_20210131-20210601_1-5000_text.csv")
rt2 <- rt2[!duplicated(rt2$text),]
names(rt1)
names(rt2)
rt1 <- rbind(rt1[,c(1,2,5)], rt2[!rt2$status_id %in% rt1$status_id, ])
rtweet::write_as_csv(rt1, "data/w2v_training_data_conpoli&elect_seeduser1-5000.csv")

rt3 <- rtweet::read_twitter_csv("../dissertation/data/election2020_seed_users_tweets_20210131-20210601_5001-10000_text.csv")
rt3 <- rt3[!duplicated(rt3$text),]
rt1 <- rbind(rt1, rt3)
rt1 <- rt1[!duplicated(rt1$text),]
rt1 <- rt1[!duplicated(rt1$status_id),]
rm(rt3)
rtweet::write_as_csv(rt1, "data/w2v_training_data_conpoli&elect_seeduser1-10000.csv")
