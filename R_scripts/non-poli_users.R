
rt <- readRDS("data/movie_star_listedtweets.rds")

rt2 <- do.call("rbind", rt[1:300])

rtweet::write_as_csv(rt2[, 1:5], "data/test_movie_star_1-300_text.csv")
