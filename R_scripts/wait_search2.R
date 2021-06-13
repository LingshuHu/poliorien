

#' wait_search
#'
#' @param s start point
#' @param t terminate point
#' @param inu max number of users can get per search
#' @param uids user ids
#' @param id_type user_id or screen_name
#' @param n number of tweets per user
wait_search <- function(s, t, inu = NULL, uids, id_type = "screen_name", n) {
  #token <- get_tokens()
  
  rq = as.integer(ceiling(n/200L)) # calculate number of quires
  
  if (is.null(inu)) { # max number of users can get per search
    inu <- as.integer(floor((1500L)/rq))
  }
  
  lst_len <- as.integer(ceiling( (t - s) / inu) )
  lst <- vector("list", length = lst_len) # empty list
  
  gt <- function(st, en) {
    rt <- tryCatch(
      rtweet::get_timeline(uids[st:en], 
                           n = n, 
                           token = rtweet::bearer_token(), 
                           check = F),
      error = function(e) NA
    )
    return(rt)
  }
  
  i <- 1L
  while(s <= t) {
    #rtlimit <- rate_limit(token, "search/tweets")
    #remaining <- rtlimit[["remaining"]] * 100
    #reset <- rtlimit[["reset"]]
    #reset <- as.numeric(reset, "secs")
    end_num <- as.integer(s + inu - 1L)
    if(end_num > t) {
      end_num <- t
    } 
    time_s <- Sys.time()
    lst[[i]] <- gt(st = s, en = end_num) ## search tweets and store in lst
    time_e <- Sys.time()
    time_diff <- as.numeric(time_e - time_s, units = "secs")
    
   
    if (id_type == "screen_name") {
      
      end_num1 <- tryCatch(
        which(uids == tolower(lst[[i]]$screen_name[nrow(lst[[i]])])),
        error = function(e) NULL
      )
      end_num2 <- tryCatch(
        which(uids == tolower(lst[[i]]$screen_name[nrow(lst[[i]])-1])),
        error = function(e) NULL
      )
      end_num3 <- tryCatch(
        which(uids == tolower(lst[[i]]$screen_name[nrow(lst[[i]])-2])),
        error = function(e) NULL
      )
      end_nums <- c(end_num1, end_num2, end_num3)
      
    } else if (id_type == "user_id") {
      end_num1 <- tryCatch(
        which(uids == lst[[i]]$user_id[nrow(lst[[i]])]),
        error = function(e) NULL
      )
      end_num2 <- tryCatch(
        which(uids == lst[[i]]$user_id[nrow(lst[[i]])-1]),
        error = function(e) NULL
      )
      end_num3 <- tryCatch(
        which(uids == lst[[i]]$user_id[nrow(lst[[i]])-2]),
        error = function(e) NULL
      )
      end_nums <- c(end_num1, end_num2, end_num3)
    }
    
    if (length(end_nums) <= 0L) {
      message("Problems occurred. Not finished")
      break
    } else {
      end_num <- max(end_nums)
      i <- i + 1L
      s <- end_num + 1L
      if (s < t & time_diff < 950) {
        message(paste0("waiting about ", (950 - time_diff)/60, " minutes"))
        Sys.sleep(950 - time_diff)
      }
    }
  }
  #lst <- do.call("rbind", lst)
  return(lst)
}

