
library(dplyr)
library(httr)


xfmreq <- "eyJib2R5Ijp7InVybCI6Ii9hcGkvbWF0Y2hNZWRpYT9tYXRjaElkPTQ1MDY0NDUmY2NvZGUzPVNXRSIsImNvZGUiOjE3MzQzNTAzNDIwMDEsImZvbyI6IjBjMjkwYzM4OSJ9LCJzaWduYXR1cmUiOiJFMDA5MzlGQkQyREEzOTAzQURBMjQ3MjMxNTBENUU5OCJ9"

fotmob_league <- function(league_id = 47, xfmreq) {
  
  
  url <- paste0("https://www.fotmob.com/api/leagues?id=",league_id,"&ccode3=SWE")
  
  res <- GET(url,
             add_headers(
               `authority` = "www.fotmob.com",
               `accept` = "*/*",
               `accept-encoding` = "gzip, deflate, br, zstd",
               `accept-language` = "sv,en;q=0.9,en-GB;q=0.8,en-US;q=0.7",
               `sec-ch-ua` = "\"Chromium\";v=\"130\", \"Microsoft Edge\";v=\"130\", \"Not?A_Brand\";v=\"99\"",
               `sec-ch-ua-mobile` = "?0",
               `sec-ch-ua-platform` = "\"Windows\"",
               `sec-fetch-dest` = "empty",
               `sec-fetch-mode` = "cors",
               `sec-fetch-site` = "same-origin",
               `user-agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/130.0.0.0 Safari/537.36 Edg/130.0.0.0",
               `X-Mas` = xfmreq
             ))
  
  
  if (httr::status_code(res) == 200) {
    
    json_file <- jsonlite::fromJSON(rawToChar(res$content))
    
  } else {
    
    print(paste("Failed to fetch data. Status code:",  httr::status_code(res)))
    return(NULL)
  }
  
    is_finished <- json_file$matches$allMatches$status$finished
    
    match_ids <- json_file$matches$allMatches$id[is_finished]
    hteams <- json_file$matches$allMatches$home$name[is_finished]
    ateams <- json_file$matches$allMatches$away$name[is_finished]
    
    cancelled <- json_file$matches$allMatches$status$cancelled[is_finished]
    awarded <- json_file$matches$allMatches$status$awarded[is_finished]
    
    awarded[is.na(awarded)] <- FALSE
    
    date <- json_file$matches$allMatches$status$utcTime[is_finished]
    

  return(data.frame(match_ids = match_ids, hteam = hteams, ateam = ateams,
                     cancelled = cancelled, awarded = awarded, date = date))
  
}


fotmob_match <- function(match_id, xfmreq) {
  
 
  url <- paste0("https://www.fotmob.com/api/matchDetails?matchId=", match_id)
  
  res <- GET(url,
             add_headers(
               `authority` = "www.fotmob.com",
               `accept` = "*/*",
               `accept-encoding` = "gzip, deflate, br, zstd",
               `accept-language` = "sv,en;q=0.9,en-GB;q=0.8,en-US;q=0.7",
               `sec-ch-ua` = "\"Chromium\";v=\"130\", \"Microsoft Edge\";v=\"130\", \"Not?A_Brand\";v=\"99\"",
               `sec-ch-ua-mobile` = "?0",
               `sec-ch-ua-platform` = "\"Windows\"",
               `sec-fetch-dest` = "empty",
               `sec-fetch-mode` = "cors",
               `sec-fetch-site` = "same-origin",
               `user-agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/130.0.0.0 Safari/537.36 Edg/130.0.0.0",
               `X-Mas` =xfmreq
             ))
  
  
  
  if (httr::status_code(res) != 200) {
    stop(paste("Failed to fetch data. Status code:", httr::status_code(res)))
  }
  
  
  json_file <- jsonlite::fromJSON(rawToChar(res$content))
  

  # Home team
  player_h_id <- json_file$content$lineup$homeTeam$starters$id
  player_h_name <- json_file$content$lineup$homeTeam$starters$name
  subs_h_id <- json_file$content$lineup$homeTeam$subs$id
  subs_h_name <- json_file$content$lineup$homeTeam$subs$name
  

  
  # Away team
  player_a_id <- json_file$content$lineup$awayTeam$starters$id
  player_a_name <- json_file$content$lineup$awayTeam$starters$name
  subs_a_id <- json_file$content$lineup$awayTeam$subs$id
  subs_a_name <- json_file$content$lineup$awayTeam$subs$name
  
  
  get_player_stats_fun <- function(player_id) {
    stats_list <- list()
    # Define the metrics to retrieve
    metrics <- c("FotMob rating", "Minutes played", "Goals", "Assists",
                 "Chances created", "xG Non-penalty", "Expected assists (xA)")
    
  
    for (metric in metrics) {
      stat_value <- json_file$content$playerStats[[as.character(player_id)]]$stats$stats[[metric]]
      
      # Check if the stat is available and extract the non-NA value
      if (!is.null(stat_value$stat$value)) {
        stats_list[[metric]] <- stat_value$stat$value[!is.na(stat_value$stat$value)][1]
      } else {
        if(player_id == "0") {
          stats_list[[metric]] <- NA
        } else {
          stats_list[[metric]] <- 0
        }
      }
    }
    
    return(stats_list)
  }
  
  
  get_team_info <- function(player_ids, player_names, position) {
    player_stats_list <- lapply(player_ids, get_player_stats_fun)
    team_df <- do.call(rbind, lapply(seq_along(player_ids), function(i) {
      c(ID = player_ids[i], Name = player_names[i],Position = position[i], unlist(player_stats_list[[i]]))
    }))
    team_df <- data.frame(team_df, stringsAsFactors = FALSE)
    return(team_df)
  }
  

  squad_h <- get_team_info(c(player_h_id,subs_h_id), c(player_h_name,subs_h_name),
                          c(rep("Starter",11),rep("Sub",length(subs_h_name))))
  
  squad_a <- get_team_info(c(player_a_id,subs_a_id), c(player_a_name,subs_a_name),
                           c(rep("Starter",11),rep("Sub",length(subs_a_name))))
  

  result <- list(
    hteam = json_file$general$homeTeam$name,
    ateam = json_file$general$awayTeam$name,
    starttime = json_file$general$matchTimeUTCDate,
    score = json_file$header$teams$score
  )
  
  result$home_players <- squad_h
  result$away_players <- squad_a

  
  return(result)
}


matches <- fotmob_league(league_id = 47, xfmreq = xfmreq)




list_stats <- list()


for(i in 1:nrow(matches)) {
  
  print(i)
  temp_match <- fotmob_match(match_id = matches$match_ids[i], xfmreq = xfmreq)
  list_stats <- append(list_stats, list(temp_match))
  names(list_stats)[length(list_stats)] <- str_c(temp_match$hteam, " vs ", temp_match$ateam)
  
}

saveRDS(list_stats, "list_stats.RDS")





