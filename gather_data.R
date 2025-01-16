
library(dplyr)
library(httr)
library(jsonlite)
library(RSelenium)
library(wdman)
library(netstat)
library(dplyr)
library(stringr)
library(rvest)


# FOTMOB API ===================================================================

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

saveRDS(list_stats, "RDS_files/list_stats.RDS")









# www.premierleague.com WEB SCRAPE =============================================

url <- "https://www.premierleague.com/results"



remote_driver <- rsDriver(browser = "firefox",chromever = NULL,
                          verbose = F, port = free_port())
remDrv <- remote_driver$client
remDrv$maxWindowSize()

remDrv$navigate(url)

html_content <- remDrv$getPageSource()[[1]]
html_parsed <- read_html(html_content)
# Select elements with the data-href attribute
matchurls <- html_parsed %>%  html_nodes('[data-href]') %>%  html_attr('data-href')



list_report <- list()

for(i in 1:length(matchurls)) {
  
  print(i)
  html_content <- read_html(str_c("https:",matchurls[i]))
  match_report <- html_content %>% html_nodes('.article__body p') %>% html_text() 
  full_report <- paste(match_report, collapse = " ")
  
  
  teamnames <- html_content %>% html_nodes('.mc-summary__team-name') %>% html_text() 
  teamnames <- teamnames[c(T,F,T,F)]
  
  list_report <- append(list_report, list(list(report = full_report,
                                               hteam = teamnames[1],
                                               ateam = teamnames[2])))
  
  names(list_report)[length(list_report)] <- str_c(teamnames[1]," vs ", teamnames[2])
  
}
saveRDS(list_report, "RDS_files/list_report.RDS")


remote_driver$server$stop()










# MERGE TEAMNAMES ==============================================================


list_report <- readRDS("RDS_files/list_report.RDS")
list_stats <- readRDS("RDS_files/list_stats.RDS")


fotmob_names <- unique(as.vector(unname(unlist(sapply(list_stats,
                                                      function(x) c(x$hteam,x$ateam))))))

PL_names <- unique(as.vector(unname(unlist(sapply(list_report,
                                                  function(x) c(x$hteam,x$ateam))))))

mat <- matrix(NA, length(fotmob_names), 3)
colnames(mat) <- c("fotmob","PL","valid")



fotmob_names <- fotmob_names %>%  as.data.frame()
colnames(fotmob_names) <- "team"


PL_names <- PL_names %>% as.data.frame() 
colnames(PL_names) <- "team"


i <- 1
for(team in fotmob_names$team) {
  
  index_team_PL <- which.min(lapply(PL_names, function(x) stringdist::stringdist(team, x, method = "jw")) %>% unlist())
  dist_PL <- (lapply(PL_names, function(x) stringdist::stringdist(team, x, method = "jw")) %>% unlist())[index_team_PL]
  
  
  
  index_team <- index_team_PL
  dist <- dist_PL
  
  
  mat[i,1] <- team
  mat[i,2] <- PL_names$team[index_team]
  mat[i,3] <- ifelse(dist < 0.1, TRUE, FALSE)
  
  i <- i + 1
}

mat[20,2] <- "Spurs"
mat[18,2] <- "Man City"


saveRDS(mat[,1:2],"RDS_files/team_translator.RDS")











