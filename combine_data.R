
library(dplyr)
library(stringr)
library(tidytext)
library(tokenizers)
library(sentimentr)
library(stringi)
library(ggplot2)

# From file gather_data.R
# Webscraped "https://www.premierleague.com/results" to get all the match reports
list_report <- readRDS("RDS_files/list_report.RDS")

# From file gather_data.R
# Used Fotmobs API to get player performance of the same matches as in the match reports
list_stats <- readRDS("RDS_files/list_stats.RDS")



# The two datsets uses different names on football teams
# thats why we have a translator
team_translator <- readRDS("RDS_files/team_translator.RDS") %>% as.data.frame()

translate <- function(team) {
  team_translator[team_translator$PL == team, "fotmob"]
}


# Merging the two datasets list_report and list_stats
for(i in 1:length(list_stats)) {
  
  hteam_stats <- list_stats[[i]]$hteam
  ateam_stats <- list_stats[[i]]$ateam
  
  
  all_hteam_ateam_report <- as.vector(unname(unlist(sapply(list_report,
                                      function(x){
                                        translate_hteam <- translate(x$hteam)
                                        translate_ateam <- translate(x$ateam)
                                        str_c(translate_hteam,translate_ateam)
                                      }))))
  
  index_report <- which(all_hteam_ateam_report == str_c(hteam_stats, ateam_stats))
  
  list_stats[[i]]$report <- list_report[[index_report]]$report
}


saveRDS(list_stats, "RDS_files/combined_stats_report_list.RDS")




# ggplot histogram
players_rating <- as.numeric(unlist(sapply(list_stats, function(x){
  c(x$home_players$FotMob.rating, x$away_players$FotMob.rating)
} )))


players_rating <- players_rating[!players_rating == 0]



ggplot(data = data.frame(players_rating), aes(x = players_rating)) +
  geom_histogram(binwidth = 0.5, fill = "lightblue", color = "black") +
  labs(title = "Histogram of FotMob Ratings", x = "FotMob Rating", y = "Frequency") +
  theme_minimal() +
  theme(
    text = element_text(size = 13),      
    axis.title = element_text(size = 15), 
    axis.text = element_text(size = 12), 
    plot.title = element_text(size = 17)  
  )


ggsave("player_ratings_histogram.png", width = 5, height = 3.5, dpi = 300)


