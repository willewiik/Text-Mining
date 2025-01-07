
library(dplyr)
library(stringr)
library(tidytext)
library(tokenizers)
library(sentimentr)
library(stringi)

# From file premierleague.R
# Webscraped "https://www.premierleague.com/results" to get all the match reports
list_report <- readRDS("list_report.RDS")

# From file fotmob.R
# Used Fotmobs API to get player performance of the same matches as in the match reports
list_stats <- readRDS("list_stats.RDS")



# The two datsets uses different names on football teams
# thats why we have a translator
team_translator <- readRDS("team_translator.RDS") %>% as.data.frame()

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


saveRDS(list_stats, "combined_stats_report_list.RDS")





# 
# 
# get_entitiy_sentiment_analysis <- function(report, df_players) {
#   
#   # Use tokenizers package for splitting into sentences
#   sentences <- tokenize_sentences(report)[[1]]
#   
#   player_sentiments <- list()
#   for(i in 1:length(sentences)) {
#     
#     fullname_in_sentence <-  unlist(lapply(df_players$full_name, function(x) {
#       str_detect(sentences[i], (paste0("\\b", x, "\\b")))
#     }))
#     
#     surname_in_sentence <- unlist(lapply(df_players$surname, function(x) {
#       str_detect(sentences[i], (paste0("\\b", x, "\\b")))
#     }))
#     
#     name_in_sentence <- (fullname_in_sentence | surname_in_sentence)
#   
#    
#     if(sum(name_in_sentence) == 1) {
#       #print(sentences[i])
#       #print(df_players$full_name[name_in_sentence])
#       # Get the index of the player found
#       player_index <- which(name_in_sentence)
#       player_name <- df_players$full_name[player_index]
#       
#       # Perform sentiment analysis on the sentence
#       sentiment_result <- sentiment_by(sentences[i])$ave_sentiment
#       
#       # If this player already has sentiment accumulated, append the new sentiment
#       if (player_name %in% names(player_sentiments)) {
#         player_sentiments[[player_name]] <- c(player_sentiments[[player_name]], sentiment_result)
#       } else {
#         # Otherwise, initialize the list with the first sentiment score
#         player_sentiments[[player_name]] <- c(sentiment_result)
#       }
#       
#       
#       
#     } else {
#      
#     }
#     
#     mean_sentiments <- sapply(player_sentiments, function(sentiments) {
#       mean(sentiments)
#     })
#     
#   }
#  
#     return(mean_sentiments)
# 
# }
# 
# 
# 
# 
# 
# for(i in 1:length(list_stats)){
#   print(i)
#   
#   
#   list_stats[[i]]$home_players$sentiment_value <- NA
#   list_stats[[i]]$away_players$sentiment_value <- NA
#   
# 
#   report <- list_stats[[i]]$report
#   
#   if(report == "") next
#   
#   # Remove special characters and accents
#   list_stats[[i]]$home_players$Name <- stri_trans_general(list_stats[[i]]$home_players$Name, "Latin-ASCII")
#   list_stats[[i]]$away_players$Name <- stri_trans_general(list_stats[[i]]$away_players$Name, "Latin-ASCII")
# 
#   
#   h_players <- list_stats[[i]]$home_players$Name
#   a_players <- list_stats[[i]]$away_players$Name
#   
#   players <- c(h_players, a_players)
#   
#   surnames <- sapply(players, function(player) {
#     parts <- str_split(player, " ", simplify = TRUE)
#     if (length(parts) > 1) {
#       return(parts[, 2])  # Return the second part (surname)
#     } else {
#       return(player)  # Return the name itself if there's no space (single-part name)
#     }
#   })
#   
#  
#   
#   df_players <- data.frame("full_name" = players, "surname" = unname(surnames))
#   
#   
#   result <- get_entitiy_sentiment_analysis(report = report, df_players = df_players)
#   
#   
#   index_h_players <- which(h_players %in% names(result))
#   index_a_players <- which(a_players %in% names(result))
#   
#   if(length(index_h_players) > 0) {
#     
#     players <- h_players[index_h_players]
#     sentiment_scores <- result[players]
#     
#     # Assign sentiment scores to the appropriate indices
#     list_stats[[i]]$home_players$sentiment_value[index_h_players] <- sentiment_scores
#     
#   }
#   
#   if(length(index_a_players) > 0) {
#     
#     players <- a_players[index_a_players]
#     sentiment_scores <- result[players]
#     
#     # Assign sentiment scores to the appropriate indices
#     list_stats[[i]]$away_players$sentiment_value[index_a_players] <- sentiment_scores
#   }
#   
#   
# }
# 
# 
# 
# 
# combined_data <- do.call(
#   rbind, 
#   lapply(list_stats, function(x) x$home_players[, c("Name", "FotMob.rating", "sentiment_value")])
# )
# 
# # Remove rows where sentiment_value is NA
# # Remove rows where fotmob ratings = 0 (too few minutes played)
# 
# cleaned_data <- combined_data %>%
#   filter(!is.na(sentiment_value), FotMob.rating != 0) %>%
#   mutate(
#     sentiment_class = factor(
#       case_when(
#         sentiment_value < 0 ~ "Negative",
#         sentiment_value == 0 ~ "Neutral",
#         sentiment_value > 0 ~ "Positive"
#       ),
#       levels = c("Negative", "Neutral", "Positive")
#     ),
#     FotMob.rating = as.numeric(FotMob.rating)
#   )
# 
# library(ggplot2)
# 
# ggplot(cleaned_data, aes(x = FotMob.rating, y = sentiment_value)) +
#   geom_point() +
#   geom_smooth(method = "lm", color = "blue", se = FALSE) + # Add a trend line
#   labs(
#     title = "FotMob Rating vs Sentiment Value",
#     x = "FotMob Rating",
#     y = "Sentiment Value"
#   ) +
#   theme_minimal()
# 
# 
# ggplot(cleaned_data, aes(x = sentiment_class, y = FotMob.rating, fill = sentiment_class)) +
#   geom_boxplot(outlier.color = "black", outlier.shape = 1) +
#   labs(
#     title = "FotMob Rating by Sentiment Classification",
#     x = "Sentiment Classification",
#     y = "FotMob Rating"
#   ) +
#   scale_fill_manual(
#     values = c("Negative" = "#FF0000", "Neutral" = "#808080", "Positive" = "#00FF00") # Red, gray, green
#   ) +
#   theme_minimal()
# 
# 
# # Load necessary library
# library(dplyr)
# 
# # Compute statistics for FotMob.rating and sentiment_value per sentiment class
# summary_stats_by_sentiment <- cleaned_data %>%
#   group_by(sentiment_class) %>%
#   summarise(
#     num_obs = n(),
#     FotMob_rating_median = median(FotMob.rating, na.rm = TRUE),
#     FotMob_rating_quantiles = list(quantile(FotMob.rating, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)),
#   )
# 
# # View the results
# summary_stats_by_sentiment
# 
# 
# 
# 
# table(cleaned_data$sentiment_class)
# 
# 
# 
# 
# # Data section, ggplot histogram
# 
# 
# players_rating <- as.numeric(unlist(sapply(list_stats, function(x){
#   c(x$home_players$FotMob.rating, x$away_players$FotMob.rating)
# } )))
# 
# 
# players_rating <- players_rating[!players_rating == 0]V
# 
# library(ggplot2)
# 
# # Assuming players_rating is a numeric vector
# ggplot(data = data.frame(players_rating), aes(x = players_rating)) +
#   geom_histogram(binwidth = 0.5, fill = "lightblue", color = "black") +
#   labs(title = "Histogram of Player Ratings", x = "Rating", y = "Frequency") +
#   theme_minimal() +
#   theme(
#     text = element_text(size = 13),      # Increase text size
#     axis.title = element_text(size = 15), # Increase axis title size
#     axis.text = element_text(size = 12),  # Increase axis number size
#     plot.title = element_text(size = 17)  # Increase plot title size
#   )
# 
# 
# ggsave("player_ratings_histogram.png", width = 5, height = 3.5, dpi = 300)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
