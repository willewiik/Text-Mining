
library(dplyr)
library(stringr)
library(tidytext)
library(tokenizers)

list_report <- readRDS("list_report.RDS")
list_stats <- readRDS("list_stats.RDS")

# The two datsets uses different names on football teams
# thats why we have a translator
team_translator <- readRDS("team_translator.RDS") %>%  as.data.frame()

translate <- function(team) {
  team_translator[team_translator$PL == team,"fotmob"]
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







