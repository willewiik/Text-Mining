

list_report <- readRDS("list_report.RDS")
list_stats <- readRDS("list_stats.RDS")


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


saveRDS(mat[,1:2],"team_translator.RDS")

