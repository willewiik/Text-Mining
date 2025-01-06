library(httr)
library(jsonlite)
library(RSelenium)
library(wdman)
library(netstat)
library(dplyr)
library(stringr)
library(rvest)


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
  

#remote_driver$server$stop()

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
saveRDS(list_report, "list_report.RDS")






