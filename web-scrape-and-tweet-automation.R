##################################
# Web Scape and Tweet Automation #
# Using {rvest} and {rtweet}     #
##################################


library('rvest')
library('dplyr')
library('stringr')


###
# Scrape Cleaning the Glass
###

url <- "https://cleaningtheglass.com/stats/games?date=2022-02-27"

page = read_html(url)
rawlists <- 
  page %>% 
  html_nodes('.game_table') %>% 
  html_table()


###
# Clean up and collapse the data
###

cleanData <- data.frame(matrix(nrow = 0, ncol = 13))
colnames(cleanData) = c("game", "team", "pts", "pts_poss_tile", "pts_poss", 
                        'efg_tile', 'efg', 'tov_tile', "tov", 'oreb_tile', 
                        "oreb", "ftr_tile", "ftr")

for(i in 1:length(rawlists)) {
  listi <- 
    rawlists %>%
    .[[i]] %>%
    .[1:2,2:13] %>%
    `colnames<-`(c("team", "pts", "pts_poss_tile", "pts_poss", 'efg_tile', 'efg',
                   'tov_tile', "tov", 'oreb_tile', "oreb", "ftr_tile", 'ftr')) %>%
    mutate(game = paste(.$team, collapse = "-"), .before = "team")
  
  cleanData = rbind(cleanData, listi)
}


###
# Make the calculations
###
jazz = 
  cleanData %>% 
  mutate(efg = as.numeric(str_sub(efg, 1, str_length(efg)-1)), 
         tov = as.numeric(str_sub(tov, 1, str_length(tov)-1)),
         oreb = as.numeric(str_sub(oreb, 1, str_length(oreb)-1)),
         ftr = as.numeric(ftr)) %>%
  filter(grepl("UTA", game))

if(nrow(jazz) != 0) {
  matchup = unique(jazz$game)
  
  o = jazz$pts_poss[jazz$team == "UTA"]
  o_tile = jazz$pts_poss_tile[jazz$team == "UTA"]
  d = jazz$pts_poss[jazz$team != "UTA"]
  d_tile = 100 - as.integer(jazz$pts_poss_tile[jazz$team != "UTA"])
  
  efg = sprintf("%.1f", round(jazz$efg[jazz$team == "UTA"] - jazz$efg[jazz$team != "UTA"], 1))
  tov = sprintf("%.1f", round(jazz$tov[jazz$team == "UTA"] - jazz$tov[jazz$team != "UTA"], 1) * -1)
  oreb = sprintf("%.1f", round(jazz$oreb[jazz$team == "UTA"] - jazz$oreb[jazz$team != "UTA"], 1))
  ftr = sprintf("%.1f", round(jazz$ftr[jazz$team == "UTA"] - jazz$ftr[jazz$team != "UTA"], 1))
  
  tweetText = paste("Stat breakdown for ", matchup,":\nOffense: ", o, " (",
                    o_tile, "%tile)\nDefense: ", d, " (", d_tile, "%tile)\n\n", 
                    "eFG% adv: ", ifelse(efg > 0, paste("+", efg, sep=""), efg), 
                    "\nTOV% adv: ", ifelse(tov > 0, paste("+", tov, sep=""), tov), 
                    "\nOREB% adv: ", ifelse(oreb > 0, paste("+", oreb, sep=""), oreb), 
                    "\nFTArate adv: ", ifelse(ftr > 0, paste("+", ftr, sep=""), ftr), 
                    sep ="")
}

