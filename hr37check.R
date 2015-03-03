# test the Amash recipe with a case where both sides got about the same money, 
# and which was not as ideologically clear-cut as Amash

library('rvest')
hr37   <- html('http://maplight.org/us-congress/bill/114-hr-37/6586030/contributions-by-vote')
hr37df <- hr37 %>% html_nodes('table') %>% .[[7]] %>% html_table()
hr37df$AmountYes <- as.numeric(gsub("\\$|,","", hr37df[['$ From Interest GroupsThat Support']]))
hr37df$AmountNo  <- as.numeric(gsub("\\$|,","", hr37df[['$ From Interest GroupsThat Oppose']]))
