## @knitr get_data
myurls <- NULL
myurls['amash'] <- "http://maplight.org/us-congress/bill/113-hr-2397/1742215/contributions-by-vote?sort=asc&order=$%20From%20Interest%20Groups%3Cbr%20/%3EThat%20Opposed&party%5BD%5D=D&party%5BR%5D=R&party%5BI%5D=I&vote%5BAYE%5D=AYE&vote%5BNOE%5D=NOE&vote%5BNV%5D=NV&voted_with%5Bwith%5D=with&voted_with%5Bnot-with%5D=not-with&state=&custom_from=01/01/2011&custom_to=12/31/2012&all_pols=1&uid=44999&interests-support=&interests-oppose=D2000-D3000-D5000-D9000-D4000-D0000-D6000&from=01-01-2011&to=12-31-2012&source=pacs-nonpacs&campaign=congressional"
myurls['hr37']  <- "http://maplight.org/us-congress/bill/114-hr-37/6586030/contributions-by-vote"

# This is annoying: different URL's have the
# table of interest at different html nodes
mynodes        <- c(6,7)
names(mynodes) <- names(myurls)

#' Get raw data from maplight.org
#' Depends on package rvest
#' @param myurl URL to scrape from
#' @param node which html node
getMapLight <- function(myurl,node=6) {
  x <- html(myurl)
  df <- x %>% html_nodes('table') %>% .[[node]] %>% html_table()
  yes <- names(df)[grep('Support',names(df))]
  no  <- names(df)[grep('Oppose',names(df))]
  df[['AmountYes']] <- as.numeric(gsub("[$,]","", df[[yes]]))
  df[['AmountNo']]  <- as.numeric(gsub("[$,]","", df[[no]]))
  names(df)[names(df)=="State"] <- "District"
  df <- df[,!(names(df) %in% c(yes,no))]
  # drop abstainers (want 2 categories for Wilcoxon rank-sum test)
  df       <- df[df$Vote!='Not Voting',]
  df$Vote  <- factor(df$Vote, levels=c('Yes','No'))
  df$Party <- factor(df$Party)
  return(df)
}

# Don't scrape if you don't have to: bring in data
# for the Amash amendment and for HR 37.
for(i in names(myurls)) {
  outfile <- paste(i,'RData',sep='.')
  if(!file.exists(outfile)) {
    df <- getMapLight(myurl=myurls[i],node=mynodes[i])
    save(df,file=outfile)
  } else {
    load(outfile)
    assign(i,df)
    rm(df)
  } 
}

