# Amash votes
# Original story: http://www.wired.com/threatlevel/2013/07/money-nsa-vote/
# Summary data: http://s3.documentcloud.org/documents/741074/amash-amendment-vote-maplight.pdf
# -- amashvotevsmoney.csv
# Complete data : http://bit.ly/11IhEtY
# -- amashbig.csv
rm(list=ls(all=TRUE))
library("coin")
library("Zelig")
library("ggplot2")
library("dplyr")
library("rvest")

# # Read raw data
# df <- read.csv('amashvotevsmoney.csv',header=T,sep="|",stringsAsFactors=F,strip.white=T)
# bigset <- read.csv('amashbig.csv',header=T,stringsAsFactors=F)
# 
# # Clean up a bit
# df$Amount    <- as.numeric(gsub("\\$|,","", df$Amount)) # make dollars numeric
# names(df)[3] <- 'District' # shorten district name
# df       <- df[df$Vote!='Not Voting',] # drop abstainers (want 2 categories for Wilcoxon rank-sum test)
# df$Vote  <- factor(df$Vote, levels=c('Yes','No'))
# df$Party <- factor(df$Party)

# # Relative average prices: how much for a No for Democrats vs. Republicans, nationally
# getGoingPrice <- function(vote) {
#    big  <- df[df$Vote==vote,]
#    r    <- big[big$Party=='R',]
#    d    <- big[big$Party=='D',]
#    m.r  <- c(mean(big$Amount),mean(r$Amount),mean(d$Amount))
#    sd.r <- c(sd(big$Amount),sd(r$Amount),sd(d$Amount))
#    tab  <- cbind(m.r,sd.r)
#    rownames(tab) <- c("All","Republicans","Democrats")
#    colnames(tab) <- c("Mean","SD")
#    w    <- wilcox.test(d$Amount,r$Amount)
#    c    <- list(tab,w)
#    names(c) <- c("summary","wilcoxon.ranksum")
#    return(c)
# }
# 
# # How much for a No vs. Yes within a party?
# getDifference <- function(party) {
#    require("coin")
#    d <- df[df$Party==party,]
#    yes   <- d[d$Vote=='Yes',]
#    no    <- d[d$Vote=='No',]
#    yes.r <- c(nrow(yes),mean(yes$Amount),sd(yes$Amount))
#    no.r  <- c(nrow(no),mean(no$Amount),sd(no$Amount))
#    tab  <- rbind(yes.r,no.r)
#    colnames(tab) <- c("Votes","Mean","St.dev")
#    rownames(tab) <- paste(party,c("Yes","No"),sep=".")
#    w.R     <- wilcox.test(d$Amount~d$Vote)
#    w.Stata <- wilcox_test(d$Amount~as.factor(d$Vote))
#    c       <- list(tab,w.R,w.Stata)
#    names(c) <- c("summary","wilcoxon.R","wilcoxon.Stata")
#    return(c)   
# }
# 
# # Mean and SD by party, by vote, overall and for a state of interest
# getAmount <- function(vote,party,state) {
#    big <- df[df$Vote==vote & df$Party==party,]
#    lil <- big[grep(state,big$District),]
#    a <- c(mean(big$Amount, na.rm=T),median(big$Amount, na.rm=T),sd(big$Amount, na.rm=T),nrow(big))
#    b <- c(mean(lil$Amount, na.rm=T),median(lil$Amount, na.rm=T),sd(lil$Amount, na.rm=T),nrow(lil))
#    d <- rbind(a,b)
#    rownames(d) <- c("US",state)
#    colnames(d) <- c("Mean","Median","SD",paste(party,vote,sep="."))
#    w <- wilcox.test(big$Amount,lil$amount)
#    c <- list()
#    c$list             <- lil
#    c$wilcoxon.ranksum <- w
#    c$summary          <- d
#    return(c)
# }
# 
# # Call them all
# R.Difference <- getDifference('R')
# D.Difference <- getDifference('D')
# Yes  <- getGoingPrice('Yes')
# No   <- getGoingPrice('No')
# NC.D.Yes <- getAmount('Yes','D','NC')
# NC.R.Yes <- getAmount('Yes','R','NC')
# NC.D.No  <- getAmount('No','D','NC')
# NC.R.No  <- getAmount('No','R','NC')
# 
# # Show it pretty
# print(format(NC.R.No$summary,big.mark=",",digits=4),quote=F)
# print(format(No$summary,big.mark=",",digits=4),quote=F)

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

# Don't scrape if you don't have to
scrapethis <- "http://maplight.org/us-congress/bill/113-hr-2397/1742215/contributions-by-vote?sort=asc&order=$%20From%20Interest%20Groups%3Cbr%20/%3EThat%20Opposed&party%5BD%5D=D&party%5BR%5D=R&party%5BI%5D=I&vote%5BAYE%5D=AYE&vote%5BNOE%5D=NOE&vote%5BNV%5D=NV&voted_with%5Bwith%5D=with&voted_with%5Bnot-with%5D=not-with&state=&custom_from=01/01/2011&custom_to=12/31/2012&all_pols=1&uid=44999&interests-support=&interests-oppose=D2000-D3000-D5000-D9000-D4000-D0000-D6000&from=01-01-2011&to=12-31-2012&source=pacs-nonpacs&campaign=congressional"
if(!file.exists('amash.RData')) {
  df <- getMapLight(myurl=scrapethis)
  save(df,file='amash.RData')
} else {
  load('amash.RData')
}

# Funding range
incr <- 5000
maxd <- 200000
rng  <- seq(0,maxd,incr)

# Colorblind-safe red/blue hues for 
# Republican/Democrat lines
myRedBlue        <- c("#ef8a62", "#67a9cf")
parties          <- c('R','D') # in this order
names(myRedBlue) <- parties

# Now estimate prob of voting No as a function of Party and Amount.

#' Pick out data for drawing ggplot2 version of plot.ci().
#' 
#' Need a series of of median expected values bracketed by
#' the bottom and the top of a confidence interval, ci.
#' 
#' @param x a zlogit$sim.out object with range and range1
#' @param ci width of the confidence interval
getRibbon <- function(x,ci=.95) {
  ran       <- x$range  
  ran1      <- x$range1
  n         <- length(rng)  
  # get bottom 2.5%th, top 97.5%th, median
  getLoMeHi <- function(x) {
    ci  <- (1-ci)/2
    ve  <- sort(x$ev[[1]])
    lo  <- floor(length(ve)*ci)
    hi  <- ceiling(length(ve)*(1-ci))
    lo  <- ve[lo]
    hi  <- ve[hi]
    me  <- median(ve)
    return(c(lo,me,hi))
  }
  evmatrix  <- matrix(rep(NA,n*3),ncol=3)
  evmatrix1 <- evmatrix
  for(i in 1:n) {
    evmatrix[i,]  <- getLoMeHi(ran[[i]])
    evmatrix1[i,] <- getLoMeHi(ran1[[i]])
  }
  evmatrix  <- cbind(0L,rng,evmatrix)
  evmatrix1 <- cbind(1L,rng,evmatrix1)
  out <- rbind(evmatrix,evmatrix1)
  # Now make it useful for ggplot2
  colnames(out) <- c('Party','Amount','Low','Median','High')
  out <- as.data.frame(out)
  out$Party <- factor(out$Party,labels=parties)
  return(out)
}

#' Minimum and safe levels of pay
#' 
#' You need to pick a level of pay that clears
#' the .5 probability threshold either at the
#' mid-point of the range (minimum) or at the
#' bottom of the range. This is a "safe" level
#' of pay that promises a favorable vote with
#' a probability P()=ci, where default ci=95%.
#' The latter may not be attainable.
#' 
#' @param m  an object returned by getRibbon()
getPay <- function(m) {
  mycols  <- as.data.frame(myRedBlue)
  colnames(mycols) <- 'Color'
  mycols$Party <- rownames(mycols)
  minpay  <- (subset(m,Median>.5) %>% 
                group_by(Party) %>% 
                slice(which.min(Median)))
  minpay  <- merge(minpay,mycols)
  safepay <- (subset(m,Low>.5) %>% 
                group_by(Party) %>% 
                slice(which.min(Low)))
  safepay <- merge(safepay,mycols)
  out <- list()
  out[['min']]  <- minpay
  out[['safe']] <- safepay
  return(out)
}
  
#' Expected P(No) graph
#' 
#' Draws response curves over a given funding range
#' with vertical bars at minimum or safe levels of
#' pay that would produce a No vote with P(No)>.5
#' for each party.
#' 
#' @param m an object returned by getRibbon()
#' @param mname name of the model for prettier graph titles
#' @param p an element from c('min','safe')
#' @param vote an element from c('Yes','No')
drawEPic <- function(m,mname,p='min',vote='No') {
   pay <- getPay(m)[[p]] 
   xl  <- paste('Net funding from',vote,'interests',sep=' ')
   yl  <- paste('Expected P(votes ',vote,')',sep='')
   pic <- ggplot(data=m, aes(x=Amount, y=Median, group=Party)) + 
     geom_ribbon(aes(ymin=Low,ymax=High,fill=Party),alpha=.2) +
     geom_line(aes(color=Party)) + xlab(xl) + 
     ylab(yl) + ggtitle(mname)
   pic <- pic + scale_colour_manual(values=myRedBlue)
   pic <- pic + geom_segment(data=pay,
                             aes(x = Amount,
                                 y = Low,
                                 xend = Amount,
                                 yend = High),
                             color=pay$Color,
                             size=2) +
                 geom_hline(aes(yintercept=.5),alpha=.2) +
                 geom_text(data=pay, aes(x = Amount,
                                         y = .95*Low,
                                         label = paste('$',
                                                       round(Amount/1000),
                                                       'K',sep='')))
   return(pic)
}

#' Big wrapper
#' 
#' You set whether you want minimum or safe pay levels,
#' and at which % confidence safe. You get a picture
#' @param model string 'Model 1' or 'Model 2'
#' @param ci numeric btw 0 and 1
#' @param p string 'min' or 'safe'
#' @param vote string 'Yes' or 'No'
getBigPicture <- function(model='Model 1',ci=.95,p='min',vote='No') {
  # which model: common slope vs. interaction
  fml <<- paste("Vote==\'",vote,"\' ~ Party + Amount",sep="")
  mdl <- 'Model 1: baseline difference by party, but same response to funding'
  if(model=='Model 2') {
    fml <<- paste("Vote==\'",vote,"\' ~ Party * Amount",sep="")
    mdl <- 'Model 2: both baseline and response to funding are different by party'
  }
  mdl <- paste(mdl,'\n',sep='')
  # which kind of pay level
  pl <- 'minimum'
  if(p=='safe') {
    pl <- paste(ci*100,'% safe',sep='')
  }
  sb <- paste(mdl,'Funding level: ',pl,sep='')
  ma <- paste('Expected probability of a',vote,'vote by party\n',sb,sep=' ')
  # estimate model, simulate expected probability of a 
  # favorable vote as a function of net funding from
  # interests that want a Yes or a No outcome -- i.e.,
  # Amount = AmountNo - AmountYes for a No vote, 
  # or reverse the sign for a Yes vote
  df$Amount <- df$AmountNo - df$AmountYes
  if(vote=='Yes') df$Amount <- -df$Amount
  z5 <- zlogit$new()
  z5$zelig(formula=as.formula(fml), data=df)
  z5$setrange(Party='R', Amount=rng)
  z5$setrange1(Party='D', Amount=rng)
  z5$sim()
  # extract ribbon data
  mr <- getRibbon(z5$sim.out,ci=ci)
  # draw picture
  pic <- drawEPic(mr,mname=ma,p=p,vote=vote)
  return(pic)
}

m1.95.min <- getBigPicture()
m2.80.safe <- getBigPicture(model='Model 2',ci=.8,p='safe')

# Test code
# m1r   <- getRibbon(m1)
# m2r   <- getRibbon(m2)
# epic1 <- drawEPic(m1r,'Model 1')
# epic2 <- drawEPic(m2r,'Model 2')

# # Pure Zelig 5
# z1 <- zlogit$new()
# z1$zelig(Vote ~ Party + Amount, data=df)
# z1$setrange(Party='R', Amount=rng)
# z1$setrange1(Party='D', Amount=rng)
# z1$sim()
# # plot.ci(z1, ci=95)
# 
# z2 <- zlogit$new()
# z2$zelig(Vote ~ Party * Amount, data=df)
# z2$setrange(Party='R', Amount=rng)
# z2$setrange1(Party='D', Amount=rng)
# z2$sim()
# # plot.ci(z2, ci=95)
# 
# m1 <- z1$sim.out
# m2 <- z2$sim.out
