# Amash votes
# Original story: http://www.wired.com/threatlevel/2013/07/money-nsa-vote/
# Summary data: http://s3.documentcloud.org/documents/741074/amash-amendment-vote-maplight.pdf
# -- amashvotevsmoney.csv
# Complete data : http://bit.ly/11IhEtY
# -- amashbig.csv
library("coin")
library("ggplot2")
library("dplyr")

# Colorblind-safe red/blue hues for 
# Republican/Democrat lines
myRedBlue        <- c("#ef8a62", "#67a9cf")
parties          <- c('R','D') # in this order
names(myRedBlue) <- parties
# produced with ../zelig_work/zelig_work.R
load('hr37sims.RData')

# Now estimate prob of voting No as a function of Party and Amount.

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
drawEPic <- function(m,mname,p='min',vote='Yes') {
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
#' @param x
#' @param ci
#' @param p
#' @param vote string 'Yes' or 'No'
getBigPicture <- function(model='Model 1',ci=.95,p='min',vote='Yes') {
  # which model: common slope vs. interaction
  mdl <- 'Model 1: baseline difference by party, but same response to funding'
  if(model=='Model 2') {
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
  # collect ribbon 
  mr <- datasets[[vote]][[model]][[paste(ci*100,'%')]]
  # draw picture
  pic <- drawEPic(mr,mname=ma,p=p,vote=vote)
  return(pic)
}
