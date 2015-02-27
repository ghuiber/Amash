# Amash votes
# Original story: http://www.wired.com/threatlevel/2013/07/money-nsa-vote/
# Summary data: http://s3.documentcloud.org/documents/741074/amash-amendment-vote-maplight.pdf
# -- amashvotevsmoney.csv
# Complete data : http://bit.ly/11IhEtY
# -- amashbig.csv
library("coin")
library("Zelig")
library("ggplot2")
library("dplyr")

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
drawEPic <- function(m,mname,p='min') {
   pay <- getPay(m)[[p]] 
   xl  <- 'Funding from security and defense interests'
   yl  <- 'Expected P(votes No)'
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
getBigPicture <- function(model='Model 1',ci=.95,p='min') {
  # which model: common slope vs. interaction
  fml <<- 'Vote ~ Party + Amount'
  mdl <- 'Model 1: baseline difference by party, but same response to funding'
  if(model=='Model 2') {
    fml <<- 'Vote ~ Party * Amount'
    mdl <- 'Model 2: both baseline and response to funding are different by party'
  }
  mdl <- paste(mdl,'\n',sep='')
  # which kind of pay level
  pl <- 'minimum'
  if(p=='safe') {
    pl <- paste(ci*100,'% safe',sep='')
  }
  sb <- paste(mdl,'Funding level: ',pl,sep='')
  ma <- paste('Expected probability of a No vote by party\n',sb,sep='')
  # estimate model, simulate expected probability
  z5 <- zlogit$new()
  z5$zelig(formula=as.formula(fml), data=df)
  z5$setrange(Party='R', Amount=rng)
  z5$setrange1(Party='D', Amount=rng)
  z5$sim()
  # extract ribbon data
  mr <- getRibbon(z5$sim.out,ci=ci)
  # draw picture
  pic <- drawEPic(mr,mname=ma,p=p)
  return(pic)
}

m1.95.min <- getBigPicture()
m2.80.safe <- getBigPicture(model='Model 2',ci=.8,p='safe')
