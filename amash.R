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

# Read raw data
df <- read.csv('amashvotevsmoney.csv',header=T,sep="|",stringsAsFactors=F,strip.white=T)
bigset <- read.csv('amashbig.csv',header=T,stringsAsFactors=F)

# Clean up a bit
df$Amount    <- as.numeric(gsub("\\$|,","", df$Amount)) # make dollars numeric
names(df)[3] <- 'District' # shorten district name
df   <- df[df$Vote!='Not Voting',] # drop abstainers (want 2 categories for Wilcoxon rank-sum test)
incr <- 5000
maxd <- 200000
rng  <- seq(0,maxd,incr)

# Colorblind-safe red/blue hues for 
# Republican/Democrat lines
myRedBlue        <- c("#ef8a62", "#67a9cf")
parties          <- c('R','D') # in this order
names(myRedBlue) <- parties

# Relative average prices: how much for a No for Democrats vs. Republicans, nationally
getGoingPrice <- function(vote) {
   big  <- df[df$Vote==vote,]
   r    <- big[big$Party=='R',]
   d    <- big[big$Party=='D',]
   m.r  <- c(mean(big$Amount),mean(r$Amount),mean(d$Amount))
   sd.r <- c(sd(big$Amount),sd(r$Amount),sd(d$Amount))
   tab  <- cbind(m.r,sd.r)
   rownames(tab) <- c("All","Republicans","Democrats")
   colnames(tab) <- c("Mean","SD")
   w    <- wilcox.test(d$Amount,r$Amount)
   c    <- list(tab,w)
   names(c) <- c("summary","wilcoxon.ranksum")
   return(c)
}

# How much for a No vs. Yes within a party?
getDifference <- function(party) {
   require("coin")
   d <- df[df$Party==party,]
   yes   <- d[d$Vote=='Yes',]
   no    <- d[d$Vote=='No',]
   yes.r <- c(nrow(yes),mean(yes$Amount),sd(yes$Amount))
   no.r  <- c(nrow(no),mean(no$Amount),sd(no$Amount))
   tab  <- rbind(yes.r,no.r)
   colnames(tab) <- c("Votes","Mean","St.dev")
   rownames(tab) <- paste(party,c("Yes","No"),sep=".")
   w.R     <- wilcox.test(d$Amount~d$Vote)
   w.Stata <- wilcox_test(d$Amount~as.factor(d$Vote))
   c       <- list(tab,w.R,w.Stata)
   names(c) <- c("summary","wilcoxon.R","wilcoxon.Stata")
   return(c)   
}

# Mean and SD by party, by vote, overall and for a state of interest
getAmount <- function(vote,party,state) {
   big <- df[df$Vote==vote & df$Party==party,]
   lil <- big[grep(state,big$District),]
   a <- c(mean(big$Amount, na.rm=T),median(big$Amount, na.rm=T),sd(big$Amount, na.rm=T),nrow(big))
   b <- c(mean(lil$Amount, na.rm=T),median(lil$Amount, na.rm=T),sd(lil$Amount, na.rm=T),nrow(lil))
   d <- rbind(a,b)
   rownames(d) <- c("US",state)
   colnames(d) <- c("Mean","Median","SD",paste(party,vote,sep="."))
   w <- wilcox.test(big$Amount,lil$amount)
   c <- list()
   c$list             <- lil
   c$wilcoxon.ranksum <- w
   c$summary          <- d
   return(c)
}

# Call them all
R.Difference <- getDifference('R')
D.Difference <- getDifference('D')
Yes  <- getGoingPrice('Yes')
No   <- getGoingPrice('No')
NC.D.Yes <- getAmount('Yes','D','NC')
NC.R.Yes <- getAmount('Yes','R','NC')
NC.D.No  <- getAmount('No','D','NC')
NC.R.No  <- getAmount('No','R','NC')

# Show it pretty
print(format(NC.R.No$summary,big.mark=",",digits=4),quote=F)
print(format(No$summary,big.mark=",",digits=4),quote=F)

# Now estimate prob of voting No as a function of Party and Amount.
# Use Zelig 5 with Zelig 4 compatibility wrappers.
df$Vote  <- factor(df$Vote, levels=c('Yes','No'))
df$Party <- factor(df$Party)

# Pure Zelig 5
z1 <- zlogit$new()
z1$zelig(Vote ~ Party + Amount, data=df)
z1$setrange(Party='R', Amount=rng)
z1$setrange1(Party='D', Amount=rng)
z1$sim()
plot.ci(z1, ci=95)

z2 <- zlogit$new()
z2$zelig(Vote ~ Party * Amount, data=df)
z2$setrange(Party='R', Amount=rng)
z2$setrange1(Party='D', Amount=rng)
z2$sim()
plot.ci(z2, ci=95)

m1 <- z1$sim.out
m2 <- z2$sim.out

#' Pick out data for drawing ggplot2 version of plot.ci().
#' Need a series of of median expected values bracketed by
#' the bottom and the top of a confidence interval, ci.
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
  
# Expected P(No) graph
drawEPic <- function(m,mname) {
   pic <- ggplot(data=m, aes(x=Amount, y=Median, group=Party)) + 
     geom_ribbon(aes(ymin=Low,ymax=High,fill=Party),alpha=.2) +
     geom_line(aes(color=Party)) + xlab('Funding from security and defense industry') + 
     ylab('Expected P(votes No)') + 
     ggtitle(paste('Expected probability of a No vote by party,',mname,sep=' '))
   pic <- pic + scale_colour_manual(values=myRedBlue)
   return(pic)
}

m1r   <- getRibbon(m1)
m2r   <- getRibbon(m2)
epic1 <- drawEPic(m1r,'Model 1')
epic2 <- drawEPic(m2r,'Model 2')

# Now add vertical bars to the plots where the
# amount is enough to cross the .5 probability
# threshold for each party.
m1pair <- (subset(m1r,Median>.5 & Median<.53) %>% group_by(Party) %>% slice(which.min(Median)))
m2pair <- (subset(m2r,Median>.5 & Median<.53) %>% group_by(Party) %>% slice(which.min(Median)))
pic1   <- epic1 + geom_segment(data=m1pair,
                               aes(x = Amount,
                                   y = Low,
                                   xend = Amount,
                                   yend = High),
                               color=myRedBlue,
                               size=2) +
                  geom_hline(aes(yintercept=.5),alpha=.2)
pic2   <- epic2 + geom_segment(data=m2pair,
                               aes(x = Amount,
                                   y = Low,
                                   xend = Amount,
                                   yend = High),
                               color=myRedBlue,
                               size=2) +
                  geom_hline(aes(yintercept=.5),alpha=.2)

