# Amash votes
# Original story: http://www.wired.com/threatlevel/2013/07/money-nsa-vote/
# Summary data: http://s3.documentcloud.org/documents/741074/amash-amendment-vote-maplight.pdf
# -- amashvotevsmoney.csv
# Complete data : http://bit.ly/11IhEtY
# -- amashbig.csv
rm(list=ls(all=TRUE))
library(coin)
library(Zelig)
library(ggplot2)

# Read raw data
df <- read.csv('amashvotevsmoney.csv',header=T,sep="|",stringsAsFactors=F,strip.white=T)
bigset <- read.csv('amashbig.csv',header=T,stringsAsFactors=F)

# Clean up a bit
df$Amount    <- as.numeric(gsub("\\$|,","", df$Amount)) # make dollars numeric
names(df)[3] <- 'District' # shorten district name
df   <- df[df$Vote!='Not Voting',] # drop abstainers (want 2 categories for Wilcoxon rank-sum test)
incr <- 5000
maxd <- 200000

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
   require(coin)
   d <- df[df$Party==party,]
   yes   <- d[d$Vote=='Yes',]
   no    <- d[d$Vote=='No',]
   yes.r <- c(dim(yes)[1],mean(yes$Amount),sd(yes$Amount))
   no.r  <- c(dim(no)[1],mean(no$Amount),sd(no$Amount))
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
   a <- c(mean(big$Amount, na.rm=T),median(big$Amount, na.rm=T),sd(big$Amount, na.rm=T),dim(big)[1])
   b <- c(mean(lil$Amount, na.rm=T),median(lil$Amount, na.rm=T),sd(lil$Amount, na.rm=T),dim(lil)[1])
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

# Now estimate prob of voting No as a function of Party and Amount. Use Zelig.
df$Vote  <- factor(df$Vote, levels=c('Yes','No'))
df$Party <- factor(df$Party)
z1.out <- zelig(Vote ~ Party + Amount, data=df, model="logit", cite=FALSE)
z2.out <- zelig(Vote ~ Party + Amount + Party*Amount, data=df, model="logit", cite=FALSE)
getSims <- function(z.out) {
   x.R <- setx(z.out,Party='R',Amount=seq(0,maxd,incr))
   x.D <- setx(z.out,Party='D',Amount=seq(0,maxd,incr))
   s.out <- sim(z.out,x=x.D,x1=x.R)
   s.D <- sim(z.out,x=x.D,x1=x.R)
   s.R <- sim(z.out,x=x.R,x1=x.D)
   c <- list(x.R,x.D,s.out,s.D,s.R)
   names(c) <- c('x.R','x.D','s.out','s.D','s.R')
   return(c)
}   
m1 <- getSims(z1.out)
m2 <- getSims(z2.out)

# Collect predicted values from a Zelig sim() object
collectPredictedValues <- function(zs) {
   rnames    <- names(summary(zs)[['stats']])
   expected  <- NULL
   predicted <- NULL
   for(i in rnames) {
      expected  <- rbind(expected,summary(zs)[['stats']][[i]][[1]])
      predicted <- rbind(predicted,summary(zs)[['stats']][[i]][[2]])
   }
   c <- cbind(expected,predicted)
   rownames(c) <- rnames
   return(c)
}
getPV <- function(m) {
   p.D <- collectPredictedValues(m[['s.D']])
   p.R <- collectPredictedValues(m[['s.R']])
   p.No <- as.data.frame(rbind(p.R[,c(6,7)],p.D[,c(6,7)]))
   p.No$group  <- rownames(p.No)
   names(p.No) <- c('Yes','No','Group')
   p.No$Amount <- as.numeric(gsub("^P.*=","",p.No$Group,perl=T))
   p.No$Party  <- gsub("(Party=|,.*)","",p.No$Group,perl=T)
   p.No <- p.No[,c('Party','Amount','Yes','No')]   
   return(p.No)
}
getEV <- function(m) {
   p.D <- collectPredictedValues(m[['s.D']])
   p.R <- collectPredictedValues(m[['s.R']])
   p.No <- as.data.frame(rbind(p.R[,c(1,2)],p.D[,c(1,2)]))
   p.No$group  <- rownames(p.No)
   names(p.No) <- c('Mean','SD','Group')
   p.No$Amount <- as.numeric(gsub("^P.*=","",p.No$Group,perl=T))
   p.No$Party  <- gsub("(Party=|,.*)","",p.No$Group,perl=T)
   p.No <- p.No[,c('Party','Amount','Mean','SD')]   
   return(p.No)
}
# Predicted P(No) graph
drawPPic <- function(m,mname) {
   p.No <- getPV(m)
   pic <- ggplot(data=p.No, aes(x=Amount, y=No, group=Party, color=Party)) + geom_line() + xlab('Funding from security and defense industry') + ylab('Predicted P(votes No)') + ggtitle(paste('Predicted probability of a No vote by party,',mname,sep=' '))
   pic <- pic + scale_colour_manual(values=c("blue", "red"))    
   return(pic)
}
# Expected P(No) graph
drawEPic <- function(m,mname) {
   p.No <- getEV(m)
   pic <- ggplot(data=p.No, aes(x=Amount, y=Mean, group=Party, color=Party)) + geom_line() + xlab('Funding from security and defense industry') + ylab('Expected P(votes No)') + ggtitle(paste('Expected probability of a No vote by party,',mname,sep=' '))
   pic <- pic + scale_colour_manual(values=c("blue", "red"))   
   return(pic)
}
ppic1 <- drawPPic(m1,'Model 1')
ppic2 <- drawPPic(m2,'Model 2')
epic1 <- drawEPic(m1,'Model 1')
epic2 <- drawEPic(m2,'Model 2')
p1.No <- getPV(m1)
p2.No <- getPV(m2)

# Now try and draw somehow vertical confidence 
# intervals. plot.ci() in latest Zelig bugged.
# Step 1. Get the 95CI for a No by amount, by 
# party, for a given model.
getCIData <- function(m) {
   p.D <- collectPredictedValues(m[['s.D']])
   p.R <- collectPredictedValues(m[['s.R']])
   p.No <- as.data.frame(rbind(p.R[,c(4,5)],p.D[,c(4,5)]))
   p.No$group  <- rownames(p.No)
   names(p.No) <- c('Lower','Upper','Group')
   p.No$Amount <- as.numeric(gsub("^P.*=","",p.No$Group,perl=T))
   p.No$Party  <- gsub("(Party=|,.*)","",p.No$Group,perl=T)
   p.No <- p.No[,c('Party','Amount','Lower','Upper')]   
   return(p.No)
}
# Then put it into long format (make it tidy for ggplot2 to handle)
makeItLong <- function(p) {
   p.long <- reshape(p, direction="long", varying=c('Lower','Upper'), v.names="Value", idvar=c("Party","Amount"), timevar="CI", times=c('Lower','Upper'))
   p.long.sorted <- p.long[order(p.long$Party,p.long$Amount,p.long$CI),]
   return(as.data.frame(p.long.sorted))
}
ci.m1 <- makeItLong(getCIData(m1))
ci.m2 <- makeItLong(getCIData(m2))
getCIplot <- function(df,mname) {
   foo <- df
   foo$Grouping <- df$Amount+as.numeric(as.factor(df$Party))
   p <- ggplot(data=foo, aes(x=Amount, y=Value, group=Amount, colour=Party)) + geom_point() + geom_line(aes(group = Grouping))   
   p <- p + xlab('Funding from security and defense industry') + ylab('Expected P(votes No)') + ggtitle(paste('95% confidence intervals for the expected \n probabilities of a No vote by party,',mname,sep=' '))
   p <- p + scale_colour_manual(values=c("blue", "red"))
   return(p)
}
ci.plot.m1 <- getCIplot(ci.m1,'Model 1')
ci.plot.m2 <- getCIplot(ci.m2,'Model 2')
