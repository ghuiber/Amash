# Amash votes
# Original story: http://www.wired.com/threatlevel/2013/07/money-nsa-vote/
# Summary data: http://s3.documentcloud.org/documents/741074/amash-amendment-vote-maplight.pdf
# -- amashvotevsmoney.csv
# Complete data : http://bit.ly/11IhEtY
# -- amashbig.csv
rm(list=ls(all=TRUE))
library("Zelig")

data(turnout)
z5 <- zlogit$new()
z5$zelig(vote ~ age + educate, data = turnout)
z5$setx(educate=12)
z5$setx1(educate=16)
z5$setrange(age=seq(18,70,2))
z5$sim()
s5 <- sim(obj=z5,x=setx(z5,educate=12,age=seq(18,70,2)),x1=setx(z5,educate=16,age=seq(18,70,2)))

plot.ci(z5)
plot.ci(s5)

# Read raw data
df <- read.csv('amashvotevsmoney.csv',header=T,sep="|",stringsAsFactors=F,strip.white=T)

# Clean up a bit
df$Amount    <- as.numeric(gsub("\\$|,","", df$Amount)) # make dollars numeric
names(df)[3] <- 'District' # shorten district name
df   <- df[df$Vote!='Not Voting',] # drop abstainers (want 2 categories for Wilcoxon rank-sum test)
# Range of contributions and increment, in dollars
incr <- 5000
maxd <- 200000
df$Vote  <- factor(df$Vote, levels=c('Yes','No'))
df$Party <- factor(df$Party)

# Now estimate prob of voting No as a function of Party and Amount.
# Use Zelig 5 with Zelig 4 compatibility wrappers.
# Two models: 
# -- z1: party and amount
# -- z2: party, amount, and interaction between the two.

# Pure Zelig 5
z1 <- zlogit$new()
z1$zelig(Vote ~ Party + Amount, data=df)
z1$setx(Party='R')
z1$setx1(Party='D')
z1$setrange(Amount=seq(0,maxd,incr))
z1$setrange1(Amount=seq(0,maxd,incr))


z2 <- zlogit$new()
z2$zelig(Vote ~ Party * Amount, data=df)
z2$setx(Party='R')
z2$setx1(Party='D')
z2$setrange(Amount=seq(0,maxd,incr))
z2$setrange1(Amount=seq(0,maxd,incr))

# Zelig 5 simulation
z1$sim()
z2$sim()
# Zelig 4 compatibility wrapper
s1 <- sim(obj=z1,x=setx(z1,Party='R',Amount=seq(0,maxd,incr)),x1=setx(z1,Party='D',Amount=seq(0,maxd,incr)))
s2 <- sim(obj=z2,x=setx(z2,Party='R',Amount=seq(0,maxd,incr)),x1=setx(z2,Party='D',Amount=seq(0,maxd,incr)))

# These look right:
plot.ci(s1)
plot.ci(s2)

# These do not:
plot.ci(z1)
plot.ci(z2)
