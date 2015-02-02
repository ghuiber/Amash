# Amash 3, cleaned up

## @knitr theSetup
library(ggplot2) # for pretty plots
library(Zelig)   # for simulations
library(pryr)    # for looking at Zelig objects
library(compare) # for comparing objects
# Read raw data
df <- read.csv('amashvotevsmoney.csv',header=T,sep="|",stringsAsFactors=F,strip.white=T)

# Clean up a bit
df$Amount <- as.numeric(gsub("\\$|,","", df$Amount)) # make dollars numeric
names(df)[3] <- 'District'                           # shorten district name
df <- df[df$Vote!='Not Voting',]                     # drop abstainers (I want 2 categories for Wilcoxon rank-sum test)

## @knitr theSimulations
incr <- 5000
maxd <- 200000

# Now estimate prob of voting No as a function of Party and Amount. Use Zelig.
df$Vote  <- factor(df$Vote, levels=c('Yes','No'))
df$Party <- factor(df$Party)
z1.out <- zelig(Vote ~ Party + Amount, data=df, model="logit", cite=FALSE)
z2.out <- zelig(Vote ~ Party + Amount + Party*Amount, data=df, model="logit", cite=FALSE)
getSims <- function(z.out) {
   x.R <- setx(z.out,Party='R',Amount=seq(0,maxd,incr))
   x.D <- setx(z.out,Party='D',Amount=seq(0,maxd,incr))
   s.out <- sim(z.out,x=x.D,x1=x.R)
   return(s.out)
}   
s1.out <- getSims(z1.out)
s2.out <- getSims(z2.out)
