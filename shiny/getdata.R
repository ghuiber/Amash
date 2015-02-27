# Read raw data
# Amash votes
# Original story: http://www.wired.com/threatlevel/2013/07/money-nsa-vote/
# Summary data: http://s3.documentcloud.org/documents/741074/amash-amendment-vote-maplight.pdf
# -- amashvotevsmoney.csv
# Complete data : http://bit.ly/11IhEtY
# -- amashbig.csv
df <- read.csv('amashvotevsmoney.csv',header=T,sep="|",stringsAsFactors=F,strip.white=T)

# Clean up a bit
df$Amount    <- as.numeric(gsub("\\$|,","", df$Amount)) # make dollars numeric
names(df)[3] <- 'District' # shorten district name
df       <- df[df$Vote!='Not Voting',] # drop abstainers (want 2 categories for Wilcoxon rank-sum test)
df$Vote  <- factor(df$Vote, levels=c('Yes','No'))
df$Party <- factor(df$Party)

incr <- 5000
maxd <- 200000
rng  <- seq(0,maxd,incr)

# Colorblind-safe red/blue hues for 
# Republican/Democrat lines
myRedBlue        <- c("#ef8a62", "#67a9cf")
parties          <- c('R','D') # in this order
names(myRedBlue) <- parties
