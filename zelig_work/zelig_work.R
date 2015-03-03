# Because shinyapps.io does not support Zelig, you
# need to do some of the work separately.
library("Zelig")

# Funding range
incr <- 5000
maxd <- 200000
rng  <- seq(0,maxd,incr)

# Options for confidence intervals
cilist        <- list(.8,.9,.95,.99)
names(cilist) <- paste(unlist(cilist)*100,'%')

# Handle data collection in separate script, because you
# might also need it for other things than Zelig work.
source('zelig_work/get_data.R')

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

#' Big wrapper
#' 
#' You set whether you want minimum or safe pay levels,
#' and at which % confidence safe. You get a picture
#' @param model
#' @param ci
#' @param vote string 'Yes' or 'No'
#' @param df which data frame to work on -- amash or hr37
runBigZelig <- function(model='Model 1',ci=.95,vote='No',df=amash) {
  # which model: common slope vs. interaction
  fml <<- paste("Vote==\'",vote,"\' ~ Party + Amount",sep="")
  if(model=='Model 2') {
    fml <<- paste("Vote==\'",vote,"\' ~ Party * Amount",sep="")
  }
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
}  

# Now function calls for Amash
datasets <- list()
datasets[['Model 1']] <- list()
datasets[['Model 2']] <- list()
for(j in c('Model 1','Model 2')) {
  for(i in cilist) {
    datasets[[j]][[paste(i*100,'%')]] <- runBigZelig(model=j,ci=i)
  }
}
save(datasets,file='shiny/amashsims.RData')

# Now function calls for HR37
datasets <- list()
datasets[['Yes']] <- list()
datasets[['No']] <- list()
datasets[['Yes']][['Model 1']] <- list()
datasets[['Yes']][['Model 2']] <- list()
datasets[['No']][['Model 1']] <- list()
datasets[['No']][['Model 2']] <- list()
for(k in c('Yes','No')) {
  for(j in c('Model 1','Model 2')) {
    for(i in cilist) {
      datasets[[k]][[j]][[paste(i*100,'%')]] <- runBigZelig(model=j,ci=i,
                                                            df=hr37,vote=k)
    }
  }
}
save(datasets,file='hr37shiny/hr37sims.RData')
