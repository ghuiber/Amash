# Because shinyapps.io does not support Zelig
library("Zelig")
source("getdata.R")
cilist        <- list(.8,.9,.95,.99)
names(cilist) <- paste(unlist(cilist)*100,'%')

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
#' @param x
#' @param ci
#' @param p
runBigZelig <- function(model='Model 1',ci=.95) {
  # which model: common slope vs. interaction
  fml <<- 'Vote ~ Party + Amount'
  if(model=='Model 2') {
    fml <<- 'Vote ~ Party * Amount'
  }
  # estimate model, simulate expected probability
  z5 <- zlogit$new()
  z5$zelig(formula=as.formula(fml), data=df)
  z5$setrange(Party='R', Amount=rng)
  z5$setrange1(Party='D', Amount=rng)
  z5$sim()
  # extract ribbon data
  mr <- getRibbon(z5$sim.out,ci=ci)
}  

# Now function calls
datasets <- list()
datasets[['Model 1']] <- list()
datasets[['Model 2']] <- list()
for(j in c('Model 1','Model 2')) {
  for(i in cilist) {
    datasets[[j]][[paste(i*100,'%')]] <- runBigZelig(model=j,ci=i)
  }
}
save(datasets,file='../shiny/mydata.RData')
