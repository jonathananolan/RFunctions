##see bottom for a little tutorial
optim.finder <- function(splb,binedges){
  # requires a continuous function to optimise, with the minimum at the median
  objfun <- function(x){
    (.5-splb$splineCDF(x))^2
  }
  # one dimensional optimisation to get point closest to .5 cdf
  out <- optimize(f=objfun, interval = range(binedges, na.rm=TRUE))
  return(out$minimum)
}

quantile.bins<-function (binedges,bincounts){
  splb   <- splinebins  (binedges,bincounts)
  output <- optim.finder(splb,binedges)
  output}