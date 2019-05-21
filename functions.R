

#quantile.bins will take binedges, a column that contains incomes in ascending order and bincounts, the number of people and provide a median income
#The top bin is changed to 'undefined' and the value of that column is irrelevant. 

optim.finder <- function(splb,binedges){
  # requires a continuous function to optimise, with the minimum at the median
  objfun <- function(x){
    (.5-splb$splineCDF(x))^2
  }
  # one dimensional optimisation to get point closest to .5 cdf
  out <- optimize(f=objfun, interval = range(binedges, na.rm=TRUE))
  return(out$minimum)
}

##see bottom for a little tutorial
median.approxfun<-function(x,probability){
  jane <- 0
  tim <- 0
  while(jane < probability){
    jane <- x(tim)
    tim <- tim + 1000
  }
  while(jane > probability){
    jane <- x(tim)
    tim <- tim - 100
  }
  while(jane < probability){
    jane <- x(tim)
    tim <- tim + 10
  }
  while(jane > probability){
    jane <- x(tim)
    tim <- tim - 1
  }
  while(jane < probability){
    jane <- x(tim)
    tim <- tim + .1
  }
  return(tim)}

quantile.bins<-function(binedges,bincounts,quantile=.5){
  splb <- splinebins(binedges, bincounts)
  output<-median.approxfun(splb$splineCDF,quantile)
  output}
