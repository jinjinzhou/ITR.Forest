#' Value function used for initial treatment heterogeneity assessment.  This is used 
#' inside the tree growing function. 
#' 
#' @param dat dataset being assessed 
#' @param z new (alternative) treatment assignment in the splitting procedure.  
#' @param n0 minimum number of observations needed to make a split. 
#' @return itr value from a defined split.
#' @export


itrtest2 <- function(dat,z,n0){ 
  y <- dat$y         #outcome variable
  trt <- dat$trt     #treatment indicator
  prtx <- dat$prtx   #prob of being in treatment
  itr <- NA          #initialize value function 
  n <- nrow(dat)     #node sample size
  if (length(y)!=length(z)) stop("the vector z must have the same length as data.")
  if(n > n0) {
  #if node size is larger than specified cutoff, calculate itr value
    n.0 = length(y[trt==0])     #size of control node
    n.1 = n - n.0               #size of treatment node
    itr <- (1/n.1)*sum(z*y/prtx)+(1/n.0)*sum(((1-z)*y/(1-prtx)))   #value function
  } 
  itr
}