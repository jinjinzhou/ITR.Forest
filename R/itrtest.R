#' Value function used for initial treatment heterogeneity assessment.  This is used 
#' inside the tree growing function. 
#' 
#' @param dat dataset being assessed 
#' @param z new (alternative) treatment assignment in the splitting procedure.  
#' @param n0 minimum number of observations needed to make a split. 
#' @return itr value from a defined split.
#' @export



itrtest <- function(dat,z,n0){
  #remember here that z is the new treatment assignment based on the split
  y <- dat$y        #outcome variable
  trt <- dat$trt    #treatment variable
  prtx <- dat$prtx  #prob of being in treatment
  itr <- NA         #value function
  n <- nrow(dat)    #sample size
  if (length(y)!=length(z)) stop("the vector z must have the same length as data.")
  if(n > n0) {
  #calculate itr value if sample size in the node is larger than smallest specified
  #this is used only in the initial assessment of all patients being in treatment or control
    itr <- mean(trt*y*z/prtx+(1-trt)*y*(1-z)/(1-prtx))
  }
  itr
}