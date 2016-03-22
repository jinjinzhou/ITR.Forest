#' Calcuates variable importance measures for a random forest object.  Input must be an object
#'  from the random forest function Build.RF.ITR. 
#' 
#' @param RF.fit forest object from Build.RF.ITR. Required input. 
#' @param n0 minimum number of treatment/control observations needed in a split to call a node terminal. Defaults to 2. 
#' @param sort sort the variable importance measure? Defaults to TRUE. 
#' @param n0 minimum number of treatment/control observations needed in a split to call a node terminal. Defaults to 5. 
#' @param details print details. Defaults to FALSE.
#' @param truncate.zeros sets variable importances less than 0 to 0. Defaults to TRUE.
#' @return summary of tree performance
#' @export


Variable.Importance.ITR <- function(RF.fit, n0=2, sort=T, details=F, truncate.zeros=T,depth=1){
  trees <- RF.fit$TREES
  id.boots <- RF.fit$ID.Boots.Samples
  # Extract necessary variables from the forest object
  Model.Specification <- RF.fit$Model.Specification
  dat0 <- Model.Specification$data
  col.y <- Model.Specification$col.y
  col.trt <- Model.Specification$col.trt
  col.prtx <- Model.Specification$col.prtx
  split.var <- Model.Specification$split.var
  ctg <- Model.Specification$ctg
  vnames <- colnames(dat0)[split.var]
  ntree <- length(trees)
  p <- length(split.var)
  VI <- rep(0, p)
  # Consider each tree in the forest separately
  for (b in 1:ntree){
    id.b <- id.boots[[b]]
    #OOB is the out of bag sample (i.e. the complement of the bootstrap sample taken in the RF tree generation)
    dat.oob <- dat0[-sort(unique(id.b)), ] 
    n.oob <- nrow(dat.oob)	
    tre.b <- trees[[b]]
    ########## NOTE THAT revise.tree=T HERE! ##########
    out0.b <- send.down.VI.ITR(dat.new=dat.oob, tre=tre.b, col.y=col.y, col.trt=col.trt, col.prtx=col.prtx, ctg=ctg, n0=n0, revise.tree=T,depth=1)  
    tre0.b <- out0.b$tre0	
    #Here we consider only trees that obtained at least one split
    if (nrow(tre0.b) > 1) {
      Xs.b <- sort(unique(na.omit(tre0.b$var))) 
      G.oob <- out0.b$score
      #Consider each of the splitting covariates
      for (j in 1:p) {
        if (details) print(j)     #Print details?
        G.j <- G.oob              #OOB score after being run down tree
        col.xj <- split.var[j]    #which split variable?
        if (is.element(col.xj, Xs.b)){			
          x.j <- dat.oob[, col.xj]
          dat.permuted <- dat.oob
          dat.permuted[ , col.xj] <- x.j[sample(1:n.oob,n.oob, replace=F)]
          ########## NOTE THAT revise.tree=F HERE! ##########
          #Send the permuted OOB sample down the tree
          out0.bj <- send.down.VI.ITR(dat.new=dat.permuted, tre=tre0.b, col.y=col.y, col.trt=col.trt, col.prtx=col.prtx, ctg=ctg, n0=n0, revise.tree=F,depth=1)
          tre0.bj <- out0.bj$tre0		
          #If there is one row in the tree (only parent node) then OOB score is the score from the parent node
          #Otherwise store the score for the variable from the send down function
          G.j <- ifelse(nrow(tre0.bj) ==1, G.oob, out0.bj$score)
        }
        if (G.j > G.oob) G.j <- G.oob  		
        ##################### PREVENTS NEGATIVE IMPORTANCE VALUES 
        # Variable importance is defined as the sum of the importance for the variable across all trees
        VI[j] <- VI[j] + (G.oob - G.j)/G.oob
      }
    }	
  }
  if (truncate.zeros) VI[VI <0] <- 0  		# this should not be necessary since all VIs are >0  
  names(VI) <- vnames
  if (sort) VI <- sort(VI, decreasing=T) 
  VI<-VI/sum(VI)
  return(VI)
}