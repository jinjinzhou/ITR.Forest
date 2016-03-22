#' Generates partition summary based on itr value. Used inside of the grow.ITR function.
#' 
#' @param dat data set from which the partition is to be made.  Must contain outcome, binary 
#'  treatment indicator, columns of splitting covariates, and column of probability of being
#'  in treatment group.
#' @param split.var columns of potential spliting variables. Required input.
#' @param min.ndsz minimum number of observations required to call a node terminal. Defaults to 20.
#' @param ctg identifies the categorical input columns.  Defaults to NULL.  Not available yet. 
#' @param n0 minimum number of treatment/control observations needed in a split to call a node terminal. Defaults to 5. 
#' @param max.depth controls the maximum depth of the tree. Defaults to 15. 
#' @param mtry sets the number of randomly selected splitting variables to be included. Defaults to number of splitting variables.
#' @return partition information based on itr value 
#' @export




partition.ITR <- function(dat, test=NULL, name="0", min.ndsz=20, n0=5, split.var, ctg=NULL, max.depth=15, mtry=length(split.var))
{   
  # here we initialize splitting and node info variables
  call <- match.call()
  out <- match.call(expand = F)
  out$info <- NULL
  out$name.l <- NULL
  out$name.r <- NULL
  out$left <- NULL
  out$right <- NULL
  out$... <- NULL
  # label the binary tree by 1 (left) and 2 (right).
  name.l <- paste(name, 1, sep="")
  name.r <- paste(name, 2, sep="")
  # sample size
  n <- nrow(dat)
  # check whether testing data is provided
  if (!is.null(test)) {
    n.test <- nrow(test)
    score.test <- NA
  }
  # prepare for the first cut these variable were used to store final cut information
  var <- NA
  vname <- NA
  cut <- NA
  # inilize score statistics. 
  # at the initial stage, there is no subgroup. 
  # Inidividuals either assign to trt=1 (z=rep(1,dim(dat)[1]))  
  # or trt=0 (z=rep(0,dim(dat)[1])) depending on which one gives better utility.
  # This step uses the itrtest function 
  if(name==0){
    max.score <- max(itrtest(dat, z=rep(0,dim(dat)[1]), n0),itrtest(dat, z=rep(1,dim(dat)[1]), n0))
  }else{
    max.score <- itrtest(dat, z=dat$new.trt, n0)
  }
  
  # extract value from data
  trt <- dat$trt
  y <- dat$y
  vnames <- colnames(dat)
  # COMPUTE THE TREATMENT EFFECT IN CURRENT NODE
  trt.effect <- NA
  n.0 = length(y[trt==0])
  n.1 = n - n.0
  if (min(n.1, n.0) >0) {
    trt.effect <- mean(y[trt==1]) - mean(y[trt==0])
  }
  # CONTROL THE MAX TREE DEPTH
  # name is the currently tree label.
  # only when currently depth < max.depth and n > min terminal node proceed.
  depth <- nchar(name) 
  if (depth <= max.depth && n >= min.ndsz) {  #this is probably not necessary
    if (is.null(mtry)) {
      m.try = length(split.var)
    }else{
      m.try = mtry
    }
    # if this is not random forrest, program will loop over all covariates.
    #for functions gdataM and rdat, split.var size is 4 and m.try is 4
    for(i in sample(split.var, size=m.try, replace=F)) { 
      x <- dat[,i]
      v.name <- vnames[i]
      temp <- sort(unique(x))
      if(length(temp) > 1) {
        #this will set the possible cut values for the variable
        # check if variable is categorical first
        if (is.element(i,ctg)){
          zcut <- power.set(temp)
        #if not categorical, handle as continuous
        } else{
          zcut <- temp[-length(temp)]
        }
        # zcut are the values for all possible cut 
        for(j in zcut) {
          score <- NA
          if (is.element(i,ctg)){
            grp <- sign(is.element(x, j))
            cut1 <- paste(j, collapse=" ")
          } else  {
            # define left and right groups
            grp.l <- sign(x <= j)
            cut1.l <-  cbind("l",as.character(j))
            grp.r <- sign(x > j)
            cut1.r <- cbind("r",as.character(j))
          }
          # use itr rule to calcuate the value function value for each split
          n.1 <- sum(grp.l==1)
          n.0 <- n-n.1
          score.l <- itrtest2(dat, z=grp.l, n0)
          n.1 <- sum(grp.r==1)
          n.0 <- n-n.1
          score.r <- itrtest2(dat, z=grp.r, n0)
          # record the one with improved value score
          # the following runs through the possible scenarios
          if (!is.na(score.l) && !is.na(score.r)) {
            if(score.l>max.score & score.r>max.score){
              if(score.l>score.r) {
                max.score <- score.l
                var <- i
                vname <- v.name
                cut <- cut1.l
                best.cut<-j
              }else{
                max.score <- score.r
                var <- i
                vname <- v.name
                cut <- cut1.r
                best.cut<-j
              }
              
            }else if(score.l>max.score & score.r<max.score){
              max.score <- score.l
              var <- i
              vname <- v.name
              cut <- cut1.l
              best.cut<-j
            }else if(score.l<max.score & score.r>max.score){
              max.score <- score.r
              var <- i
              vname <- v.name
              cut <- cut1.r
              best.cut<-j
            }
          }
        }
      }
    }
  }
  # when testing data is provided, assess new treatment assignment 
  # using testing sample and the rule caluclated from training sample
  # var is the covariates calcualted before where spliting adopts. 
  # best.cut is the cutting point.
  # This section of the code will be used in the variable importance functions
  if (!is.null(test)) { 
    n.test <- nrow(test)
    score.test <- NA
    if (!is.na(var)) {
      if (is.element(var,ctg)) {
        grp.test <- sign(is.element(test[,var], best.cut))
      }
      else  {
        grp.test <- sign(test[,var] <= best.cut)
      }
      score.test <- irttest(test, z=grp.test, n0=(n0/2))
      if (!is.na(score.test)){
        out$name.l <- name.l
        out$name.r <- name.r
        out$left.test <- test[grp.test==1,  ]
        out$right.test <- test[grp.test==0,  ]
        if (is.element(var,ctg)) {
          out$left  <- dat[is.element(dat[,var], best.cut),]
          out$right <- dat[!is.element(dat[,var], best.cut), ]}
        else {
          out$left  <- dat[dat[,var]<= best.cut,]
          out$right <- dat[dat[,var]> best.cut, ]
        }
      } else {
        var <- NA
        vname <- NA
        cut <- NA
        max.score <- NA
      }
      # output results from both testing and training data.
      out$info <- data.frame(node=name, size = n, n.1=n.1, n.0=n.0, trt.effect=trt.effect,var = var, 
                             vname=vname, cut= cut, score=ifelse(max.score==-1e10, NA, max.score),score.test, size.test=n.test)
    } else {
      out$info <- data.frame(node=name, size = n, n.1=n.1, n.0=n.0, trt.effect=trt.effect, var = NA, 
                             vname=NA, cut.1= NA,cut.2=NA, score=NA,score.test=NA, size.test=n.test)
    }
  }	else {
    # if no testing data output results from training data only.
    if (!is.na(var)) {
      out$name.l <- name.l
      out$name.r <- name.r
      if (is.element(var,ctg)) {                                                                               
        out$left  <- dat[is.element(dat[,var], best.cut),]
        out$right <- dat[!is.element(dat[,var], best.cut), ]}
      else {
        if(cut[1]=='l'){
          out$left  <- cbind(dat[dat[,var]<= best.cut,],new.trt=rep(1,n=sum(dat[,var]<= best.cut)))
          out$right <- cbind(dat[dat[,var]> best.cut, ],new.trt=rep(0,n=sum(dat[,var]> best.cut)))
        }else{
          out$left  <- cbind(dat[dat[,var]<= best.cut,],new.trt=rep(0,n=sum(dat[,var]<= best.cut)))
          out$right <- cbind(dat[dat[,var]> best.cut, ],new.trt=rep(1,n=sum(dat[,var]> best.cut)))
        }  
      }
      out$info <- data.frame(node=name, size = n, n.1=n.1, n.0=n.0, trt.effect=trt.effect, var = var, 
                             vname=vname, cut= cut, score=ifelse(max.score==-1e10, NA, max.score))
    } else {
      out$info <- data.frame(node=name, size = n, n.1=n.1, n.0=n.0, trt.effect=trt.effect,var=NA, 
                             vname=NA, cut.1= NA,cut.2=NA, score=NA)
    }
  }
  out 
}