#' Plots the variable importance measures. Requires an input of a variable importance object
#' created from the function Variable.Importance.ITR.
#' 
#' @param VI variable importance object from Variable.Importance.ITR. Required input. 
#' @param filename name of destination file. Defaults to NULL.
#' @param horizontal barplot horizontal? Defaults to TRUE.
#' @param rain.bow controls color scheme for output graphic. Defaults to TRUE.  
#' @return barplot of variable importance measures
#' @export


plot.VI <- function(VI, filename=NULL, horizontal=T, rain.bow=T)
{
  library(RColorBrewer)
  if (!is.null(filename)) postscript(file=filename, horizontal=horizontal)
  par(mfrow=c(1, 1), mar = c(7, 4, 7, 4));
  require(grDevices)
  p <- length(VI)
  color0 <- gray(0:(p - 1)/(p - 1))
  if (rain.bow) color0 <- brewer.pal(p, "YlOrRd")
  barplot(VI, col=color0, names.arg = names(VI), ylab="Importance (Proportion)", xlab="Variable", 
          cex.names = 1.2,  las=3);  # TO HAVE VERTICAL AXIS LABELING 
  text(x=Variable, y=VI, labels=VI, pos=3, xpd=NA)
  title(main = list("Variable Importance Rank",  font = 4, cex = 1.4));
  if (!is.null(filename)) dev.off()
}