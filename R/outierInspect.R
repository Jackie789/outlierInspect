#' Outlier inspection produces 4 graphs
#
#' @param data The data that is being inspected
#'
#' @export
#' @examples
#' outlierInspect(data)
#'
#'
#'

outlierInspect <- function(data){
  data.cor <- cor(data)

  layout(matrix(c(1,1,1,1,1,1,1,1,2,2), 5, 2, byrow = TRUE))
  par(oma=c(5,7,1,1))

  cx <- rev(colorpanel(25,"blue","white","red"))
  leg <- seq(min(data.cor,na.rm=T),max(data.cor,na.rm=T),length=10)

  image(data.cor,main="Correlation plot:\n Lung cancer/No lung cancer data",axes=T,col=cx)
  axis(1,at=seq(0,1,length=ncol(data.cor)),label=dimnames(data.cor)[[2]],cex.axis=0.9,las=2)
  axis(2,at=seq(0,1,length=ncol(data.cor)),label=dimnames(data.cor)[[2]],cex.axis=0.9,las=2)

  image(as.matrix(leg),col=cx,axes=F)
  tmp <- round(leg,2)
  axis(1,at=seq(0,1,length=length(leg)),labels=tmp,cex.axis=1)


}

