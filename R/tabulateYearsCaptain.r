#' @export

tabulateYearsCaptain <- function(x, groupVariable='PrimaryLFA'){
  co=grep('captain',colnames(x))
  x[,co] = decodeYearsCaptain(x[,co])
  x5 = split(x,f=x[,groupVariable])
  out=list()

  par(mfrow=c(2,3),las=2)
  for(i in 1:length(x5)){
         m = unlist(x5[[i]][,co])
         l = nrow(x5[[i]])
    if(any(!is.na(m))){
    v = table(m)/l*100
    barplot(v,ylab='Percent of Respondants',xlab='Ages',              main=paste(unique(x5[[i]]$PrimaryLFA),'; n=',l,sep=" "))
    out[[i]] = v
    names(out)[[i]] = unique(x5[[i]]$PrimaryLFA)
      }
  }
  return(out)
  }
