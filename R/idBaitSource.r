#' @export
idBaitSource <- function(x){
  id = x[,1]
  x = x[,-1]
  w = names(x)
  w = substr(w,63,100)
  out = data.frame(InvCode=id,BaitSource=NA,NSources= NA)
  for(i in 1:nrow(x)){
    if(all(is.na(x[i,]))) next
    n = w[which(x[i,]>0)]
    nl = length(n)
    n = paste(n,collapse='|')
    out$NSources[i] = nl
    out$BaitSource[i] = n
  }

  return(out)
}
