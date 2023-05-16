#' @export
idTechnology <- function(x){
  id = x[,1]
  x = x[,-1]
  w = names(x)
  w = substr(w,47,100)
  out = data.frame(InvCode=id,Tech=NA,NTechs= NA)
  for(i in 1:nrow(x)){
    if(all(is.na(x[i,]))) next
    n = w[which(x[i,]>0)]
    nl = length(n)
    n = paste(n,collapse='|')
    out$NTechs[i] = nl
    out$Tech[i] = n
  }

  return(out)
}
