#' @export
idSpeciesFished <- function(x){
  id = x[,1]
  x = x[,-1]
  w = names(x)
  wnm = grep('Name',w)
  wpc = grep('Inc',w)
  x[,wnm] = apply(x[,wnm],2,decodeSpeciesFished)
  out = data.frame(InvCode=id,Species=NA)
  for(i in 1:nrow(x)){
    if(all(is.na(x[i,]))) next
    o = c()
    for(j in 1:length(wnm)){
      if(all(is.na(x[i,wnm[j]]))) next
      xi = x[i,c(wnm[j],wpc[j])]
      o = c(o, paste(xi,collapse=','))
    }
    oo = paste(o,collapse="|")
    out$Species[i] = oo
  }

  return(out)
}
