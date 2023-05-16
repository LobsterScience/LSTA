#' @export
idLFA <- function(x, primLFA=c('LFA 29','LFA 36','LFA 30', 'LFA 38', 'LFA 28')){
  id = x[,1]
  x = x[,-1]
  w = names(x)
  w = substr(w,54,60)
  out = data.frame(InvCode=id,LFA=NA,PrimaryLFA= NA)
  for(i in 1:nrow(x)){
    if(all(is.na(x[i,]))) next
    n = w[which(x[i,]>0)]
    n = paste(n,collapse='|')
    out$PrimaryLFA[i] = primLFA[grep(n,primLFA)]
    out[i,2] = n
  }

  return(out)
}
