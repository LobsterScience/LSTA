#' @export
idGearConfig <- function(x){
  id = x[,1]
  x = x[,-1]
  w = names(x)[1:3]
  w = substr(w,72,100)
  out = data.frame(InvCode=id,GearConfig=NA,NGearConfigs= NA,NTrapsPerString=NA)
  for(i in 1:nrow(x)){
    if(all(is.na(x[i,]))) next
    n = w[which(x[i,]>0)]
    nl = length(n)
    n = paste(n,collapse='|')
    out$NGearConfigs[i] = nl
    out$GearConfig[i] = n
    out$NTrapsPerString[i] = x[i,4]
    }

  return(out)
}
