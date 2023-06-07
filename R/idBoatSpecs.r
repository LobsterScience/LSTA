#' @export
idBoatSpecs <- function(x){
  id = x[,1]
  x = x[,-1]
  len = grep('length',names(x))
  wid = grep('width',names(x))
  ehp = grep('Horse',names(x))
  lwe = grep('Live',names(x))
  fue = grep('Fuel',names(x))
  mod = grep('Modif',names(x))

  #lengths
  x$dec = (x[,len]-floor(x[,len]))*100
  x$ft = floor(x[,len])
   i = which(x$dec<12)
   j = which(x$dec>12)

     x$dec[i,] = x$dec[i,]/12
     x$dec[j,] = x$dec[j,]/10/12
  x$lengthCorrected = x$ft+x$dec

  #widths
  x$dec = (x[,wid]-floor(x[,wid]))*100
  x$ft = floor(x[,wid])
  i = which(x$dec<12)
  j = which(x$dec>12)

  x$dec[i,] = x$dec[i,]/12
  x$dec[j,] = x$dec[j,]/10/12
  x$widthsCorrected = x$ft+x$dec

  x$horsePower = x[,ehp]*1
  i = grep('N',x$`22.d. Live Well`,ignore.case = T)
  x$livewell = NA
  x$livewell[i] = 0
  i = grep('Y',x$`22.d. Live Well`,ignore.case = F)
  x$livewell[i] = 1

  x$fuel = x[,fue]*1

  out = subset(x,select=c(lengthCorrected,widthsCorrected, horsePower,livewell,fuel))
  out$id = as.data.frame(id)

  oo = data.frame(id=out$id[,1],lengthCorrected = out$lengthCorrected[,1], width = out$widthsCorrected[,1],  horsePower = out$horsePower[,1],livewell=out$livewell,fuel=out$fuel[,1])
  return(oo)
}
