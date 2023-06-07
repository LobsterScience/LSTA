#' @export

tabulateBoatSpecs <- function(x,variableOfInterest=c('length','width','livewell','fuel'), groupVariable='PrimaryLFA'){
  le = grep(variableOfInterest,colnames(x))

  x5 = split(x,f=x[,groupVariable])
  out=list()
  par(mfrow=c(2,3),las=2)
  for(i in 1:length(x5)){

    m = unlist(x5[[i]][,le])
    l = nrow(x5[[i]])
    lr = nrow(subset(x5[[i]],!is.na(variableOfInterest)))
    if(variableOfInterest %in% c('length','width','fuel')){
      if(any(!is.na(m))){
        v = hist(m,breaks=seq(min(m,na.rm=T),max(m,na.rm=T),by=1),plot=F)
        out[[i]] = data.frame(            names(out)[[i]] = unique(x5[[i]]$PrimaryLFA)) ##not done
      }
    }
  }
  return(out)
}
